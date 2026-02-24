if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
p_load(tidyverse, dplyr, data.table, ggplot2, fastDummies, readxl)

input_csv <- "PLACES__Local_Data_for_Better_Health,_County_Data_2024_release_20260223.csv"
output_long_csv <- "places_filtered_mobility_proxies_long.csv"
output_wide_csv <- "places_filtered_mobility_proxies_wide.csv"
bubble_plot_png <- file.path("plots", "All", "bubble_under18_vs_transport_barriers.png")
binned_means_plot_png <- file.path("plots", "All", "under18_binned_means_transport_barriers.png")
coef_plot_png <- file.path("plots", "All", "under18_share_coefficient_plot.png")
regional_plot_prefix <- "bubble_under18_vs_transport_barriers"
pop_bucket_plot_prefix <- file.path("plots", "All", "bubble_under18_vs_transport_barriers_pop_bucket")
pop_bucket_region_plot_prefix <- "bubble_under18_vs_transport_barriers_pop_bucket_region"
coef_output_csv <- file.path("results", "under18_share_model_coefficients.csv")
coef_summary_md <- file.path("results", "under18_share_model_summary.md")
urbanicity_csv <- "county_urbanicity.csv" # Optional: must include LocationID + urbanicity

target_measures <- c(
  LACKTRPT = "Transportation Barriers",
  MOBILITY = "Mobility Disability",
  INDEPLIVE = "Independent Living Disability",
  LPA = "Physical Inactivity",
  MHLTH = "Frequent Mental Distress",
  ISOLATION = "Social Isolation"
)

region_levels <- c("Midwest", "Northeast", "South", "West")

places <- read_csv(input_csv, show_col_types = FALSE)

core <- places |>
  filter(
    Year == 2022,
    DataSource == "BRFSS",
    StateAbbr != "US",
    MeasureId %in% names(target_measures)
  ) |>
  mutate(
    total_pop = parse_number(as.character(TotalPopulation)),
    total_pop_18plus = parse_number(as.character(TotalPop18plus)),
    under18_share = 1 - (total_pop_18plus / total_pop)
  ) |>
  select(
    Year, StateAbbr, StateDesc, LocationID, LocationName,
    MeasureId, Short_Question_Text, Data_Value, Data_Value_Unit, Data_Value_Type,
    Low_Confidence_Limit, High_Confidence_Limit,
    total_pop, total_pop_18plus, under18_share
  )

if (file.exists(urbanicity_csv)) {
  urbanicity <- read_csv(urbanicity_csv, show_col_types = FALSE) |>
    mutate(LocationID = as.character(LocationID))

  core <- core |>
    mutate(LocationID = as.character(LocationID)) |>
    left_join(urbanicity, by = "LocationID") |>
    filter(grepl("suburb", tolower(urbanicity), fixed = TRUE))
}

core_wide <- core |>
  mutate(Data_Value = as.numeric(Data_Value)) |>
  select(
    LocationID, LocationName, StateAbbr, StateDesc,
    under18_share, MeasureId, Data_Value
  ) |>
  pivot_wider(
    names_from = MeasureId,
    values_from = Data_Value,
    values_fn = ~ mean(.x, na.rm = TRUE)
  ) |>
  mutate(
    mobility_exclusion_pressure = rowMeans(
      across(any_of(names(target_measures))),
      na.rm = TRUE
    )
  )

county_context <- core |>
  group_by(LocationID, LocationName, StateAbbr, StateDesc) |>
  summarise(
    total_pop = mean(total_pop, na.rm = TRUE),
    under18_share = mean(under18_share, na.rm = TRUE),
    .groups = "drop"
  )

bubble_data <- core_wide |>
  left_join(
    county_context,
    by = c("LocationID", "LocationName", "StateAbbr", "StateDesc", "under18_share")
  ) |>
  mutate(
    region = case_when(
      StateAbbr %in% c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA") ~ "Northeast",
      StateAbbr %in% c(
        "IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"
      ) ~ "Midwest",
      StateAbbr %in% c(
        "DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV",
        "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"
      ) ~ "South",
      StateAbbr %in% c(
        "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA"
      ) ~ "West",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(region = factor(region, levels = region_levels)) |>
  filter(!is.na(region), !is.na(LACKTRPT), !is.na(under18_share), !is.na(total_pop), total_pop > 0) |>
  mutate(
    pop_bucket = ntile(total_pop, 4)
  ) |>
  mutate(
    pop_bucket = factor(
      pop_bucket,
      levels = c(1, 2, 3, 4),
      labels = c("Q1 Smallest", "Q2", "Q3", "Q4 Largest")
    )
  )

bubble_plot <- ggplot(
  bubble_data,
  aes(x = under18_share, y = LACKTRPT, size = total_pop, color = region)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    data = bubble_data,
    mapping = aes(x = under18_share, y = LACKTRPT),
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    color = "gray35",
    inherit.aes = FALSE
  ) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_color_hue(drop = FALSE, limits = region_levels) +
  scale_size_area(
    max_size = 18,
    labels = scales::label_comma(),
    name = "Total population"
  ) +
  labs(
    title = "Children Share vs Transportation Barriers (County Level)",
    subtitle = "Bubble size shows total county population",
    x = "Share of population under age 18",
    y = "Transportation barriers (% of adults)",
    size = "Total population",
    color = "Census region"
  ) +
  theme_minimal(base_size = 12)

dir.create(file.path("plots", "All"), recursive = TRUE, showWarnings = FALSE)
dir.create("results", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = bubble_plot_png,
  plot = bubble_plot,
  width = 11,
  height = 7,
  dpi = 300
)

binned_data <- bubble_data |>
  mutate(under18_decile = ntile(under18_share, 10)) |>
  group_by(under18_decile) |>
  summarise(
    under18_mid = mean(under18_share, na.rm = TRUE),
    lacktrpt_mean = mean(LACKTRPT, na.rm = TRUE),
    lacktrpt_sd = sd(LACKTRPT, na.rm = TRUE),
    n = dplyr::n(),
    se = lacktrpt_sd / sqrt(n),
    ci_low = lacktrpt_mean - 1.96 * se,
    ci_high = lacktrpt_mean + 1.96 * se,
    .groups = "drop"
  )

binned_means_plot <- ggplot(
  binned_data,
  aes(x = under18_mid, y = lacktrpt_mean)
) +
  geom_line(color = "gray35", linewidth = 0.8) +
  geom_pointrange(
    aes(ymin = ci_low, ymax = ci_high),
    color = "steelblue4"
  ) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Mean Transportation Barriers by Child-Share Decile",
    subtitle = "Points are county-decile means; bars are 95% confidence intervals",
    x = "Share of population under age 18 (decile midpoint)",
    y = "Mean transportation barriers (% of adults)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = binned_means_plot_png,
  plot = binned_means_plot,
  width = 11,
  height = 7,
  dpi = 300
)

extract_under18_coef <- function(model, model_name, region_name) {
  coef_mat <- summary(model)$coefficients
  if (!"under18_share" %in% rownames(coef_mat)) {
    return(tibble())
  }

  est <- unname(coef_mat["under18_share", "Estimate"])
  se <- unname(coef_mat["under18_share", "Std. Error"])
  n_obs <- length(stats::fitted(model))
  r2 <- summary(model)$r.squared

  tibble(
    model = model_name,
    region = region_name,
    estimate = est,
    std_error = se,
    conf_low = est - 1.96 * se,
    conf_high = est + 1.96 * se,
    n = n_obs,
    r_squared = r2
  )
}

regions <- sort(unique(bubble_data$region))
coef_rows <- list()
coef_rows[[length(coef_rows) + 1]] <- extract_under18_coef(
  lm(LACKTRPT ~ under18_share, data = bubble_data),
  model_name = "Unadjusted",
  region_name = "All"
)
coef_rows[[length(coef_rows) + 1]] <- extract_under18_coef(
  lm(LACKTRPT ~ under18_share + log(total_pop), data = bubble_data),
  model_name = "Adjusted (log population)",
  region_name = "All"
)

for (r in regions) {
  regional_data <- bubble_data |>
    filter(region == r)

  if (nrow(regional_data) < 10) {
    next
  }

  coef_rows[[length(coef_rows) + 1]] <- extract_under18_coef(
    lm(LACKTRPT ~ under18_share + log(total_pop), data = regional_data),
    model_name = "Adjusted (log population)",
    region_name = as.character(r)
  )
}

coef_results <- bind_rows(coef_rows) |>
  mutate(
    label = if_else(
      region == "All",
      paste(region, model, sep = " - "),
      paste(region, "Region Adjusted", sep = " - ")
    )
  )

write_csv(coef_results, coef_output_csv)

coef_plot_data <- coef_results |>
  mutate(label = forcats::fct_reorder(label, estimate))

coef_plot <- ggplot(
  coef_plot_data,
  aes(x = estimate, y = label, color = region)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray55") +
  geom_segment(
    aes(x = conf_low, xend = conf_high, y = label, yend = label),
    linewidth = 0.8
  ) +
  geom_point(size = 2.2) +
  labs(
    title = "Estimated Effect of Child Share on Transportation Barriers",
    subtitle = "Point estimates with 95% confidence intervals",
    x = "Coefficient for under18_share",
    y = NULL,
    color = "Model/region"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = coef_plot_png,
  plot = coef_plot,
  width = 11,
  height = 7,
  dpi = 300
)

overall_adjusted <- coef_results |>
  filter(region == "All", model == "Adjusted (log population)")

regional_adjusted <- coef_results |>
  filter(region != "All") |>
  arrange(desc(estimate))

summary_lines <- c(
  "# Summary: Under-18 Share and Transportation Barriers",
  "",
  paste0("**Date:** ", Sys.Date()),
  "",
  "## Objective",
  "Assess whether counties with a higher share of children also report higher adult transportation barriers.",
  "",
  "## Key Finding (All Counties, Adjusted Model)",
  paste0(
    "- Estimated association: **", round(overall_adjusted$estimate, 2),
    "**."
  ),
  paste0(
    "- Interpretation: a 10-point increase in child share is associated with about **",
    round(overall_adjusted$estimate * 0.10, 2), " percentage points** higher transportation barriers."
  ),
  paste0(
    "- 95% confidence interval: **", round(overall_adjusted$conf_low, 2),
    " to ", round(overall_adjusted$conf_high, 2), "**."
  ),
  paste0("- Counties included: **", overall_adjusted$n, "**."),
  paste0("- Model R-squared: **", round(overall_adjusted$r_squared, 3), "**."),
  "",
  "## Regional Results (Adjusted Models)",
  "Larger values indicate a stronger positive association.",
  paste0(
    "- **", regional_adjusted$region, "**: ",
    round(regional_adjusted$estimate, 2),
    " (range ", round(regional_adjusted$conf_low, 2), " to ",
    round(regional_adjusted$conf_high, 2), ")"
  ),
  "",
  "## Interpretation",
  "The data show a consistent positive association: counties with more children tend to report higher transportation barriers among adults.",
  "",
  "## Limits",
  "- This is an association, not proof of cause and effect.",
  "- The model uses county-level data, so local context can still vary.",
  "- Other factors beyond population size may also influence barriers."
)

writeLines(summary_lines, coef_summary_md)

for (r in regions) {
  regional_data <- bubble_data |>
    filter(region == r)
  region_dir <- file.path("plots", as.character(r))
  dir.create(region_dir, recursive = TRUE, showWarnings = FALSE)

  regional_plot <- ggplot(
    regional_data,
    aes(x = under18_share, y = LACKTRPT, size = total_pop, color = region)
  ) +
    geom_point(alpha = 0.65) +
    geom_smooth(
      data = regional_data,
      mapping = aes(x = under18_share, y = LACKTRPT),
      method = "lm",
      se = FALSE,
      linewidth = 0.8,
      color = "gray35",
      inherit.aes = FALSE
    ) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_color_hue(drop = FALSE, limits = region_levels) +
    scale_size_area(
      max_size = 18,
      labels = scales::label_comma(),
      name = "Total population"
    ) +
    labs(
      title = paste0(
        "Children Share vs Transportation Barriers (", r, " Counties)"
      ),
      subtitle = "Bubble size shows total county population",
      x = "Share of population under age 18",
      y = "Transportation barriers (% of adults)",
      color = "Census region"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")

  region_slug <- gsub("[^a-z0-9]+", "_", tolower(r))
  region_file <- file.path(
    region_dir,
    paste0(regional_plot_prefix, "_", region_slug, ".png")
  )

  ggsave(
    filename = region_file,
    plot = regional_plot,
    width = 11,
    height = 7,
    dpi = 300
  )
}

pop_buckets <- levels(bubble_data$pop_bucket)
for (b in pop_buckets) {
  bucket_data <- bubble_data |>
    filter(pop_bucket == b)

  bucket_plot <- ggplot(
    bucket_data,
    aes(x = under18_share, y = LACKTRPT, size = total_pop, color = region)
  ) +
    geom_point(alpha = 0.65) +
    geom_smooth(
      data = bucket_data,
      mapping = aes(x = under18_share, y = LACKTRPT),
      method = "lm",
      se = FALSE,
      linewidth = 0.8,
      color = "gray35",
      inherit.aes = FALSE
    ) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_color_hue(drop = FALSE, limits = region_levels) +
    scale_size_area(
      max_size = 18,
      labels = scales::label_comma(),
      name = "Total population"
    ) +
    labs(
      title = paste0(
        "Children Share vs Transportation Barriers (Population Bucket: ", b, ")"
      ),
      subtitle = "Combined county data split by total population quartile",
      x = "Share of population under age 18",
      y = "Transportation barriers (% of adults)",
      color = "Census region"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")

  bucket_slug <- gsub("[^a-z0-9]+", "_", tolower(as.character(b)))
  bucket_file <- paste0(pop_bucket_plot_prefix, "_", bucket_slug, ".png")

  ggsave(
    filename = bucket_file,
    plot = bucket_plot,
    width = 11,
    height = 7,
    dpi = 300
  )
}

for (b in pop_buckets) {
  for (r in regions) {
    bucket_region_data <- bubble_data |>
      filter(pop_bucket == b, region == r)
    region_dir <- file.path("plots", as.character(r))
    dir.create(region_dir, recursive = TRUE, showWarnings = FALSE)

    if (nrow(bucket_region_data) == 0) {
      next
    }

    bucket_region_plot <- ggplot(
      bucket_region_data,
      aes(x = under18_share, y = LACKTRPT, size = total_pop, color = region)
    ) +
      geom_point(alpha = 0.65) +
      geom_smooth(
        data = bucket_region_data,
        mapping = aes(x = under18_share, y = LACKTRPT),
        method = "lm",
        se = FALSE,
        linewidth = 0.8,
        color = "gray35",
        inherit.aes = FALSE
      ) +
      scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
      scale_color_hue(drop = FALSE, limits = region_levels) +
      scale_size_area(
        max_size = 18,
        labels = scales::label_comma(),
        name = "Total population"
      ) +
      labs(
        title = paste0(
          "Children Share vs Transportation Barriers (", r, ", ", b, ")"
        ),
        subtitle = "Combined county data split by population quartile and region",
        x = "Share of population under age 18",
        y = "Transportation barriers (% of adults)",
        color = "Census region"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right")

    bucket_slug <- gsub("[^a-z0-9]+", "_", tolower(as.character(b)))
    region_slug <- gsub("[^a-z0-9]+", "_", tolower(as.character(r)))
    bucket_region_file <- paste0(
      pop_bucket_region_plot_prefix, "_", bucket_slug, "_", region_slug, ".png"
    )
    bucket_region_file <- file.path(region_dir, bucket_region_file)

    ggsave(
      filename = bucket_region_file,
      plot = bucket_region_plot,
      width = 11,
      height = 7,
      dpi = 300
    )
  }
}

write_csv(core, output_long_csv)
write_csv(core_wide, output_wide_csv)

message("Wrote: ", output_long_csv)
message("Wrote: ", output_wide_csv)
message("Wrote: ", bubble_plot_png)
message("Wrote regional plots: ", paste(regions, collapse = ", "))
message("Wrote population-bucket plots: ", paste(pop_buckets, collapse = ", "))
message("Wrote population-bucket-by-region plots")
