if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
p_load(tidyverse, dplyr, data.table, ggplot2, fastDummies, readxl)

input_csv <- "PLACES__Local_Data_for_Better_Health,_County_Data_2024_release_20260223.csv"
output_long_csv <- "places_filtered_mobility_proxies_long.csv"
output_wide_csv <- "places_filtered_mobility_proxies_wide.csv"
bubble_plot_png <- "bubble_under18_vs_transport_barriers.png"
urbanicity_csv <- "county_urbanicity.csv" # Optional: must include LocationID + urbanicity

target_measures <- c(
  LACKTRPT = "Transportation Barriers",
  MOBILITY = "Mobility Disability",
  INDEPLIVE = "Independent Living Disability",
  LPA = "Physical Inactivity",
  MHLTH = "Frequent Mental Distress",
  ISOLATION = "Social Isolation"
)

places <- read_csv(input_csv, show_col_types = FALSE)

core <- places |>
  filter(
    Year == 2022,
    DataSource == "BRFSS",
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
      TRUE ~ "Other"
    )
  ) |>
  filter(!is.na(LACKTRPT), !is.na(under18_share), !is.na(total_pop), total_pop > 0)

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
  scale_size_continuous(labels = scales::label_comma()) +
  labs(
    title = "Children Share vs Transportation Barriers (County Level)",
    subtitle = "Bubble size shows total county population",
    x = "Share of population under age 18",
    y = "Transportation barriers (% of adults)",
    size = "Total population",
    color = "Census region"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = bubble_plot_png,
  plot = bubble_plot,
  width = 11,
  height = 7,
  dpi = 300
)

write_csv(core, output_long_csv)
write_csv(core_wide, output_wide_csv)

message("Wrote: ", output_long_csv)
message("Wrote: ", output_wide_csv)
message("Wrote: ", bubble_plot_png)
