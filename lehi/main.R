data_dir <- file.path("lehi", "data")
output_csv <- file.path("lehi", "bikeway_hourly_aggregated.csv")
output_plot <- file.path("lehi", "bikeway_hourly_area.png")

target_columns <- list(
  bike_passenger = "Bike Passenger",
  stroller = "Stroller",
  jogger = "Jogger",
  child_proxy = c("Backpack", "Student")
)

target_labels <- c(
  bike_passenger = "Bike passenger",
  stroller = "Stroller",
  jogger = "Jogger",
  child_proxy = "Backpack / Student"
)

target_colors <- c(
  bike_passenger = "#F8766D",
  stroller = "#7CAE00",
  jogger = "#C77CFF",
  child_proxy = "#00BFC4"
)

legend_order <- c("bike_passenger", "stroller", "child_proxy", "jogger")

parse_count_file <- function(path) {
  raw <- read.csv(
    path,
    header = FALSE,
    stringsAsFactors = FALSE,
    fill = TRUE,
    check.names = FALSE
  )

  rows <- as.data.frame(lapply(raw, as.character), stringsAsFactors = FALSE)
  timestamp_col <- trimws(rows[[2]])
  block_starts <- which(grepl("^1899-12-30( [0-9]{2}:[0-9]{2})?$", timestamp_col))

  if (!length(block_starts)) {
    stop(sprintf("No timestamp rows found in %s", basename(path)))
  }

  date_match <- regexpr("[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}", basename(path))
  if (date_match < 0) {
    stop(sprintf("Unable to parse date from %s", basename(path)))
  }

  count_date <- as.Date(regmatches(basename(path), date_match), format = "%m.%d.%y")
  records <- vector("list", length(block_starts))

  for (idx in seq_along(block_starts)) {
    start_row <- block_starts[idx]
    header <- trimws(unlist(rows[start_row - 1, ], use.names = FALSE))
    block <- rows[(start_row + 1):(start_row + 4), , drop = FALSE]

    sums <- vapply(target_columns, function(column_names) {
      cols <- which(header %in% column_names)
      if (!length(cols)) {
        return(0)
      }

      values <- suppressWarnings(as.numeric(unlist(block[, cols, drop = FALSE], use.names = FALSE)))
      sum(values, na.rm = TRUE)
    }, numeric(1))

    time_text <- trimws(rows[start_row, 2])
    time_piece <- if (grepl(" ", time_text, fixed = TRUE)) {
      sub("^.* ", "", time_text)
    } else {
      "00:00"
    }

    datetime <- as.POSIXct(
      sprintf("%s %s", format(count_date, "%Y-%m-%d"), time_piece),
      tz = "America/Denver"
    )
    hour_bucket <- as.POSIXct(strftime(datetime, "%Y-%m-%d %H:00:00"), tz = "America/Denver")

    records[[idx]] <- data.frame(
      source_file = basename(path),
      count_date = as.character(count_date),
      hour_bucket = format(hour_bucket, "%H:00"),
      bike_passenger = sums[["bike_passenger"]],
      stroller = sums[["stroller"]],
      jogger = sums[["jogger"]],
      child_proxy = sums[["child_proxy"]],
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, records)
}

count_files <- sort(list.files(
  data_dir,
  pattern = "Hourly Bike-Ped Counts\\.csv$",
  full.names = TRUE
))

if (!length(count_files)) {
  stop(sprintf("No Hourly Bike-Ped Counts CSV files found in %s", data_dir))
}

interval_counts <- do.call(rbind, lapply(count_files, parse_count_file))
hour_levels <- sprintf("%02d:00", 0:23)
hourly_totals <- aggregate(
  interval_counts[c("bike_passenger", "stroller", "jogger", "child_proxy")],
  by = list(hour = interval_counts$hour_bucket),
  FUN = sum,
  na.rm = TRUE
)

hourly_totals <- merge(
  data.frame(hour = hour_levels, stringsAsFactors = FALSE),
  hourly_totals,
  by = "hour",
  all.x = TRUE,
  sort = FALSE
)
hourly_totals[is.na(hourly_totals)] <- 0

write.csv(hourly_totals, output_csv, row.names = FALSE)

stacked_matrix <- as.matrix(hourly_totals[names(target_labels)])
colnames(stacked_matrix) <- unname(target_labels)
stacked_cumulative <- t(apply(stacked_matrix, 1, cumsum))
x <- seq_len(nrow(stacked_matrix))

png(output_plot, width = 1400, height = 800, res = 150)
par(mar = c(5, 5, 4, 1) + 0.1, family = "sans")

plot(
  x,
  stacked_cumulative[, ncol(stacked_cumulative)],
  type = "n",
  axes = FALSE,
  xaxt = "n",
  yaxt = "n",
  xlab = "Hour of day",
  ylab = "Combined hourly counts across 3 dates",
  main = "Bikeway traffic attributes by hour",
  sub = "Attributes summed across 15-minute blocks and combined across Aug 19, Nov 5, and Dec 12, 2025"
)

axis_positions <- seq(1, length(hour_levels), by = 2)
axis(1, at = axis_positions, labels = hour_levels[axis_positions], las = 2, cex.axis = 0.9)
axis(2, las = 1, labels = FALSE, tcl = -0.25)
axis(2, las = 1, tick = FALSE, line = -0.4)
box()
grid(nx = NA, ny = NULL, col = "#DDDDDD", lty = "dotted")

for (idx in seq_len(ncol(stacked_matrix))) {
  lower <- if (idx == 1) rep(0, nrow(stacked_matrix)) else stacked_cumulative[, idx - 1]
  upper <- stacked_cumulative[, idx]
  polygon(
    x = c(x, rev(x)),
    y = c(lower, rev(upper)),
    col = adjustcolor(unname(target_colors[idx]), alpha.f = 0.85),
    border = NA
  )
  lines(x, upper, col = "white", lwd = 1)
}

legend(
  "topright",
  legend = unname(target_labels[legend_order]),
  fill = adjustcolor(unname(target_colors[legend_order]), alpha.f = 0.85),
  border = NA,
  bty = "n",
  inset = 0.02
)

dev.off()

message(sprintf("Wrote %s", output_csv))
message(sprintf("Wrote %s", output_plot))
