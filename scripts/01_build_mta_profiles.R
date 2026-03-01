library(tidyverse)
library(lubridate)
library(stringr)

stopifnot(file.exists("data/mta/mta_hourly.csv"))

hourly <- readr::read_csv(
  "data/mta/mta_hourly.csv",
  col_select = c(transit_timestamp, transit_mode, station_complex, ridership),
  show_col_types = FALSE
)

bmt_routes <- c("J","Z","L","M","N","Q","R","W")
bmt_pat <- paste0("(^|\\s)(", paste(bmt_routes, collapse = "|"), ")(\\s|$)")

hourly_bmt <- hourly %>%
  filter(transit_mode == "subway") %>%
  mutate(
    routes_txt = str_match(station_complex, "\\(([^)]*)\\)")[,2],
    routes_txt = coalesce(routes_txt, ""),
    is_bmt_complex = str_detect(routes_txt, bmt_pat),
    transit_timestamp_clean = str_trim(str_remove(transit_timestamp, "\\s*\\+.*$")),
    ts = mdy_hms(transit_timestamp_clean, tz = "America/New_York", quiet = TRUE),
    date = as.Date(ts),
    hour = hour(ts),
    day_type = if_else(wday(ts) %in% c(1, 7), "Weekend", "Weekday")
  ) %>%
  filter(is_bmt_complex, !is.na(ts), !is.na(hour))

daily_hour_bmt <- hourly_bmt %>%
  group_by(date, day_type, hour) %>%
  summarise(riders = sum(ridership, na.rm = TRUE), .groups = "drop")

bmt_hour_profile <- daily_hour_bmt %>%
  group_by(day_type, hour) %>%
  summarise(
    avg_riders = mean(riders, na.rm = TRUE),
    p10 = quantile(riders, 0.10, na.rm = TRUE),
    p90 = quantile(riders, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(day_type, hour)

out_file <- "data/mta/bmt_hour_profile.csv"
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(bmt_hour_profile, out_file)

file.exists(out_file)
