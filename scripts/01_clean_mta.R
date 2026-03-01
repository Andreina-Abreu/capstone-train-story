library(tidyverse)
library(lubridate)
library(stringr)

hourly <- readr::read_csv(
  "data/mta/mta_hourly.csv",
  col_select = c(transit_timestamp, ridership),
  show_col_types = FALSE
)

hourly2 <- hourly %>%
  mutate(
    transit_timestamp_clean = str_trim(str_remove(transit_timestamp, "\\s*\\+.*$")),
    ts = mdy_hms(transit_timestamp_clean, tz = "America/New_York", quiet = TRUE),
    date = as.Date(ts),
    hour = hour(ts),
    day_type = if_else(wday(ts) %in% c(1, 7), "Weekend", "Weekday")
  ) %>%
  filter(!is.na(ts), !is.na(hour))

daily_hour <- hourly2 %>%
  group_by(date, day_type, hour) %>%
  summarize(riders = sum(ridership, na.rm = TRUE), .groups = "drop")

mta_hour_profile <- daily_hour %>%
  group_by(day_type, hour) %>%
  summarize(
    avg_riders = mean(riders),
    p10 = quantile(riders, 0.10),
    p90 = quantile(riders, 0.90),
    .groups = "drop"
  ) %>%
  arrange(day_type, hour)

readr::write_csv(mta_hour_profile, "data/mta/mta_hour_profile.csv")
