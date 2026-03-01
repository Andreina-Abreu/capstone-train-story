library(tidyverse)
library(lubridate)

otp_raw <- readr::read_csv("data/mta/mta_otp.csv", show_col_types = FALSE)

nm <- names(otp_raw)

pick_col <- function(options) {
  hit <- options[options %in% nm]
  if (length(hit) == 0) NA_character_ else hit[1]
}

col_month     <- pick_col(c("month", "Month"))
col_division  <- pick_col(c("division", "Division"))
col_line      <- pick_col(c("line", "Line"))
col_day_type  <- pick_col(c("day_type", "Day Type", "daytype"))
col_otp       <- pick_col(c("terminal_on_time_performance", "terminal_on_time_performance_pct",
                            "Terminal On-Time Performance", "otp", "OTP"))
col_on_time   <- pick_col(c("num_on_time_trips", "On-Time Trips", "on_time_trips"))
col_sched     <- pick_col(c("num_sched_trips", "Scheduled Trips", "scheduled_trips", "sched_trips"))

needed <- c(col_month, col_division, col_line, col_day_type, col_otp, col_on_time, col_sched)
if (any(is.na(needed))) {
  stop(
    "Your mta_otp.csv is missing one or more required columns.\n",
    "I looked for: month, division, line, day_type, terminal_on_time_performance, num_on_time_trips, num_sched_trips.\n",
    "Found columns:\n", paste(names(otp_raw), collapse = ", ")
  )
}

otp <- otp_raw %>%
  rename(
    month = !!col_month,
    division = !!col_division,
    line = !!col_line,
    day_type = !!col_day_type,
    terminal_otp = !!col_otp,
    num_on_time_trips = !!col_on_time,
    num_sched_trips = !!col_sched
  )


otp <- otp %>%
  mutate(
    month = as.Date(month),
    day_type = case_when(
      as.character(day_type) %in% c("1", "Weekday", "WEEKDAY") ~ "Weekday",
      as.character(day_type) %in% c("2", "Weekend", "WEEKEND") ~ "Weekend",
      TRUE ~ as.character(day_type)
    ),
    division = as.character(division),
    line = as.character(line),
    terminal_otp = as.numeric(terminal_otp),
    num_on_time_trips = as.numeric(num_on_time_trips),
    num_sched_trips = as.numeric(num_sched_trips)
  ) %>%
  filter(!is.na(month), !is.na(line), !is.na(day_type))


bmt_lines <- c("N", "Q", "R", "W", "L", "JZ", "S Fkln")

otp_bmt <- otp %>%
  filter(division %in% c("B DIVISION", "B Division", "B")) %>%
  mutate(
    line = if_else(line %in% c("S-FK", "S FK", "S_FK", "Franklin", "Franklin Shuttle"),
                   "S Fkln", line)
  ) %>%
  filter(line %in% bmt_lines)


mta_otp_bmt_byline <- otp_bmt %>%
  group_by(month, day_type, line) %>%
  summarize(
    otp = sum(num_on_time_trips, na.rm = TRUE) / sum(num_sched_trips, na.rm = TRUE),
    sched_trips = sum(num_sched_trips, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month, day_type, line)

mta_otp_bmt_overall <- otp_bmt %>%
  group_by(month, day_type) %>%
  summarize(
    otp = sum(num_on_time_trips, na.rm = TRUE) / sum(num_sched_trips, na.rm = TRUE),
    sched_trips = sum(num_sched_trips, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(line = "BMT (overall)") %>%
  arrange(month, day_type)

readr::write_csv(mta_otp_bmt_byline,  "data/mta_otp_bmt_byline.csv")
readr::write_csv(mta_otp_bmt_overall, "data/mta_otp_bmt_overall.csv")

message("Saved: data/mta/mta_otp_bmt_byline.csv")
message("Saved: data/mta/mta_otp_bmt_overall.csv")

