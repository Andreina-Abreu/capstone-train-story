library(tidyverse)
library(readr)

hmss_to_seconds <- function(x) {
  parts <- str_split_fixed(x, ":", 3)
  h <- as.integer(parts[,1]); m <- as.integer(parts[,2]); s <- as.integer(parts[,3])
  h*3600 + m*60 + s
}

sec_to_hhmm <- function(sec) {
  h <- sec %/% 3600
  m <- (sec %% 3600) %/% 60
  sprintf("%02d:%02d", h, m)
}

routes      <- read_csv("data/mta/routes.txt", show_col_types = FALSE)
trips       <- read_csv("data/mta/trips.txt", show_col_types = FALSE)
stop_times  <- read_csv("data/mta/stop_times.txt", show_col_types = FALSE)
calendar    <- if (file.exists("data/mta/calendar.txt")) read_csv("data/calendar.txt", show_col_types = FALSE) else NULL
cal_dates   <- if (file.exists("data/mta/calendar_dates.txt")) read_csv("data/calendar_dates.txt", show_col_types = FALSE) else NULL

bmt_short <- c("N","Q","R","W","L","J","Z","S")

routes_bmt <- routes %>%
  mutate(
    route_short_name = as.character(route_short_name),
    route_long_name  = as.character(route_long_name),
    is_franklin = route_short_name == "S" & str_detect(str_to_lower(route_long_name), "franklin")
  ) %>%
  filter(
    route_short_name %in% c("N","Q","R","W","L","J","Z") |
      is_franklin
  ) %>%
  select(route_id, route_short_name, route_long_name, is_franklin)

write_csv(routes_bmt, "data/bmt_route_lookup.csv")


if (is.null(calendar)) stop("calendar.txt not found in /data (needed for weekday/weekend).")

cal_class <- calendar %>%
  mutate(
    weekday_sum = monday + tuesday + wednesday + thursday + friday,
    weekend_sum = saturday + sunday,
    day_type = case_when(
      weekday_sum == 5 & weekend_sum == 0 ~ "Weekday",
      weekday_sum == 0 & weekend_sum >= 1 ~ "Weekend",
      TRUE ~ "Mixed"
    )
  ) %>%
  select(service_id, day_type)

bmt_trips <- trips %>%
  inner_join(routes_bmt, by = "route_id") %>%
  left_join(cal_class, by = "service_id") %>%
  filter(day_type %in% c("Weekday","Weekend"))

trip_starts <- stop_times %>%
  semi_join(bmt_trips, by = "trip_id") %>%
  mutate(dep_sec = hmss_to_seconds(as.character(departure_time))) %>%
  group_by(trip_id) %>%
  summarize(start_dep_sec = min(dep_sec, na.rm = TRUE), .groups = "drop") %>%
  mutate(hour_service = start_dep_sec %/% 3600) # can be 0..29 etc

bmt_by_hour <- bmt_trips %>%
  select(trip_id, route_short_name, route_long_name, day_type) %>%
  inner_join(trip_starts, by = "trip_id") %>%
  group_by(day_type, route_short_name, hour_service) %>%
  summarize(trips = n(), .groups = "drop") %>%
  arrange(day_type, route_short_name, hour_service)


write_csv(filter(bmt_by_hour, day_type == "Weekday"), "data/bmt_trips_by_hour_weekday.csv")
write_csv(filter(bmt_by_hour, day_type == "Weekend"), "data/bmt_trips_by_hour_weekend.csv")


bmt_span <- bmt_trips %>%
  select(trip_id, route_short_name, day_type) %>%
  inner_join(trip_starts, by = "trip_id") %>%
  group_by(day_type, route_short_name) %>%
  summarize(
    first_dep_sec = min(start_dep_sec, na.rm = TRUE),
    last_dep_sec  = max(start_dep_sec, na.rm = TRUE),
    n_trips = n(),
    .groups = "drop"
  ) %>%
  mutate(
    first_departure = sec_to_hhmm(first_dep_sec),
    last_departure  = sec_to_hhmm(last_dep_sec)
  ) %>%
  arrange(day_type, route_short_name)

write_csv(bmt_span, "data/mta/bmt_service_span.csv")

message("Done! Wrote: bmt_trips_by_hour_weekday.csv, bmt_trips_by_hour_weekend.csv, bmt_service_span.csv, bmt_route_lookup.csv")
