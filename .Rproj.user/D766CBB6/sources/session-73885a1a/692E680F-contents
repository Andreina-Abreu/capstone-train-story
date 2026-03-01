library(tidyverse)

gtfs_path <- "data/jreast"

routes     <- read_csv(file.path(gtfs_path, "routes.txt"), show_col_types = FALSE)
trips      <- read_csv(file.path(gtfs_path, "trips.txt"), show_col_types = FALSE)
stop_times <- read_csv(file.path(gtfs_path, "stop_times.txt"), show_col_types = FALSE)
stops      <- read_csv(file.path(gtfs_path, "stops.txt"), show_col_types = FALSE)
calendar   <- read_csv(file.path(gtfs_path, "calendar.txt"), show_col_types = FALSE)

y_routes <- routes %>%
  mutate(search = str_to_lower(paste(route_short_name, route_long_name,
                                     if("route_desc" %in% names(.)) route_desc else ""))) %>%
  filter(str_detect(search, "yamanote|山手|\\bjy\\b"))

y_trips <- trips %>%
  semi_join(y_routes, by = "route_id") %>%
  left_join(calendar %>% select(service_id, monday, saturday, sunday), by = "service_id") %>%
  mutate(
    day_type = case_when(
      monday == 1 ~ "Weekday",
      saturday == 1 | sunday == 1 ~ "Weekend",
      TRUE ~ NA_character_
    )
  )

cat("Trips by day_type:\n")
print(y_trips %>% count(day_type, direction_id))


yamanote_stop_ids <- stop_times %>%
  semi_join(y_trips, by = "trip_id") %>%
  count(stop_id, sort = TRUE)

top_stops <- yamanote_stop_ids %>%
  left_join(stops %>% select(stop_id, stop_name), by = "stop_id") %>%
  head(30)

cat("\nTop Yamanote stops by trip count:\n")
print(top_stops)

ref_stop_id <- stops %>%
  filter(str_detect(stop_name, "新宿|Shinjuku")) %>%
  semi_join(yamanote_stop_ids, by = "stop_id") %>%
  slice(1) %>%
  pull(stop_id)

ref_stop_name <- stops %>% filter(stop_id == ref_stop_id) %>% pull(stop_name) %>% first()
cat("\nUsing reference stop:", ref_stop_name, "(", ref_stop_id, ")\n")

ref_deps <- stop_times %>%
  filter(stop_id == ref_stop_id) %>%
  semi_join(y_trips, by = "trip_id") %>%
  left_join(y_trips %>% select(trip_id, direction_id, service_id, day_type),
            by = "trip_id") %>%
  mutate(
    dep_sec = {
      parts <- str_split_fixed(departure_time, ":", 3)
      as.numeric(parts[,1]) * 3600 + as.numeric(parts[,2]) * 60 + as.numeric(parts[,3])
    },
    hour = floor(dep_sec / 3600) %% 24  
  )


headways_fixed <- ref_deps %>%
  filter(!is.na(day_type)) %>%
  arrange(day_type, direction_id, dep_sec) %>%
  group_by(day_type, direction_id, service_id) %>%
  mutate(headway_min = (dep_sec - lag(dep_sec)) / 60) %>%
  ungroup() %>%
  filter(
    !is.na(headway_min),
    headway_min > 0,
    headway_min < 30   
  ) %>%
  filter(direction_id == min(direction_id))

cat("\n=== Fixed headway summary ===\n")
print(summary(headways_fixed$headway_min))
print(table(round(headways_fixed$headway_min)))

headway_profile <- headways_fixed %>%
  filter(hour >= 5, hour <= 23) %>%
  group_by(day_type, hour) %>%
  summarise(
    ref_stop      = ref_stop_name,
    n_obs         = n(),
    median_hw_min = median(headway_min),
    q25           = quantile(headway_min, 0.25),
    q75           = quantile(headway_min, 0.75),
    .groups = "drop"
  )

cat("\n=== Headway profile (should show ~3-4 min peak, ~5-6 min off-peak) ===\n")
print(headway_profile, n = 50)

write_csv(headways_fixed %>%
            mutate(ref_stop = ref_stop_name) %>%
            select(ref_stop, day_type, direction_id, departure_time, dep_sec, hour, headway_min),
          "data/processed/jreast/yamanote_headways.csv")

write_csv(headway_profile,
          "data/processed/jreast/yamanote_headway_profile.csv")

cat("\n✓ Saved fixed yamanote_headways.csv and yamanote_headway_profile.csv\n")
