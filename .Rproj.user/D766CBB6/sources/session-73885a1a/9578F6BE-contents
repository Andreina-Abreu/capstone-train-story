library(tidyverse)

gtfs_path <- "data/jreast"
out_path  <- "data/processed/jreast"
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

hmss_to_seconds <- function(x) {
  parts <- stringr::str_split_fixed(x, ":", 3)
  h <- as.integer(parts[,1]); m <- as.integer(parts[,2]); s <- as.integer(parts[,3])
  h*3600 + m*60 + s
}

sec_to_hm <- function(sec){
  sec <- sec %% (24*3600)
  sprintf("%02d:%02d", sec %/% 3600, (sec %% 3600) %/% 60)
}

routes     <- readr::read_csv(file.path(gtfs_path, "routes.txt"), show_col_types = FALSE)
trips      <- readr::read_csv(file.path(gtfs_path, "trips.txt"), show_col_types = FALSE)
stop_times <- readr::read_csv(file.path(gtfs_path, "stop_times.txt"), show_col_types = FALSE)
stops      <- readr::read_csv(file.path(gtfs_path, "stops.txt"), show_col_types = FALSE)

library(jsonlite)

station_info <- jsonlite::fromJSON(file.path(gtfs_path, "jre_station_info.json"), flatten = TRUE)

# mapping: Japanese -> English
station_name_map <- station_info %>%
  transmute(
    stop_name_ja = `odpt:stationTitle.ja`,
    stop_name_en = `odpt:stationTitle.en`
  ) %>%
  filter(!is.na(stop_name_ja), !is.na(stop_name_en)) %>%
  distinct(stop_name_ja, .keep_all = TRUE)

stops <- stops %>%
  left_join(station_name_map, by = c("stop_name" = "stop_name_ja")) %>%
  mutate(stop_name_display = coalesce(stop_name_en, stop_name))


calendar_exists <- file.exists(file.path(gtfs_path, "calendar.txt"))
if (calendar_exists) {
  calendar <- readr::read_csv(file.path(gtfs_path, "calendar.txt"), show_col_types = FALSE)
}


route_desc_safe <- if ("route_desc" %in% names(routes)) routes$route_desc else ""

y_routes <- routes %>%
  mutate(search_text = str_to_lower(paste(route_short_name, route_long_name, route_desc_safe))) %>%
  filter(str_detect(search_text, "yamanote|山手"))

if (nrow(y_routes) == 0) {
  print(routes %>% select(route_id, route_short_name, route_long_name) %>% print(n = 200))
  stop("Couldn't find Yamanote in routes.txt. Print output above and adjust the pattern.")
}

y_trips <- trips %>% semi_join(y_routes, by = "route_id")

y_stop_times <- stop_times %>%
  inner_join(y_trips %>% select(trip_id, route_id, service_id), by = "trip_id") %>%
  mutate(dep_sec = hmss_to_seconds(departure_time),
         hour = floor((dep_sec %% (24*3600)) / 3600))

y_stops <- stops %>% semi_join(y_stop_times, by = "stop_id")

ref_stop <- y_stops %>%
  mutate(name_low = str_to_lower(stop_name_display)) %>%
  arrange(stop_name_display)

ref_stop_id <- ref_stop %>%
  filter(str_detect(name_low, "shinjuku|新宿")) %>%
  slice(1) %>%
  pull(stop_id)

if (length(ref_stop_id) == 0) {
  ref_stop_id <- ref_stop %>% slice(1) %>% pull(stop_id)
}

ref_stop_name <- y_stops %>%
  filter(stop_id == ref_stop_id) %>%
  slice(1) %>%
  pull(stop_name_display)

y_hourly <- y_stop_times %>%
  filter(stop_id == ref_stop_id) %>%
  count(hour, name = "scheduled_departures") %>%
  mutate(ref_stop = ref_stop_name)

y_headways <- y_stop_times %>%
  filter(stop_id == ref_stop_id) %>%
  arrange(dep_sec) %>%
  mutate(headway_min = (dep_sec - lag(dep_sec))/60) %>%
  filter(!is.na(headway_min), headway_min > 0, headway_min < 60) %>%
  transmute(ref_stop = ref_stop_name,
            departure_time,
            dep_sec,
            hour,
            headway_min)

y_span <- y_stop_times %>%
  filter(stop_id == ref_stop_id) %>%
  summarize(
    ref_stop = ref_stop_name,
    first_dep_sec = min(dep_sec, na.rm = TRUE),
    last_dep_sec  = max(dep_sec, na.rm = TRUE)
  ) %>%
  mutate(first_time = sec_to_hm(first_dep_sec),
         last_time  = sec_to_hm(last_dep_sec))

y_week_profile <- NULL
if (calendar_exists) {
  service_daytype <- calendar %>%
    mutate(day_type = case_when(
      monday==1 & tuesday==1 & wednesday==1 & thursday==1 & friday==1 & saturday==0 & sunday==0 ~ "Weekday",
      saturday==1 & sunday==1 & monday==0 & tuesday==0 & wednesday==0 & thursday==0 & friday==0 ~ "Weekend",
      TRUE ~ "Other"
    )) %>%
    select(service_id, day_type)
  
  y_week_profile <- y_stop_times %>%
    filter(stop_id == ref_stop_id) %>%
    left_join(service_daytype, by = "service_id") %>%
    filter(day_type %in% c("Weekday","Weekend")) %>%
    count(day_type, hour, name = "scheduled_departures") %>%
    mutate(ref_stop = ref_stop_name)
}

readr::write_csv(y_routes,    file.path(out_path, "yamanote_routes.csv"))
readr::write_csv(y_stops,     file.path(out_path, "yamanote_stops.csv"))
readr::write_csv(y_hourly,    file.path(out_path, "yamanote_hourly_departures.csv"))
readr::write_csv(y_headways,  file.path(out_path, "yamanote_headways.csv"))
readr::write_csv(y_span,      file.path(out_path, "yamanote_service_span.csv"))
if (!is.null(y_week_profile)) {
  readr::write_csv(y_week_profile, file.path(out_path, "yamanote_weekday_weekend_profile.csv"))
}

station_dict <- y_stops %>%
  distinct(stop_id, stop_name, stop_name_display) %>%
  arrange(stop_name_display)

readr::write_csv(station_dict, file.path(out_path, "yamanote_station_dictionary.csv"))

message("JR East done. Reference stop: ", ref_stop_name)
