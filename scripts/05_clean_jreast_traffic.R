
library(tidyverse)
library(readxl)
library(stringr)
library(janitor)

gtfs_path <- "data/jreast"
in_path   <- file.path(gtfs_path, "traffic.xlsx")
out_path  <- "data/processed/jreast"
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

raw <- readxl::read_excel(in_path, sheet = 1, col_names = FALSE)

fy_rows <- which(apply(raw, 1, function(r) any(r == "FY", na.rm = TRUE)))
if (length(fy_rows) < 2) {
  stop("Could not find two tables. Expected two 'FY' header rows (levels + YoY).")
}

extract_table <- function(raw, fy_row) {
  header <- raw[fy_row, ] |> unlist() |> as.character()
  
  start <- fy_row + 1
  tail_df <- raw[start:nrow(raw), , drop = FALSE]
  blank_rel <- which(rowSums(!is.na(tail_df)) == 0)[1]
  end <- if (is.na(blank_rel)) nrow(raw) else (start + blank_rel - 2)
  
  tbl <- raw[start:end, , drop = FALSE]
  names(tbl) <- paste0("c", seq_len(ncol(tbl)))
  
  year_cols <- which(str_detect(header, "^\\d{4}\\.3$"))
  if (length(year_cols) == 0) stop("No FY columns like '2013.3' found in header row.")
  
  out <- tbl %>%
    transmute(
      category    = as.character(.data$c1),
      region      = as.character(.data$c2),
      ticket_type = as.character(.data$c3),
      across(all_of(paste0("c", year_cols)), ~ suppressWarnings(as.numeric(.x)))
    )
  
  year_names <- header[year_cols]
  names(out)[4:ncol(out)] <- year_names
  
  out %>%
    mutate(
      category    = na_if(str_squish(category), ""),
      region      = na_if(str_squish(region), ""),
      ticket_type = str_squish(ticket_type)
    ) %>%
    tidyr::fill(category, region, .direction = "down") %>%
    filter(ticket_type %in% c("定期", "定期外", "計")) %>%
    pivot_longer(
      cols = matches("^\\d{4}\\.3$"),
      names_to = "fy",
      values_to = "value"
    ) %>%
    mutate(fy = as.integer(str_remove(fy, "\\.3$")))
}

traffic_levels <- extract_table(raw, fy_rows[1]) %>%
  rename(passenger_km_millions = value)

traffic_yoy <- extract_table(raw, fy_rows[2]) %>%
  rename(yoy_percent = value)

traffic_levels <- traffic_levels %>% filter(fy >= 2023, fy <= 2025)
traffic_yoy    <- traffic_yoy %>%    filter(fy >= 2023, fy <= 2025)

readr::write_csv(
  traffic_levels,
  file.path(out_path, "jreast_transport_volume_2023_2025.csv")
)
readr::write_csv(
  traffic_yoy,
  file.path(out_path, "jreast_transport_volume_yoy_2023_2025.csv")
)

cat("\nWrote:\n",
    "- ", file.path(out_path, "jreast_transport_volume_2023_2025.csv"), "\n",
    "- ", file.path(out_path, "jreast_transport_volume_yoy_2023_2025.csv"), "\n", sep = "")

cat("\nLevels columns:\n")
print(names(traffic_levels))

cat("\nSample (levels):\n")
print(dplyr::slice_head(traffic_levels, n = 12))