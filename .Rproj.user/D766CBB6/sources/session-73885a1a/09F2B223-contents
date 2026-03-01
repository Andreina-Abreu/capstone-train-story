library(tidyverse)
library(pdftools)
library(stringr)
library(readr)

pdf_dir <- "data/jreast/monthly"
out_dir <- "data/processed/jreast"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pdf_paths <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)
if (length(pdf_paths) == 0) stop("No PDFs found in data/jreast/monthly")

get_issue_year <- function(text) {
  m <- str_match(text, "(20\\d{2})年\\d{1,2}月\\d{1,2}日")
  as.integer(m[,2])
}

parse_month_line <- function(line, issue_year) {
  line <- str_replace_all(line, "（", "(")
  line <- str_replace_all(line, "）", ")")
  line <- str_squish(line)
  
  mm <- as.integer(str_match(line, "^(\\d{1,2})月")[,2])
  if (is.na(mm)) return(NULL)
  
  year <- if (mm >= 4) issue_year - 1 else issue_year
  date <- as.Date(sprintf("%04d-%02d-01", year, mm))
  
  no_paren <- str_replace_all(line, "\\([^\\)]*\\)", " ")
  no_paren <- str_replace_all(no_paren, "\\bP\\b", " ")
  no_paren <- str_squish(no_paren)
  
  nums <- str_extract_all(no_paren, "\\d+\\.?\\d*")[[1]] %>% as.numeric()
  
  if (length(nums) < 9) return(NULL)
  nums <- nums[1:min(length(nums), 10)]  
  
  out <- tibble(
    date = date,
    revenue_commuter = nums[1],
    revenue_noncommuter_short = nums[2],
    revenue_noncommuter_midlong = nums[3],
    revenue_noncommuter_subtotal = nums[4],
    revenue_total = nums[5],
    shinkansen_tohoku = nums[6],
    shinkansen_joetsu = nums[7],
    shinkansen_hokuriku = nums[8],
    shinkansen_total = nums[9],
    commuter_pass_weekday_tokyo = ifelse(length(nums) >= 10, nums[10], NA_real_)
  )
  
  out
}

parse_one_pdf <- function(pdf_path) {
  txt <- pdf_text(pdf_path)[1]
  issue_year <- get_issue_year(txt)
  if (is.na(issue_year)) stop("Could not detect issue year from: ", basename(pdf_path))
  
  lines <- str_split(txt, "\n")[[1]] |> str_squish()
  lines <- lines[lines != ""]
  
  month_lines <- lines[str_detect(lines, "^\\d{1,2}月\\b")]
  
  out <- map_dfr(month_lines, ~ parse_month_line(.x, issue_year))
  
  out %>%
    mutate(
      file = basename(pdf_path),
      issue_year = issue_year,
      fy = if_else(lubridate::month(date) <= 3, lubridate::year(date), lubridate::year(date) + 1)
    ) %>%
    arrange(date)
}

monthly_all <- map_dfr(pdf_paths, parse_one_pdf)

if (nrow(monthly_all) == 0) stop("No month rows extracted. Check PDF formatting.")

write_csv(monthly_all, file.path(out_dir, "jreast_monthly_all.csv"))

monthly_2023_2025 <- monthly_all %>%
  filter(date >= as.Date("2023-04-01"), date <= as.Date("2025-03-31"))

write_csv(monthly_2023_2025, file.path(out_dir, "jreast_monthly_2023_2025.csv"))

cat("Wrote:\n")
cat("- ", file.path(out_dir, "jreast_monthly_all.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "jreast_monthly_2023_2025.csv"), "\n", sep = "")

cat("\nCounts by file (all):\n")
print(monthly_all %>% count(file))

cat("\nDate ranges by file (all):\n")
print(monthly_all %>% group_by(file) %>% summarise(min=min(date), max=max(date), n=n(), .groups="drop"))

cat("\nFY counts (2023–2025 window):\n")
print(monthly_2023_2025 %>% count(fy))