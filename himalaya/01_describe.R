# 01_describe.R -- load the Himalayan Database (TidyTuesday 2020-09-22) and describe every variable
# Run from the himalaya/ folder:  Rscript 01_describe.R
suppressPackageStartupMessages({ library(readr); library(dplyr); library(tidyr) })

tt <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-09-22/"
get <- function(f) {
  local <- file.path("data", paste0(f, ".csv"))
  if (!file.exists(local)) download.file(paste0(tt, f, ".csv"), local)
  read_csv(local, show_col_types = FALSE)
}
members <- get("members"); expeditions <- get("expeditions"); peaks <- get("peaks")

describe <- function(df, name, target = NULL) {
  cat("\n\n==========", name, ":", nrow(df), "rows x", ncol(df), "columns ==========\n")
  for (v in names(df)) {
    x <- df[[v]]; na <- mean(is.na(x))
    cat(sprintf("\n-- %s  [%s]  missing %.1f%%  unique %d\n", v, class(x)[1], 100 * na, n_distinct(x)))
    if (is.numeric(x)) {
      q <- quantile(x, c(0, .01, .25, .5, .75, .99, 1), na.rm = TRUE)
      cat("   min/1%/25%/median/75%/99%/max:", paste(round(q, 1), collapse = " / "),
          "  mean", round(mean(x, na.rm = TRUE), 2), "\n")
    } else if (is.logical(x)) {
      cat("   TRUE:", sum(x, na.rm = TRUE), sprintf("(%.1f%%)", 100 * mean(x, na.rm = TRUE)), "\n")
    } else if (inherits(x, "Date")) {
      cat("   range:", format(range(x, na.rm = TRUE)), "\n")
    } else {
      tb <- sort(table(x, useNA = "no"), decreasing = TRUE)
      top <- head(tb, 8)
      cat("   top:", paste0(names(top), " (", top, ")", collapse = ", "), "\n")
    }
    # success rate by variable (members table only)
    if (!is.null(target) && v != target && n_distinct(x) <= 12) {
      s <- df |> group_by(.data[[v]]) |>
        summarise(n = n(), success = round(100 * mean(.data[[target]]), 1), .groups = "drop")
      cat("   success % by level:", paste0(s[[1]], "=", s$success, "% (n=", s$n, ")", collapse = "; "), "\n")
    }
  }
}

sink("results/describe.txt")
describe(members, "MEMBERS", target = "success")
describe(expeditions, "EXPEDITIONS")
describe(peaks, "PEAKS")

cat("\n\n========== CHECKS ==========\n")
cat("Duplicate member rows (expedition_id + member_id):",
    sum(duplicated(members[c("expedition_id", "member_id")])), "\n")
cat("Members whose expedition is missing from expeditions.csv:",
    sum(!members$expedition_id %in% expeditions$expedition_id), "\n")
cat("Members whose peak is missing from peaks.csv:", sum(!members$peak_id %in% peaks$peak_id), "\n")
cat("Expeditions per peak, top 10:\n"); print(head(sort(table(members$peak_name), decreasing = TRUE), 10))
cat("\nSuccess but highpoint below peak height - 100 m:",
    members |> left_join(peaks[c("peak_id", "height_metres")], by = "peak_id") |>
      filter(success, highpoint_metres < height_metres - 100) |> nrow(), "\n")
cat("Age < 10 or > 85:", sum(members$age < 10 | members$age > 85, na.rm = TRUE), "\n")
cat("\nSuccess by decade (%):\n")
print(members |> mutate(decade = 10 * (year %/% 10)) |> group_by(decade) |>
        summarise(n = n(), success = round(100 * mean(success), 1), oxygen = round(100 * mean(oxygen_used), 1)))
cat("\nSuccess by age band (%):\n")
print(members |> mutate(age_band = cut(age, c(0, 20, 30, 40, 50, 60, 100))) |> group_by(age_band) |>
        summarise(n = n(), success = round(100 * mean(success), 1)))
cat("\nSuccess by hired x oxygen (%):\n")
print(members |> group_by(hired, oxygen_used) |> summarise(n = n(), success = round(100 * mean(success), 1), .groups = "drop"))
cat("\nSuccess by peak height band (%):\n")
print(members |> left_join(peaks[c("peak_id", "height_metres")], by = "peak_id") |>
        mutate(h = cut(height_metres, c(5000, 6500, 7000, 7500, 8000, 9000))) |> group_by(h) |>
        summarise(n = n(), success = round(100 * mean(success), 1), oxygen = round(100 * mean(oxygen_used), 1)))
cat("\nSuccess by expedition_role, top 12 (%):\n")
print(members |> count(expedition_role, success) |> group_by(expedition_role) |>
        mutate(n = sum(n)) |> filter(success) |> arrange(desc(n)) |> head(12))
cat("\nTermination reasons (expeditions):\n")
print(sort(table(expeditions$termination_reason), decreasing = TRUE))
sink()
cat("written results/describe.txt\n")
