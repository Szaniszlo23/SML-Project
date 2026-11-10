# 02_prepare.R -- build the modelling table (one row per non-hired climber) + overview figure
# Run from the himalaya/ folder after 01_describe.R:  Rscript 02_prepare.R
suppressPackageStartupMessages({ library(readr); library(dplyr); library(ggplot2); library(patchwork) })

members <- read_csv("data/members.csv", show_col_types = FALSE)
exped   <- read_csv("data/expeditions.csv", show_col_types = FALSE)
peaks   <- read_csv("data/peaks.csv", show_col_types = FALSE)

# --- fixes found in 01_describe.R -------------------------------------------
peaks$first_ascent_year[peaks$first_ascent_year < 1900] <- NA   # Sharphu II: "201" is a typo
# expedition_id KANG10101 is used twice (1910 and 2010) -> join on id AND year
exped_feat <- exped |>
  transmute(expedition_id, year, termination_reason,
            team_size   = members,              # non-hired members on the permit
            hired_staff,                        # hired staff above base camp
            exp_oxygen  = oxygen_used,          # LEAKY: only recorded if oxygen was actually used (high up)
            commercial  = !is.na(trekking_agency))

d <- members |>
  left_join(exped_feat, by = c("expedition_id", "year")) |>
  left_join(select(peaks, peak_id, height_metres, first_ascent_year), by = "peak_id")

# --- population: non-hired climbers (clients + leaders) who actually tried -----
# hired Sherpa staff are excluded: they summit 66% of the time as part of their job,
# so "hired" would be a trivial predictor and blur the question "what are MY odds?"
climbing_roles <- c("Climber", "Leader", "Deputy Leader", "Co-Leader", "Climbing Leader", "Climber (torch)")
not_attempted  <- c("Did not attempt climb", "Did not reach base camp", "Attempt rumoured", "Unknown")
n0 <- nrow(d)
d <- d |>
  filter(year >= 1950, !hired,
         expedition_role %in% climbing_roles,
         !termination_reason %in% not_attempted,
         !is.na(height_metres), !is.na(sex), season != "Unknown")
cat("Rows kept:", nrow(d), "of", n0, "\n")

# --- features: only things known BEFORE the climb -----------------------------
top_cit   <- names(sort(table(d$citizenship), decreasing = TRUE))[1:15]
top_peaks <- names(sort(table(d$peak_name), decreasing = TRUE))[1:10]
d <- d |>
  mutate(
    success     = factor(ifelse(success, "yes", "no"), levels = c("no", "yes")),
    season      = factor(season),
    sex         = factor(sex),
    age_missing = is.na(age),
    age         = ifelse(is.na(age), median(age, na.rm = TRUE), age),
    role        = factor(ifelse(grepl("Leader", expedition_role), "leader", "climber")),
    citizenship = factor(ifelse(citizenship %in% top_cit, citizenship, "Other")),
    peak        = factor(ifelse(peak_name %in% top_peaks, peak_name, "Other")),
    peak_height = height_metres,
    eight_thousander = peak_height >= 8000,
    climbed_before   = !is.na(first_ascent_year) & first_ascent_year < year,
    staff_ratio = hired_staff / pmax(team_size, 1),
    across(c(solo, exp_oxygen, oxygen_used, commercial, age_missing, eight_thousander, climbed_before), as.integer)
  ) |>
  select(expedition_id, success, year, season, sex, age, age_missing, citizenship, role, solo,
         peak, peak_height, eight_thousander, climbed_before, team_size, hired_staff, staff_ratio, commercial,
         exp_oxygen, oxygen_member = oxygen_used)   # last two: LEAKY, only for the leakage comparison
saveRDS(d, "data/model_data.rds")
cat("Success rate:", round(100 * mean(d$success == "yes"), 1), "%   expeditions:",
    n_distinct(d$expedition_id), "\n")

# --- overview figure ------------------------------------------------------------
dir.create("figures", showWarnings = FALSE)
th <- theme_minimal(base_size = 14); blue <- "#3b6ea8"
rate <- function(df, g) df |> group_by({{ g }}) |> summarise(n = n(), success = mean(success == "yes"), .groups = "drop")
pct  <- scale_y_continuous(labels = scales::percent)

p1 <- d |> rate(year) |> filter(n >= 50) |> ggplot(aes(year, success)) + geom_line() + geom_point(size = .8) +
  pct + labs(title = "Success rate by year", x = NULL, y = NULL) + th
p2 <- d |> mutate(age_band = cut(age, c(0, 25, 30, 35, 40, 45, 50, 55, 60, 90))) |> rate(age_band) |>
  ggplot(aes(age_band, success)) + geom_col(fill = blue) + pct +
  labs(title = "Success by age", x = NULL, y = NULL) + th + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3 <- d |> mutate(staff = cut(staff_ratio, c(-Inf, 0, .5, 1, 2, Inf), labels = c("0", "0-0.5", "0.5-1", "1-2", "> 2"))) |>
  rate(staff) |> ggplot(aes(staff, success)) + geom_col(fill = blue) + pct +
  labs(title = "Success by hired staff per climber", x = NULL, y = NULL) + th
p4 <- d |> rate(peak) |> ggplot(aes(reorder(peak, success), success)) + geom_col(fill = blue) + coord_flip() +
  pct + labs(title = "Success by peak", x = NULL, y = NULL) + th
ggsave("figures/eda_overview.png", (p1 + p2) / (p3 + p4), width = 11, height = 8, dpi = 130)
cat("written data/model_data.rds and figures/eda_overview.png\n")
