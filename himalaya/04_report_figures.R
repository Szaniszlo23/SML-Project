# 04_report_figures.R -- extra figures for the data report
# Run from the himalaya/ folder:  Rscript 04_report_figures.R
suppressPackageStartupMessages({ library(readr); library(dplyr); library(ggplot2); library(patchwork) })
members <- read_csv("data/members.csv", show_col_types = FALSE)
exped   <- read_csv("data/expeditions.csv", show_col_types = FALSE)
peaks   <- read_csv("data/peaks.csv", show_col_types = FALSE)
m <- members |> left_join(select(peaks, peak_id, height_metres), by = "peak_id")
th <- theme_minimal(base_size = 10); blue <- "#3b6ea8"

# 1. distributions of the numeric variables
h <- function(df, x, title, bins = 40) ggplot(df, aes({{ x }})) + geom_histogram(bins = bins, fill = blue) +
  labs(title = title, x = NULL, y = NULL) + th
p <- h(m, year, "members: year") + h(filter(m, !is.na(age)), age, "members: age") +
  h(m, height_metres, "members: peak height (m)") + h(filter(m, !is.na(highpoint_metres)), highpoint_metres, "members: personal highpoint (m)") +
  h(filter(exped, members <= 40), members, "expeditions: team size (<= 40)", 40) +
  h(filter(exped, hired_staff <= 40), hired_staff, "expeditions: hired staff (<= 40)", 40) +
  plot_layout(ncol = 3)
ggsave("figures/rep_distributions.png", p, width = 10, height = 5.2, dpi = 140)

# 2. oxygen leakage: among climbers who FAILED on 8000 m peaks, oxygen use depends on how high they got
lk <- m |> filter(!success, height_metres > 8000, !is.na(highpoint_metres)) |>
  mutate(hp = cut(highpoint_metres, c(0, 6500, 7000, 7500, 8000, 9000),
                  labels = c("< 6500", "6500-7000", "7000-7500", "7500-8000", "> 8000"))) |>
  group_by(hp) |> summarise(n = n(), oxygen = mean(oxygen_used))
p2 <- ggplot(lk, aes(hp, oxygen)) + geom_col(fill = blue) +
  geom_text(aes(label = scales::percent(oxygen, 1)), vjust = -.4, size = 3.2) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .8)) +
  labs(title = "Failed climbers on 8000 m peaks: share who used oxygen", x = "highest point reached (m)", y = NULL) + th
ggsave("figures/rep_oxygen_leakage.png", p2, width = 6.5, height = 3.4, dpi = 140)

# 3. clustering: share of members who succeeded, per expedition
cl <- members |> group_by(expedition_id) |> filter(n() > 1) |> summarise(s = mean(success))
p3 <- ggplot(cl, aes(s)) + geom_histogram(breaks = seq(-0.05, 1.05, .1), fill = blue) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Share of members who summited, per expedition", x = NULL, y = "expeditions") + th
ggsave("figures/rep_clustering.png", p3, width = 6.5, height = 3.2, dpi = 140)

# 4. success by season and by sex / hired (categorical overview)
rate <- function(df, g) df |> group_by({{ g }}) |> summarise(n = n(), success = mean(success), .groups = "drop")
c1 <- m |> filter(season != "Unknown") |> rate(season) |> ggplot(aes(reorder(season, -success), success)) +
  geom_col(fill = blue) + labs(title = "season", x = NULL, y = NULL)
c2 <- m |> filter(!is.na(sex)) |> rate(sex) |> ggplot(aes(sex, success)) + geom_col(fill = blue) + labs(title = "sex", x = NULL, y = NULL)
c3 <- m |> mutate(hired = ifelse(hired, "hired", "not hired")) |> rate(hired) |> ggplot(aes(hired, success)) +
  geom_col(fill = blue) + labs(title = "hired", x = NULL, y = NULL)
c4 <- m |> mutate(ox = ifelse(oxygen_used, "used", "not used")) |> rate(ox) |> ggplot(aes(ox, success)) +
  geom_col(fill = blue) + labs(title = "oxygen_used (member)", x = NULL, y = NULL)
p4 <- (c1 + c2 + c3 + c4 + plot_layout(ncol = 4, widths = c(2, 1, 1, 1))) &
  scale_y_continuous(labels = scales::percent, limits = c(0, .8)) & th
ggsave("figures/rep_categorical.png", p4, width = 10, height = 3, dpi = 140)
cat("figures written\n")
