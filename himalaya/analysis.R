# =====================================================================================
# Himalayan expeditions: how much of summit success is decided before the climb starts?
#
# Main question : Using only what is known before an expedition leaves, how well can we
#                 predict whether a climber reaches the summit, and what drives the odds?
# Data          : The Himalayan Database (TidyTuesday 2020-09-22): members, expeditions, peaks
# Methods       : elastic-net logistic regression, pruned tree, random forest, gradient boosting
#
# Run from the himalaya/ folder:  Rscript analysis.R      (about 15 minutes on 8 cores)
# Output: figures/fig*.png (used by slides/presentation.tex) and results/*.txt, *.csv
#
#   1. Load the data              5. Fit and tune the four models
#   2. Population and features    6. Assessment on the test set
#   3. Exploratory analysis       7. Insights from the final model
#   4. Train / test split         8. How far can we trust it? (leakage, time split)
# =====================================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(glmnet); library(rpart); library(rpart.plot); library(ranger); library(gbm); library(parallel)
})
set.seed(2026)
dir.create("figures", showWarnings = FALSE); dir.create("results", showWarnings = FALSE)
cores <- if (.Platform$OS.type == "windows") 1 else max(1, detectCores() - 1)
sink("results/results.txt", split = TRUE)          # everything printed below is also saved

# --- plot style and small helpers -----------------------------------------------------
blue <- "#2a78d6"; orange <- "#eb6834"; aqua <- "#1baf7a"; grey <- "#8a8a85"
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 12),
                  plot.title.position = "plot", plot.subtitle = element_text(colour = "grey35"),
                  strip.text = element_text(face = "bold", hjust = 0)))
pct      <- function(x) scales::percent(x, accuracy = 1)
save_fig <- function(name, plot, w, h) ggsave(file.path("figures", name), plot, width = w, height = h, dpi = 200, bg = "white")
rate     <- function(df, g) df |> group_by({{ g }}) |> summarise(n = n(), success = mean(success == "yes"), .groups = "drop")

# metrics: log loss and Brier score judge the probabilities, AUC the ranking
logloss <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
auc     <- function(p, y) { n1 <- sum(y); (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * (length(y) - n1)) }
metrics <- function(p, y) c(log_loss = logloss(p, y), brier = mean((p - y)^2), auc = auc(p, y),
                            accuracy = mean((p > 0.5) == y))


# =====================================================================================
# 1. Load the data (downloaded once, then read from data/)
# =====================================================================================
tt <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-09-22/"
get_table <- function(name) {
  file <- file.path("data", paste0(name, ".csv"))
  if (!file.exists(file)) download.file(paste0(tt, name, ".csv"), file)
  read_csv(file, show_col_types = FALSE)
}
members <- get_table("members"); expeditions <- get_table("expeditions"); peaks <- get_table("peaks")
cat("Raw tables: members", nrow(members), "| expeditions", nrow(expeditions), "| peaks", nrow(peaks), "\n")


# =====================================================================================
# 2. Population and features
# =====================================================================================
# Two data errors: first-ascent year "201" (typo), and expedition id KANG10101 is used
# for two expeditions (1910 and 2010), so expeditions are joined on id AND year.
peaks$first_ascent_year[peaks$first_ascent_year < 1900] <- NA
raw <- members |>
  left_join(transmute(expeditions, expedition_id, year, termination_reason, team_size = members, hired_staff,
                      exp_oxygen = oxygen_used, commercial = !is.na(trekking_agency)),
            by = c("expedition_id", "year")) |>
  left_join(select(peaks, peak_id, peak_height = height_metres, first_ascent_year), by = "peak_id")

# Population: paying climbers and leaders who actually tried to climb, 1950-2019.
# Hired staff are left out: reaching the summit is their job (see the rate printed below).
climbing_roles <- c("Climber", "Leader", "Deputy Leader", "Co-Leader", "Climbing Leader", "Climber (torch)")
not_attempted  <- c("Did not attempt climb", "Did not reach base camp", "Attempt rumoured", "Unknown")
steps <- list("all member records"          = raw,
              "1950 or later"               = (s1 <- filter(raw, year >= 1950)),
              "not hired staff"             = (s2 <- filter(s1, !hired)),
              "climbing role"               = (s3 <- filter(s2, expedition_role %in% climbing_roles)),
              "expedition attempted climb"  = (s4 <- filter(s3, !termination_reason %in% not_attempted)),
              "peak, sex and season known"  = (s5 <- filter(s4, !is.na(peak_height), !is.na(sex), season != "Unknown")))
cat("\n=== Population ===\n"); print(data.frame(rows = sapply(steps, nrow)))
cat("Summit rate of hired staff:", round(100 * mean(s1$success[s1$hired]), 1), "%\n")

# Features: only things known BEFORE the climb. Everything that happens on the mountain
# (highest point, death, injury, oxygen use, why the expedition ended) would leak the outcome.
# The two oxygen variables are kept aside for the leakage check in section 8.
d <- s5 |>
  transmute(expedition_id, peak_name, highpoint_metres,
            success     = factor(ifelse(success, "yes", "no"), levels = c("no", "yes")),
            year, season = factor(season), sex = factor(sex), age, citizenship,
            role        = factor(ifelse(grepl("Leader", expedition_role), "leader", "climber")),
            solo        = as.integer(solo),
            peak_height,
            eight_thousander = as.integer(peak_height >= 8000),
            climbed_before   = as.integer(!is.na(first_ascent_year) & first_ascent_year < year),
            team_size, hired_staff,
            staff_ratio = hired_staff / pmax(team_size, 1),       # hired staff per climber
            commercial  = as.integer(commercial),
            exp_oxygen  = as.integer(exp_oxygen), oxygen_member = as.integer(oxygen_used))
base_rate <- mean(d$success == "yes")
cat("\nModelling data:", nrow(d), "climbers |", n_distinct(d$expedition_id), "expeditions |",
    n_distinct(d$peak_name), "peaks | summit rate", round(100 * base_rate, 1), "%\n")


# =====================================================================================
# 3. Exploratory analysis
# =====================================================================================
# 3a. success over time (top) and number of climbers (bottom)
by_year <- rate(d, year)
e1 <- ggplot(filter(by_year, n >= 50), aes(year, success)) +
  geom_hline(yintercept = base_rate, linetype = 2, colour = grey) +
  geom_line(colour = blue, linewidth = 0.8) + geom_point(colour = blue, size = 1.2) +
  annotate("text", x = 2013.5, y = 0.10, hjust = 1, size = 3.3, colour = "grey25",
           label = "2014 Everest avalanche,\n2015 earthquake") +
  annotate("text", x = 1967, y = base_rate + 0.035, size = 3.3, colour = "grey35", label = "overall 34%") +
  scale_y_continuous(labels = pct, limits = c(0, 0.7)) + scale_x_continuous(limits = c(1949, 2020)) +
  labs(title = "Share of climbers who reach the summit, by year", x = NULL, y = NULL)
e2 <- ggplot(by_year, aes(year, n)) + geom_col(fill = grey, width = 0.8) + scale_x_continuous(limits = c(1949, 2020)) +
  labs(title = "Climbers per year", x = NULL, y = NULL)
save_fig("fig01_trend.png", e1 / e2 + plot_layout(heights = c(3, 1)), 8, 4.6)

# 3b. success by where, when, who and how much support
bar <- function(df, title, flip = FALSE) {
  p <- ggplot(df, aes(g, success)) + geom_hline(yintercept = base_rate, linetype = 2, colour = grey) +
    geom_col(fill = blue, width = 0.7) +
    geom_text(aes(label = pct(success)), size = 3, hjust = if (flip) -0.15 else 0.5, vjust = if (flip) 0.5 else -0.5) +
    scale_y_continuous(labels = pct, limits = c(0, 0.6)) + labs(title = title, x = NULL, y = NULL)
  if (flip) p + coord_flip() + theme(panel.grid.major.y = element_blank()) else p + theme(panel.grid.major.x = element_blank())
}
top10 <- names(sort(table(d$peak_name), decreasing = TRUE))[1:10]
g1 <- d |> mutate(g = ifelse(peak_name %in% top10, peak_name, "All other peaks")) |> rate(g) |>
  mutate(g = reorder(g, success)) |> bar("Where: the 10 most climbed peaks", flip = TRUE)
g2 <- d |> mutate(g = factor(season, c("Spring", "Summer", "Autumn", "Winter"))) |> rate(g) |> bar("When: season")
g3 <- d |> filter(!is.na(age)) |>
  mutate(g = cut(age, c(0, 30, 40, 50, 60, 100), labels = c("< 30", "30-39", "40-49", "50-59", "60+"), right = FALSE)) |>
  rate(g) |> bar("Who: age")
g4 <- d |> mutate(g = cut(staff_ratio, c(-Inf, 0, .5, 1, 2, Inf), labels = c("0", "up to 0.5", "0.5-1", "1-2", "> 2"))) |>
  rate(g) |> bar("Support: hired staff per climber")
save_fig("fig02_drivers.png", (g1 | (g2 / g3 / g4)) + plot_layout(widths = c(1.1, 1)), 9, 5)

# 3c. why oxygen use is leakage: among FAILED climbers on 8000 m peaks, oxygen use depends on how high they got
ox <- d |> filter(success == "no", peak_height >= 8000, !is.na(highpoint_metres)) |>
  mutate(g = cut(highpoint_metres, c(0, 6500, 7000, 7500, 8000, 9000),
                 labels = c("< 6500", "6500-7000", "7000-7500", "7500-8000", "> 8000"))) |>
  group_by(g) |> summarise(n = n(), oxygen = mean(oxygen_member))
save_fig("fig03_oxygen_leakage.png",
         ggplot(ox, aes(g, oxygen)) + geom_col(fill = orange, width = 0.7) +
           geom_text(aes(label = pct(oxygen)), vjust = -0.5, size = 3.5) +
           scale_y_continuous(labels = pct, limits = c(0, 0.8)) + theme(panel.grid.major.x = element_blank()) +
           labs(title = "Climbers who failed on 8000 m peaks: share who used oxygen",
                x = "highest point reached (m)", y = NULL), 6, 3.3)

# 3d. climbers of one expedition share their fate -> the data are clustered
cl <- d |> group_by(expedition_id) |> filter(n() > 1) |> summarise(share = mean(success == "yes"))
cat("\nExpeditions with 2+ climbers:", nrow(cl), "| nobody summits:", round(100 * mean(cl$share == 0)),
    "% | everybody summits:", round(100 * mean(cl$share == 1)), "%\n")
save_fig("fig04_clustering.png",
         ggplot(cl, aes(share)) + geom_histogram(breaks = seq(-0.05, 1.05, 0.1), fill = blue, colour = "white") +
           scale_x_continuous(labels = pct, breaks = seq(0, 1, 0.25)) +
           labs(title = "Share of the team that summits, per expedition", x = NULL, y = "expeditions"), 5.5, 3.2)


# =====================================================================================
# 4. Train / test split
# =====================================================================================
# Whole expeditions go to either training (80%) or test (20%), and the 5 CV folds used for
# tuning are also made of whole expeditions. Otherwise team-mates would sit on both sides.
ex       <- unique(d$expedition_id)
test_ex  <- sample(ex, round(0.2 * length(ex)))
is_test  <- d$expedition_id %in% test_ex

# Encodings that are learned from data use the TRAINING set only
age_median <- median(d$age[!is_test], na.rm = TRUE)
top_cit    <- names(sort(table(d$citizenship[!is_test]), decreasing = TRUE))[1:15]
top_peaks  <- names(sort(table(d$peak_name[!is_test]),   decreasing = TRUE))[1:10]
d <- d |> mutate(age_missing = as.integer(is.na(age)),
                 age         = ifelse(is.na(age), age_median, age),
                 citizenship = factor(ifelse(citizenship %in% top_cit, citizenship, "Other")),
                 peak        = factor(ifelse(peak_name %in% top_peaks, peak_name, "Other")))

predictors <- c("year", "season", "sex", "age", "age_missing", "citizenship", "role", "solo", "peak", "peak_height",
                "eight_thousander", "climbed_before", "team_size", "hired_staff", "staff_ratio", "commercial")
train <- d[!is_test, ]; test <- d[is_test, ]
y_tr  <- as.integer(train$success == "yes"); y_te <- as.integer(test$success == "yes")
fold_of_ex <- setNames(sample(rep(1:5, length.out = n_distinct(train$expedition_id))), unique(train$expedition_id))
foldid     <- unname(fold_of_ex[train$expedition_id])
cat("\n=== Split ===\ntrain:", nrow(train), "climbers,", n_distinct(train$expedition_id), "expeditions | test:",
    nrow(test), "climbers,", length(test_ex), "expeditions\n")


# =====================================================================================
# 5. Fit and tune the four models (test set not touched)
# =====================================================================================
f_all <- reformulate(predictors, "success")
pred  <- list(baseline = rep(mean(y_tr), nrow(test)))       # always predict the training summit rate

# --- 5.1 Elastic-net logistic regression: alpha and lambda by grouped CV ----------------
# Splines let the linear model bend in year, age and height. The spline basis is built on
# the training data and re-used for new data (terms object), so both share the same knots.
f_glm <- ~ splines::ns(year, 4) + splines::ns(age, 4) + splines::ns(peak_height, 3) + season + sex + age_missing +
  citizenship + role + solo + peak + eight_thousander + climbed_before + log1p(team_size) + log1p(hired_staff) +
  log1p(staff_ratio) + commercial
design  <- function(terms, data) model.matrix(terms, model.frame(terms, data))[, -1]
glm_tt  <- terms(model.frame(f_glm, train))
X_tr    <- design(glm_tt, train); X_te <- design(glm_tt, test)
alphas  <- c(0, 0.25, 0.5, 0.75, 1)
cv_list <- lapply(alphas, function(a) cv.glmnet(X_tr, y_tr, family = "binomial", alpha = a, foldid = foldid))
tune_en <- data.frame(alpha = alphas, cv_deviance = sapply(cv_list, function(cv) min(cv$cvm)))
best_alpha <- alphas[which.min(tune_en$cv_deviance)]
cv_en   <- cv_list[[which.min(tune_en$cv_deviance)]]
pred$elastic_net <- as.numeric(predict(cv_en, X_te, s = "lambda.1se", type = "response"))
cat("\n=== Elastic net ===\n"); print(tune_en)
cat("chosen alpha", best_alpha, "| lambda (1-SE rule)", signif(cv_en$lambda.1se, 3), "| non-zero coefficients",
    sum(coef(cv_en, s = "lambda.1se") != 0) - 1, "of", ncol(X_tr), "\n")

# --- 5.2 Classification tree: grow large, prune back with the 1-SE rule --------------------
tree_full <- rpart(f_all, train, method = "class", control = rpart.control(cp = 0.0003, minbucket = 50, xval = foldid))
cpt    <- as.data.frame(tree_full$cptable)
i_min  <- which.min(cpt$xerror)
cp_1se <- cpt$CP[which(cpt$xerror <= cpt$xerror[i_min] + cpt$xstd[i_min])[1]]
tree   <- prune(tree_full, cp = cp_1se)
n_leaves <- sum(tree$frame$var == "<leaf>")
pred$tree <- predict(tree, test)[, "yes"]
cat("\n=== Tree ===\n1-SE cp", signif(cp_1se, 3), "| leaves", n_leaves, "(minimum CV error at",
    cpt$nsplit[i_min] + 1, "leaves)\n")

# --- 5.3 Random forest: mtry x minimum node size by grouped CV ---------------------------
# The out-of-bag (OOB) error is recorded too, to show why it must not be used here: OOB
# leaves out single climbers while their team-mates stay in the bag.
fit_rf <- function(data, mtry, node, trees = 300, ...)
  ranger(f_all, data, num.trees = trees, mtry = mtry, min.node.size = node, probability = TRUE, seed = 1,
         num.threads = cores, ...)
tune_rf <- expand.grid(mtry = c(2, 4, 7, 11), node_size = c(25, 100, 400, 1000))
tune_rf[c("cv_logloss", "oob_logloss")] <- t(sapply(seq_len(nrow(tune_rf)), function(g) {
  rowMeans(sapply(1:5, function(k) {
    fit <- fit_rf(train[foldid != k, ], tune_rf$mtry[g], tune_rf$node_size[g])
    c(logloss(predict(fit, train[foldid == k, ])$predictions[, "yes"], y_tr[foldid == k]),
      logloss(fit$predictions[, "yes"], y_tr[foldid != k]))
  }))
}))
best_rf <- tune_rf[which.min(tune_rf$cv_logloss), ]
rf <- fit_rf(train, best_rf$mtry, best_rf$node_size, trees = 500, importance = "permutation")
pred$random_forest <- predict(rf, test)$predictions[, "yes"]
cat("\n=== Random forest ===\n"); print(tune_rf[order(tune_rf$cv_logloss), ], row.names = FALSE)

# --- 5.4 Gradient boosting: tree depth x shrinkage x number of trees by grouped CV ---------
fit_gbm <- function(data, depth, shrinkage, trees) {
  set.seed(1)
  gbm(y ~ ., "bernoulli", data = data, n.trees = trees, interaction.depth = depth, shrinkage = shrinkage,
      n.minobsinnode = 20, bag.fraction = 0.5)
}
gbm_train <- cbind(train[predictors], y = y_tr)
jobs <- expand.grid(fold = 1:5, depth = c(2, 4, 6), shrinkage = c(0.02, 0.1))
jobs$max_trees <- ifelse(jobs$shrinkage == 0.02, 3000, 1500)       # slow learning needs more trees
cv_gbm <- bind_rows(mclapply(seq_len(nrow(jobs)), function(j) {
  k <- jobs$fold[j]; grid <- seq(50, jobs$max_trees[j], 50)
  fit <- fit_gbm(gbm_train[foldid != k, ], jobs$depth[j], jobs$shrinkage[j], jobs$max_trees[j])
  P <- predict(fit, gbm_train[foldid == k, ], n.trees = grid, type = "response")
  data.frame(fold = k, depth = jobs$depth[j], shrinkage = jobs$shrinkage[j], trees = grid,
             logloss = apply(P, 2, logloss, y = y_tr[foldid == k]))
}, mc.cores = cores))
tune_gbm <- cv_gbm |> group_by(depth, shrinkage, trees) |> summarise(cv_logloss = mean(logloss), .groups = "drop")
best_gbm <- tune_gbm[which.min(tune_gbm$cv_logloss), ]
cat("\n=== Boosting (best number of trees per setting) ===\n")
print(as.data.frame(tune_gbm |> group_by(depth, shrinkage) |> slice_min(cv_logloss, n = 1) |> arrange(cv_logloss)),
      row.names = FALSE)
gb <- fit_gbm(gbm_train, best_gbm$depth, best_gbm$shrinkage, best_gbm$trees)
predict_gb <- function(fit, data) predict(fit, data, n.trees = fit$n.trees, type = "response")
pred$boosting <- predict_gb(gb, test)

write.csv(tune_en, "results/tuning_elastic_net.csv", row.names = FALSE)
write.csv(tune_rf, "results/tuning_random_forest.csv", row.names = FALSE)
write.csv(tune_gbm, "results/tuning_boosting.csv", row.names = FALSE)

# --- 5.5 Tuning figures ------------------------------------------------------------------
t1 <- bind_rows(lapply(seq_along(alphas), function(i)
  data.frame(alpha = factor(alphas[i]), loglambda = log(cv_list[[i]]$lambda), dev = cv_list[[i]]$cvm))) |>
  ggplot(aes(loglambda, dev, colour = alpha)) + geom_line(linewidth = 0.7) +
  geom_vline(xintercept = log(cv_en$lambda.1se), linetype = 2) +
  scale_colour_manual(values = colorRampPalette(c("#a9c9f0", "#0f3d7a"))(5)) +
  coord_cartesian(ylim = c(min(tune_en$cv_deviance), min(tune_en$cv_deviance) + 0.1)) +
  labs(title = "Elastic net: CV deviance", x = "log(lambda)", y = NULL, colour = "alpha")
t2 <- ggplot(cpt, aes(nsplit + 1, xerror)) +
  geom_errorbar(aes(ymin = xerror - xstd, ymax = xerror + xstd), width = 0, colour = grey) +
  geom_line(colour = blue) + geom_point(colour = blue, size = 0.9) +
  geom_vline(xintercept = n_leaves, linetype = 2) + scale_x_log10() +
  labs(title = "Tree: CV error by size", x = "leaves (log scale)", y = NULL)
t3 <- tune_rf |> pivot_longer(c(cv_logloss, oob_logloss), names_to = "type", values_to = "ll") |>
  mutate(type = ifelse(type == "cv_logloss", "CV by expedition", "out-of-bag")) |>
  ggplot(aes(factor(node_size), ll, colour = factor(mtry), linetype = type, group = interaction(mtry, type))) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.3) +
  scale_colour_manual(values = colorRampPalette(c("#a9c9f0", "#0f3d7a"))(4)) +
  labs(title = "Random forest: CV vs out-of-bag log loss", x = "minimum node size", y = NULL, colour = "mtry", linetype = NULL)
t4 <- ggplot(tune_gbm, aes(trees, cv_logloss, colour = factor(depth), linetype = factor(shrinkage))) +
  geom_line(linewidth = 0.7) + scale_colour_manual(values = c(blue, orange, aqua)) +
  coord_cartesian(ylim = c(best_gbm$cv_logloss, best_gbm$cv_logloss + 0.03)) +
  labs(title = "Boosting: CV log loss", x = "number of trees", y = NULL, colour = "depth", linetype = "shrinkage")
save_fig("fig05_tuning_simple.png", t1 + t2, 9, 3.4)
save_fig("fig06_tuning_flexible.png", t3 + t4, 9, 3.4)


# =====================================================================================
# 6. Assessment on the test set (used once, after all tuning)
# =====================================================================================
model_names <- c(baseline = "Summit rate only", elastic_net = "Elastic net", tree = "Pruned tree",
                 random_forest = "Random forest", boosting = "Boosting")
test_metrics <- t(sapply(pred, metrics, y = y_te))
cat("\n=== Test set ===\n"); print(round(test_metrics, 4))

# Uncertainty: bootstrap that resamples whole test expeditions (1000 times)
rows_by_ex <- split(seq_len(nrow(test)), test$expedition_id)
boot <- replicate(1000, {
  i <- unlist(rows_by_ex[sample(length(rows_by_ex), replace = TRUE)], use.names = FALSE)
  sapply(pred, function(p) c(logloss(p[i], y_te[i]), auc(p[i], y_te[i])))
})                                                                   # 2 metrics x 5 models x 1000
ci <- function(x) quantile(x, c(0.025, 0.975))
boot_ci <- data.frame(model = names(pred), log_loss = test_metrics[, "log_loss"], t(apply(boot[1, , ], 1, ci)),
                      auc = test_metrics[, "auc"], t(apply(boot[2, , ], 1, ci)))
names(boot_ci) <- c("model", "log_loss", "ll_lo", "ll_hi", "auc", "auc_lo", "auc_hi")
cat("\n95% bootstrap intervals:\n"); print(format(boot_ci, digits = 3), row.names = FALSE)
cat("\nLog-loss advantage of boosting over each model (95% interval):\n")
for (m in setdiff(names(pred), "boosting"))
  cat(sprintf("  vs %-14s %.4f  [%.4f, %.4f]\n", m, test_metrics[m, 1] - test_metrics["boosting", 1],
              ci(boot[1, m, ] - boot[1, "boosting", ])[1], ci(boot[1, m, ] - boot[1, "boosting", ])[2]))

# 6a. model comparison with intervals
cmp <- boot_ci |> mutate(model = factor(model_names[model], rev(model_names)), final = model == "Boosting")
r1 <- ggplot(cmp, aes(log_loss, model, colour = final)) + geom_pointrange(aes(xmin = ll_lo, xmax = ll_hi), size = 0.5) +
  geom_text(aes(label = sprintf("%.3f", log_loss)), vjust = -1, size = 3.2, colour = "grey20") +
  labs(title = "Log loss (lower is better)", x = NULL, y = NULL)
r2 <- ggplot(filter(cmp, model != "Summit rate only"), aes(auc, model, colour = final)) +
  geom_pointrange(aes(xmin = auc_lo, xmax = auc_hi), size = 0.5) +
  geom_text(aes(label = sprintf("%.3f", auc)), vjust = -1, size = 3.2, colour = "grey20") +
  labs(title = "AUC (higher is better)", x = NULL, y = NULL)
save_fig("fig07_comparison.png", (r1 / r2 + plot_layout(heights = c(5, 4))) &
           scale_colour_manual(values = c(grey, blue), guide = "none") & theme(panel.grid.major.y = element_blank()), 4.6, 4.6)

# 6b. calibration: predicted probability vs observed summit rate in test deciles
cal <- bind_rows(lapply(names(pred)[-1], function(m) data.frame(model = model_names[[m]], p = pred[[m]], y = y_te))) |>
  mutate(model = factor(model, model_names[-1])) |> group_by(model) |> mutate(bin = ntile(p, 10)) |>
  group_by(model, bin) |> summarise(predicted = mean(p), observed = mean(y), .groups = "drop")
save_fig("fig08_calibration.png",
         ggplot(cal, aes(predicted, observed)) + geom_abline(linetype = 2, colour = grey) +
           geom_line(colour = blue) + geom_point(colour = blue, size = 1.5) + facet_wrap(~ model, nrow = 1) +
           scale_x_continuous(labels = pct, limits = c(0, 0.85)) + scale_y_continuous(labels = pct, limits = c(0, 0.85)) +
           labs(title = "Calibration on the test set (deciles of predicted probability)",
                x = "predicted probability", y = "observed summit rate"), 9, 2.9)

# 6c. the answer to the main question: how far do pre-climb facts move the odds?
dec <- data.frame(p = pred$boosting, y = y_te) |> mutate(decile = ntile(p, 10)) |> group_by(decile) |>
  summarise(predicted = mean(p), observed = mean(y), n = n())
cat("\nBoosting, test climbers sorted into tenths by predicted probability:\n"); print(as.data.frame(round(dec, 3)), row.names = FALSE)
save_fig("fig09_deciles.png",
         ggplot(dec, aes(factor(decile), observed)) + geom_col(fill = blue, width = 0.7) +
           geom_hline(yintercept = mean(y_te), linetype = 2, colour = grey) +
           geom_point(aes(y = predicted), shape = 21, fill = "white", colour = "grey15", size = 2.6, stroke = 0.9) +
           geom_text(aes(y = pmax(observed, predicted), label = pct(observed)), vjust = -1.3, size = 3.3) +
           annotate("text", x = 0.6, y = mean(y_te) + 0.035, hjust = 0, size = 3.2, colour = "grey35", label = "test average") +
           scale_y_continuous(labels = pct, limits = c(0, 0.9)) + theme(panel.grid.major.x = element_blank()) +
           labs(title = "Observed summit rate of test climbers, by tenth of predicted probability",
                subtitle = "Bars: what happened.  Circles: what the boosting model predicted before the climb.",
                x = "tenth of climbers, from lowest to highest predicted probability", y = NULL), 7.5, 3.8)
write.csv(round(test_metrics, 4), "results/test_metrics.csv")


# =====================================================================================
# 7. Insights from the final model (boosting)
# =====================================================================================
# 7a. Which kind of information matters? Shuffle one block of variables in the test set
#     and measure how much the log loss gets worse (average of 5 shuffles).
blocks <- list("When: year"                            = "year",
               "Where: peak and its height"            = c("peak", "peak_height", "eight_thousander", "climbed_before"),
               "Support: hired staff, commercial"      = c("hired_staff", "staff_ratio", "commercial"),
               "When: season"                          = "season",
               "Team size"                             = c("team_size", "solo"),
               "Who: age, sex, citizenship, role"      = c("age", "age_missing", "sex", "citizenship", "role"))
perm_loss <- function(vars) { set.seed(1); mean(replicate(5, {
  shuffled <- test; shuffled[vars] <- test[sample(nrow(test)), vars]
  logloss(predict_gb(gb, shuffled), y_te)
})) - logloss(pred$boosting, y_te) }
imp_block <- data.frame(block = names(blocks), increase = unlist(mclapply(blocks, perm_loss, mc.cores = cores)))
imp_var   <- data.frame(variable = predictors, increase = unlist(mclapply(predictors, perm_loss, mc.cores = cores)))
cat("\n=== Permutation importance of the boosting model (increase in test log loss) ===\n")
print(imp_block[order(-imp_block$increase), ], row.names = FALSE, digits = 3)
print(imp_var[order(-imp_var$increase), ], row.names = FALSE, digits = 3)
save_fig("fig10_importance.png",
         ggplot(imp_block, aes(increase, reorder(block, increase))) + geom_col(fill = blue, width = 0.65) +
           geom_text(aes(label = sprintf("+%.3f", increase)), hjust = -0.15, size = 3.3) +
           scale_x_continuous(expand = expansion(mult = c(0, 0.2))) + theme(panel.grid.major.y = element_blank()) +
           labs(title = "What the model needs most", subtitle = "Increase in test log loss when a block is shuffled",
                x = NULL, y = NULL), 5, 3.3)

# 7b. Partial dependence: predicted summit probability when one variable changes
pd <- function(v) { g <- plot(gb, i.var = v, n.trees = gb$n.trees, return.grid = TRUE, type = "response")
                    data.frame(var = v, x = as.character(g[[1]]), p = g$y) }
pd_num <- bind_rows(lapply(c("year", "age", "staff_ratio"), pd)) |> mutate(x = as.numeric(x)) |>
  filter((var == "year" & x >= 1960) | (var == "age" & x >= 18 & x <= 70) | (var == "staff_ratio" & x <= 2)) |>
  mutate(var = factor(var, c("year", "staff_ratio", "age"), c("Year", "Hired staff per climber", "Age")))
p1 <- ggplot(pd_num, aes(x, p)) + geom_line(colour = blue, linewidth = 0.9) + facet_wrap(~ var, scales = "free_x") +
  scale_y_continuous(labels = pct, limits = c(0.1, 0.55)) + labs(x = NULL, y = "predicted summit probability") +
  theme(panel.spacing = unit(1.3, "lines"))
pd_cat <- bind_rows(lapply(c("peak", "season"), pd)) |> mutate(var = factor(var, c("peak", "season"), c("Peak", "Season")))
p2 <- ggplot(pd_cat, aes(p, reorder(x, p))) + geom_segment(aes(x = 0.1, xend = p, yend = reorder(x, p)), colour = grey, linewidth = 0.4) +
  geom_point(colour = blue, size = 2.4) + facet_wrap(~ var, scales = "free_y") +
  scale_x_continuous(labels = pct, limits = c(0.1, 0.55)) + theme(panel.grid.major.y = element_blank()) +
  labs(x = "predicted summit probability", y = NULL)
save_fig("fig11_partial_dependence.png", p1 / p2 + plot_layout(heights = c(1, 1.15)), 8, 5)

# 7c. The first splits of the pruned tree as a readable rule book
tree_top <- prune(tree, cp = tree$cptable[max(which(tree$cptable[, "nsplit"] <= 4)), "CP"])
tree_top$frame$var <- sub("staff_ratio", "staff per climber", tree_top$frame$var)   # readable label
png("figures/fig12_tree.png", width = 1800, height = 900, res = 200)
rpart.plot(tree_top, type = 2, extra = 107, box.palette = "Blues", roundint = FALSE, tweak = 1.15)
dev.off()

# 7d. Gini vs permutation importance in the random forest (Gini favours variables with many split points)
rf_gini <- fit_rf(train, best_rf$mtry, best_rf$node_size, importance = "impurity")
imp_rf <- data.frame(variable = predictors, permutation = importance(rf)[predictors], gini = importance(rf_gini)[predictors]) |>
  mutate(across(-variable, ~ .x / max(.x)))
cat("\n=== Random forest importance (scaled to max = 1) ===\n"); print(imp_rf[order(-imp_rf$permutation), ], row.names = FALSE, digits = 2)
save_fig("fig13_importance_rf.png",
         imp_rf |> pivot_longer(-variable) |>
           ggplot(aes(value, factor(variable, imp_rf$variable[order(imp_rf$permutation)]), fill = name)) +
           geom_col(position = "dodge", width = 0.75) + scale_fill_manual(values = c(gini = grey, permutation = blue)) +
           theme(panel.grid.major.y = element_blank(), legend.position = "top") +
           labs(title = "Random forest importance (largest = 1)", x = NULL, y = NULL, fill = NULL), 5.5, 4.6)

# 7e. One climber, different plans: predictions for a 40-year-old US man in 2019
height_of <- tapply(d$peak_height, d$peak_name, max)
plan <- function(label, ...) {
  x <- modifyList(list(year = 2019, season = "Spring", sex = "M", age = 40, peak = "Everest", team_size = 10,
                       hired_staff = 10, commercial = 1L), list(...))
  data.frame(label, x)
}
plans <- bind_rows(
  plan("Everest in spring, commercial team of 10 with 10 hired staff"),
  plan("... the same climber at age 60", age = 60),
  plan("... the same expedition in 1995", year = 1995),
  plan("... without hired staff or agency", hired_staff = 0, commercial = 0L),
  plan("Cho Oyu in autumn, same team", peak = "Cho Oyu", season = "Autumn"),
  plan("Ama Dablam in autumn, team of 6 with 3 hired staff", peak = "Ama Dablam", season = "Autumn", team_size = 6, hired_staff = 3),
  plan("Annapurna I in spring, team of 6 with 3 hired staff", peak = "Annapurna I", team_size = 6, hired_staff = 3),
  plan("Dhaulagiri I in winter, team of 6 with 3 hired staff", peak = "Dhaulagiri I", season = "Winter", team_size = 6, hired_staff = 3)) |>
  mutate(peak_height = unname(height_of[peak]), eight_thousander = as.integer(peak_height >= 8000), climbed_before = 1L,
         staff_ratio = hired_staff / team_size, age_missing = 0L, solo = 0L,
         season = factor(season, levels(train$season)), sex = factor(sex, levels(train$sex)),
         peak = factor(peak, levels(train$peak)), role = factor("climber", levels(train$role)),
         citizenship = factor("USA", levels(train$citizenship)))
plans$p <- predict_gb(gb, plans)
cat("\n=== One climber, different plans ===\n"); print(transmute(plans, label, predicted = round(p, 2)), row.names = FALSE)
save_fig("fig14_plans.png",
         ggplot(plans, aes(p, factor(label, rev(label)))) + geom_col(fill = blue, width = 0.65) +
           geom_text(aes(label = pct(p)), hjust = -0.2, size = 3.5) +
           scale_x_continuous(labels = pct, limits = c(0, 1)) + theme(panel.grid.major.y = element_blank()) +
           labs(title = "Predicted summit probability: a 40-year-old US man in 2019", x = NULL, y = NULL), 8, 3.3)


# =====================================================================================
# 8. How far can we trust it?
# =====================================================================================
# 8a. Leakage check: the same boosting model plus the oxygen variables. Oxygen is only
#     recorded when it was used, i.e. high on the mountain, so the gain is not real.
leak_vars <- list(exp_oxygen = "exp_oxygen", both_oxygen = c("exp_oxygen", "oxygen_member"))
leak <- mclapply(leak_vars, function(v) {
  fit <- fit_gbm(cbind(train[c(predictors, v)], y = y_tr), best_gbm$depth, best_gbm$shrinkage, best_gbm$trees)
  predict_gb(fit, test)
}, mc.cores = min(cores, 2))
leak_metrics <- rbind(test_metrics["boosting", ], t(sapply(leak, metrics, y = y_te)))
rownames(leak_metrics) <- c("Pre-climb variables only", "+ expedition used oxygen", "+ climber used oxygen")
cat("\n=== Leakage check (boosting, test set) ===\n"); print(round(leak_metrics, 4))
lk <- data.frame(model = factor(rownames(leak_metrics), rev(rownames(leak_metrics))), auc = leak_metrics[, "auc"])
save_fig("fig15_leakage.png",
         ggplot(lk, aes(auc, model, fill = model == "Pre-climb variables only")) + geom_col(width = 0.65) +
           geom_text(aes(label = sprintf("%.3f", auc)), hjust = -0.15, size = 3.5) +
           scale_fill_manual(values = c(orange, blue), guide = "none") + coord_cartesian(xlim = c(0.5, 0.9)) +
           theme(panel.grid.major.y = element_blank()) +
           labs(title = "Test AUC of boosting when oxygen variables are added", x = NULL, y = NULL), 5.5, 2.3)

# 8b. Time split: train on 1950-2014, predict 2015-2019 with the tuned settings of each model.
#     This is the situation of a climber planning a future expedition.
old <- d$year <= 2014; past <- d[old, ]; future <- d[!old, ]
y_past <- as.integer(past$success == "yes"); y_fut <- as.integer(future$success == "yes")
tt_past <- terms(model.frame(f_glm, past))
en_past <- glmnet(design(tt_past, past), y_past, family = "binomial", alpha = best_alpha)
tree_past <- prune(rpart(f_all, past, method = "class", control = rpart.control(cp = 0.0003, minbucket = 50, xval = 0)), cp = cp_1se)
pred_fut <- list(
  baseline      = rep(mean(y_past), nrow(future)),
  elastic_net   = as.numeric(predict(en_past, design(tt_past, future), s = cv_en$lambda.1se, type = "response")),
  tree          = predict(tree_past, future)[, "yes"],
  random_forest = predict(fit_rf(past, best_rf$mtry, best_rf$node_size, trees = 500), future)$predictions[, "yes"],
  boosting      = predict_gb(fit_gbm(cbind(past[predictors], y = y_past), best_gbm$depth, best_gbm$shrinkage, best_gbm$trees), future))
time_metrics <- cbind(t(sapply(pred_fut, metrics, y = y_fut)), mean_predicted = sapply(pred_fut, mean))
cat("\n=== Time split: trained on 1950-2014 (", nrow(past), "climbers), tested on 2015-2019 (", nrow(future),
    "climbers, observed summit rate", round(mean(y_fut), 3), ") ===\n"); print(round(time_metrics, 4))
ts <- data.frame(year = future$year, Observed = y_fut, `Predicted (boosting)` = pred_fut$boosting, check.names = FALSE) |>
  group_by(year) |> summarise(across(everything(), mean)) |> pivot_longer(-year)
save_fig("fig16_time_split.png",
         ggplot(ts, aes(year, value, colour = name)) + geom_line(linewidth = 0.9) + geom_point(size = 2) +
           geom_text(data = filter(ts, year == 2019), aes(label = name), hjust = 0, nudge_x = 0.12, size = 3.4) +
           scale_colour_manual(values = c(Observed = "grey15", `Predicted (boosting)` = blue), guide = "none") +
           scale_y_continuous(labels = pct, limits = c(0, 0.75)) + scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2019) +
           labs(title = "Trained on 1950-2014, predicting 2015-2019", subtitle = "Average summit rate per year",
                x = NULL, y = NULL), 5.5, 3.2)

sink()
cat("Done: figures/ and results/ written.\n")
