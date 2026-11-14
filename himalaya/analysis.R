# =====================================================================================
# Himalayan expeditions: how much of summit success is decided before the climb starts?
#
# Main question : Using only what is known before an expedition leaves, how well can we
#                 predict whether a climber reaches the summit, and what drives the odds?
# Data          : The Himalayan Database (TidyTuesday 2020-09-22): members, expeditions, peaks
# Methods       : elastic-net logistic regression, pruned tree, random forest, gradient boosting
#
# Run from the himalaya/ folder:  Rscript analysis.R      (about 15 minutes on 8 cores)
# Output: results/*.txt, *.csv and results/models.rds (fitted objects). At the end it runs
# figures.R, which draws figures/fig*.png from models.rds. To change only the figures, run
# Rscript figures.R (a few seconds, nothing is refitted).
#
#   1. Load the data              5. Fit and tune the four models
#   2. Population and features    6. Assessment on the test set
#   3. Exploratory analysis       7. Insights from the final model
#   4. Train / test split         8. How far can we trust it? (leakage, time split)
# =====================================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(glmnet); library(rpart); library(ranger); library(gbm); library(parallel)
})
set.seed(2026)
dir.create("figures", showWarnings = FALSE); dir.create("results", showWarnings = FALSE)
cores <- if (.Platform$OS.type == "windows") 1 else max(1, detectCores() - 1)
sink("results/results.txt", split = TRUE)          # everything printed below is also saved


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
# Figures of this section: see figures.R (Figures 1, 3, 4).

# 3d. climbers of one expedition share their fate -> the data are clustered
cl <- d |> group_by(expedition_id) |> filter(n() > 1) |> summarise(share = mean(success == "yes"))
cat("\nExpeditions with 2+ climbers:", nrow(cl), "| nobody summits:", round(100 * mean(cl$share == 0)),
    "% | everybody summits:", round(100 * mean(cl$share == 1)), "%\n")


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

# Tuning figures: see figures.R (Figures 5, 6).


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


# 6c. the answer to the main question: how far do pre-climb facts move the odds?
dec <- data.frame(p = pred$boosting, y = y_te) |> mutate(decile = ntile(p, 10)) |> group_by(decile) |>
  summarise(predicted = mean(p), observed = mean(y), n = n())
cat("\nBoosting, test climbers sorted into tenths by predicted probability:\n"); print(as.data.frame(round(dec, 3)), row.names = FALSE)
write.csv(round(test_metrics, 4), "results/test_metrics.csv")


# 6e. Confusion matrices at a cut-off c: sensitivity, specificity, precision (Unit 4)
conf <- function(p, y, c) { yhat <- as.integer(p > c); tp <- sum(yhat & y); tn <- sum(!yhat & !y)
  fp <- sum(yhat & !y); fn <- sum(!yhat & y)
  c(cutoff = c, TP = tp, FN = fn, FP = fp, TN = tn, accuracy = (tp + tn) / length(y),
    sensitivity = tp / (tp + fn), specificity = tn / (tn + fp), precision = tp / (tp + fp)) }
conf_tab <- rbind(t(sapply(pred[-1], conf, y = y_te, c = 0.5)),
                  boosting_cutoff_base_rate = conf(pred$boosting, y_te, mean(y_tr)))
cat("\n=== Confusion matrices on the test set (cut-off 0.5; last row: boosting at the training summit rate) ===\n")
print(round(conf_tab, 3))
write.csv(round(conf_tab, 4), "results/confusion.csv")

# 6f. Elastic net as an interpretable logit model (Units 2-3): coefficient path and odds ratios
b_1se <- as.matrix(coef(cv_en, s = "lambda.1se"))[-1, 1]
nice  <- function(x) x |> sub("^season", "season: ", x = _) |> sub("^peak", "peak: ", x = _) |>
  sub("^citizenship", "citizenship: ", x = _) |> sub("^sexM", "male", x = _) |> sub("^rolele", "role: le", x = _) |>
  sub("^log1p\\((.*)\\)$", "log(1 + \\1)", x = _) |> sub("staff_ratio", "staff per climber", x = _)
or_tab <- data.frame(term = nice(names(b_1se)), beta = b_1se, odds_ratio = exp(b_1se)) |>
  filter(!grepl("ns\\(", term), beta != 0) |> arrange(desc(abs(beta)))
cat("\n=== Elastic net (alpha", best_alpha, ", lambda 1-SE): odds ratios of the non-spline terms ===\n")
print(or_tab, row.names = FALSE, digits = 3)
write.csv(or_tab, "results/odds_ratios.csv", row.names = FALSE)


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

# Relative influence as defined for gradient tree boosting (gbm), next to the permutation measure
rel_inf <- summary(gb, plotit = FALSE, method = relative.influence)
imp_gb  <- data.frame(variable = predictors,
                      relative_influence = rel_inf$rel.inf[match(predictors, rel_inf$var)],
                      permutation = imp_var$increase) |> mutate(across(-variable, ~ .x / max(.x)))
cat("\n=== Boosting: relative influence vs test permutation importance (largest = 1) ===\n")
print(imp_gb[order(-imp_gb$relative_influence), ], row.names = FALSE, digits = 2)


# 7d. Gini vs permutation importance in the random forest (Gini favours variables with many split points)
rf_gini <- fit_rf(train, best_rf$mtry, best_rf$node_size, importance = "impurity")
imp_rf <- data.frame(variable = predictors, permutation = importance(rf)[predictors], gini = importance(rf_gini)[predictors]) |>
  mutate(across(-variable, ~ .x / max(.x)))
cat("\n=== Random forest importance (scaled to max = 1) ===\n"); print(imp_rf[order(-imp_rf$permutation), ], row.names = FALSE, digits = 2)

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

# Fitted objects for re-drawing figures without refitting (large, not in git)
saveRDS(list(d = d, train = train, test = test, y_tr = y_tr, y_te = y_te, foldid = foldid, predictors = predictors,
             alphas = alphas, cv_list = cv_list, cv_en = cv_en, best_alpha = best_alpha, tree = tree, cpt = cpt, n_leaves = n_leaves,
             rf = rf, tune_rf = tune_rf, best_rf = best_rf, gb = gb, tune_gbm = tune_gbm, best_gbm = best_gbm,
             pred = pred, boot_ci = boot_ci, imp_block = imp_block, imp_var = imp_var, imp_rf = imp_rf, imp_gb = imp_gb,
             plans = plans, leak_metrics = leak_metrics, pred_fut = pred_fut, y_fut = y_fut, future = future),
        "results/models.rds")

sink()
source("figures.R")       # draw all figures from results/models.rds
