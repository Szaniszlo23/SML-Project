# 05_insights.R -- figures for tuning, model comparison and interpretation, plus a time-split check
# Run from the himalaya/ folder after 03_models.R:  Rscript 05_insights.R   (~1 min)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
                                 library(glmnet); library(rpart); library(rpart.plot); library(ranger)
                                 library(gbm); library(pROC) })
M <- readRDS("results/models.rds"); list2env(M, environment())
th <- theme_minimal(base_size = 14); blue <- "#3b6ea8"
y_te <- as.integer(test$success == "yes")
logloss <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
nice <- c(baseline = "Baseline", elastic_net = "Elastic net", tree = "Pruned tree",
          random_forest = "Random forest", boosting = "Boosting")
sink("results/insights.txt")

# --- 1. tuning (one panel per method) ----------------------------------------------
t1 <- bind_rows(lapply(seq_along(cvs), function(i)
  data.frame(alpha = factor(alphas[i]), loglambda = log(cvs[[i]]$lambda), dev = cvs[[i]]$cvm))) |>
  ggplot(aes(loglambda, dev, colour = alpha)) + geom_line() +
  geom_vline(xintercept = log(cv_en$lambda.1se), linetype = 2) +
  labs(title = "Elastic net: grouped 5-fold CV", x = "log(lambda)", y = "CV deviance", colour = "alpha") + th
cpt <- as.data.frame(tree_full$cptable)
t2 <- ggplot(cpt, aes(nsplit + 1, xerror)) + geom_line() + geom_point(size = .8) +
  geom_errorbar(aes(ymin = xerror - xstd, ymax = xerror + xstd), width = 0, alpha = .4) +
  geom_vline(xintercept = sum(tree$frame$var == "<leaf>"), linetype = 2) + scale_x_log10() +
  labs(title = "Tree: CV error vs size (1-SE rule)", x = "leaves (log scale)", y = "relative CV error") + th
trf <- read.csv("results/tuning_random_forest.csv") |>
  pivot_longer(c(val_logloss, oob_logloss), names_to = "type", values_to = "ll") |>
  mutate(type = ifelse(type == "val_logloss", "held-out expeditions (used)", "out-of-bag (too optimistic)"))
t3 <- ggplot(trf, aes(factor(min.node.size), ll, colour = factor(mtry), group = interaction(mtry, type),
                      linetype = type)) + geom_line() + geom_point(size = 1) +
  labs(title = "Random forest: held-out vs out-of-bag log loss", x = "min node size", y = "log loss",
       colour = "mtry", linetype = NULL) + th + theme(legend.position = "bottom", legend.box = "vertical")
tgb <- read.csv("results/tuning_boosting.csv")
t4 <- ggplot(tgb, aes(trees, val_logloss, colour = factor(depth), linetype = factor(shrinkage))) + geom_line() +
  coord_cartesian(ylim = c(min(tgb$val_logloss), min(tgb$val_logloss) + .03)) +
  labs(title = "Boosting: validation log loss", x = "number of trees", y = "validation log loss",
       colour = "depth", linetype = "shrinkage") + th
ggsave("figures/mod_tuning.png", (t1 + t2) / (t3 + t4), width = 12, height = 8, dpi = 130)
ggsave("figures/mod_tuning_simple.png", t1 + t2, width = 12, height = 4.3, dpi = 130)
ggsave("figures/mod_tuning_flexible.png", t3 + t4, width = 12, height = 4.8, dpi = 130)

# --- 2. test comparison + calibration ---------------------------------------------------
met <- read.csv("results/test_metrics.csv", row.names = 1)
cmp <- met[names(nice), ] |> mutate(model = factor(nice, levels = rev(nice)))
c1 <- ggplot(cmp, aes(log_loss, model)) + geom_col(fill = blue) +
  geom_text(aes(label = sprintf("%.3f", log_loss)), hjust = -.1, size = 3.3) +
  coord_cartesian(xlim = c(.45, .66)) + labs(title = "Test log loss (lower = better)", x = NULL, y = NULL) + th
c2 <- ggplot(cmp, aes(auc, model)) + geom_col(fill = blue) +
  geom_text(aes(label = sprintf("%.3f", auc)), hjust = -.1, size = 3.3) +
  coord_cartesian(xlim = c(.5, .85)) + labs(title = "Test AUC (higher = better)", x = NULL, y = NULL) + th
cal <- bind_rows(lapply(names(nice)[-1], function(m) data.frame(model = unname(nice[m]), p = pred[[m]], y = y_te))) |>
  group_by(model) |> mutate(bin = ntile(p, 10)) |> group_by(model, bin) |>
  summarise(pred = mean(p), obs = mean(y), .groups = "drop")
c3 <- ggplot(cal, aes(pred, obs, colour = model)) + geom_abline(linetype = 2, colour = "grey50") +
  geom_line() + geom_point(size = 1.2) + coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Calibration (test deciles)", x = "predicted probability", y = "observed success rate", colour = NULL) + th
ggsave("figures/mod_comparison.png", (c1 / c2 | c3) + plot_layout(widths = c(1, 1.1)), width = 12, height = 5.5, dpi = 130)
ggsave("figures/mod_calibration.png", c3 + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 2)), width = 6, height = 6.5, dpi = 130)
cat("=== Test metrics ===\n"); print(met)
cat("\nAccuracy of always predicting 'no summit':", round(1 - mean(y_te), 3), "\n")

# --- 3. pruned tree -------------------------------------------------------------------
png("figures/mod_tree.png", width = 1300, height = 720, res = 130)
cps <- tree$cptable; top <- prune(tree, cp = cps[max(which(cps[, "nsplit"] <= 4)), "CP"])  # top 4 splits for the slide
rpart.plot(top, tweak = 1.2, roundint = FALSE,
           type = 2, extra = 107, box.palette = "Blues", main = "")
dev.off()

# --- 4. importance: permutation vs Gini (impurity) ---------------------------------------
rf_imp <- ranger(reformulate(predictors, "success"), train, num.trees = 300, mtry = rf_params$mtry,
                 min.node.size = rf_params$min.node.size, probability = TRUE, importance = "impurity", seed = 1)
imp <- data.frame(var = predictors, permutation = importance(rf)[predictors], gini = importance(rf_imp)[predictors]) |>
  mutate(across(c(permutation, gini), ~ .x / max(.x))) |> pivot_longer(-var)
ord <- imp |> filter(name == "permutation") |> arrange(value) |> pull(var)
p_imp <- ggplot(imp, aes(value, factor(var, ord), fill = name)) + geom_col(position = "dodge") +
  scale_fill_manual(values = c(gini = "#9ca3af", permutation = blue)) +
  labs(title = "Random forest importance (scaled to max = 1)", x = NULL, y = NULL, fill = NULL) + th
ggsave("figures/ins_importance.png", p_imp, width = 7.5, height = 5.5, dpi = 130)
cat("\n=== Importance (scaled) ===\n"); print(imp |> pivot_wider() |> arrange(desc(permutation)), n = 20)

# --- 5. partial dependence from the boosting model ---------------------------------------
pd <- function(v) { g <- plot(gbm, i.var = v, n.trees = gbm_params$trees, return.grid = TRUE, type = "response")
                    names(g) <- c("x", "p"); g$var <- v; g$x <- as.character(g$x); g }
num <- c("age", "year", "staff_ratio")   # peak height and team size: jagged / flat, see peak factor
pdn <- bind_rows(lapply(num, pd)) |> mutate(x = as.numeric(x))
lims <- train |> summarise(across(all_of(num), ~ list(quantile(.x, c(.01, .99))))) # trim to data support
pdn <- pdn |> group_by(var) |> filter(x >= lims[[var[1]]][[1]][1], x <= lims[[var[1]]][[1]][2]) |> ungroup()
labs_num <- c(age = "age", year = "year", peak_height = "peak height (m)", staff_ratio = "hired staff per climber",
              team_size = "team size")
p_pd <- ggplot(pdn, aes(x, p)) + geom_line(colour = blue, linewidth = 1) +
  facet_wrap(~ var, scales = "free_x", labeller = as_labeller(labs_num), nrow = 1) +
  scale_y_continuous(labels = scales::percent) + labs(title = "Partial dependence (boosting model)",
                                                     x = NULL, y = "predicted success") + th
pdc <- bind_rows(lapply(c("season", "peak"), pd))
p_pc <- ggplot(pdc, aes(p, reorder(x, p))) + geom_point(colour = blue, size = 2) +
  facet_wrap(~ var, scales = "free_y") + scale_x_continuous(labels = scales::percent) +
  labs(x = "predicted success", y = NULL) + th
ggsave("figures/ins_partial_dependence.png", p_pd / p_pc + plot_layout(heights = c(1, 1.1)),
       width = 11, height = 6.5, dpi = 130)

# --- 6. leakage: what the oxygen variables would "add" -------------------------------------
lk <- data.frame(model = c("Boosting (pre-climb variables)", "+ expedition used oxygen", "+ climber used oxygen too"),
                 auc = c(met["boosting", "auc"], met["LEAKY_boosting_plus_exp_oxygen", "auc"],
                         met["LEAKY_boosting_plus_both_oxygen", "auc"]))
lk$model <- factor(lk$model, rev(lk$model))
p_lk <- ggplot(lk, aes(auc, model, fill = model != "Boosting (pre-climb variables)")) + geom_col() +
  geom_text(aes(label = sprintf("%.3f", auc)), hjust = -.1, size = 3.5) +
  scale_fill_manual(values = c(blue, "#c2410c"), guide = "none") + coord_cartesian(xlim = c(.5, .9)) +
  labs(title = "Leakage: oxygen is only recorded once a climber is high up",
       subtitle = "The 'better' models use information from after the start of the climb", x = "test AUC", y = NULL) + th
ggsave("figures/ins_leakage.png", p_lk, width = 8, height = 3, dpi = 130)

# --- 7. time split: train on <= 2014, test on 2015-2019 ---------------------------------------
all <- bind_rows(train, test); old <- all$year <= 2014; y_new <- as.integer(all$success[!old] == "yes")
g_old <- all[old, predictors]; g_old$y <- as.integer(all$success[old] == "yes"); set.seed(1)
gb_t <- gbm(y ~ ., "bernoulli", data = g_old, n.trees = gbm_params$trees, interaction.depth = gbm_params$depth,
            shrinkage = gbm_params$shrinkage, n.minobsinnode = 20, bag.fraction = .5)
p_new <- predict(gb_t, all[!old, ], n.trees = gbm_params$trees, type = "response")
cat("\n=== Time split (boosting, trained on <= 2014, tested on 2015-2019:", sum(!old), "climbers) ===\n")
cat(sprintf("log loss %.4f | AUC %.4f | mean predicted %.3f vs observed %.3f\n",
            logloss(p_new, y_new), auc(y_new, p_new, quiet = TRUE), mean(p_new), mean(y_new)))
ts <- data.frame(year = all$year[!old], predicted = p_new, observed = y_new) |> group_by(year) |>
  summarise(predicted = mean(predicted), observed = mean(observed)) |> pivot_longer(-year)
p_ts <- ggplot(ts, aes(year, value, colour = name)) + geom_line(linewidth = 1) + geom_point(size = 2.5) +
  scale_colour_manual(values = c(observed = "#c2410c", predicted = blue)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .75)) +
  labs(title = "Trained on 1950-2014, predicting 2015-2019", x = NULL, y = "success rate", colour = NULL,
       subtitle = "2015: Everest season cancelled after the earthquake (0% on Everest)") + th
ggsave("figures/ins_time_split.png", p_ts, width = 7, height = 3.6, dpi = 130)

# --- 8. example climbers ---------------------------------------------------------------------
proto <- test[rep(1, 4), ]
ex <- data.frame(who = c("45-yr-old woman, commercial Everest, spring",
                         "30-yr-old man, small team on Ama Dablam, autumn",
                         "55-yr-old man, Everest, no hired staff",
                         "35-yr-old man, Dhaulagiri I, winter"),
                 age = c(45, 30, 55, 35), sex = c("F", "M", "M", "M"),
                 peak = c("Everest", "Ama Dablam", "Everest", "Dhaulagiri I"),
                 season = c("Spring", "Autumn", "Spring", "Winter"),
                 team_size = c(10, 3, 4, 6), hired_staff = c(12, 1, 0, 4), commercial = c(1, 1, 0, 0))
for (v in c("age", "team_size", "hired_staff", "commercial")) proto[[v]] <- ex[[v]]
proto$sex <- factor(ex$sex, levels(train$sex)); proto$peak <- factor(ex$peak, levels(train$peak))
proto$season <- factor(ex$season, levels(train$season))
proto$peak_height <- c(Everest = 8850, "Ama Dablam" = 6814, "Dhaulagiri I" = 8167)[ex$peak]
proto$eight_thousander <- as.integer(proto$peak_height >= 8000); proto$climbed_before <- 1L
proto$staff_ratio <- proto$hired_staff / pmax(proto$team_size, 1); proto$year <- 2018; proto$age_missing <- 0L
proto$role <- factor("climber", levels(train$role)); proto$solo <- 0L
proto$citizenship <- factor("USA", levels(train$citizenship))
ex$predicted_success <- round(predict(gbm, proto, n.trees = gbm_params$trees, type = "response"), 2)
cat("\n=== Example climbers (US citizens, 2018) ===\n"); print(ex[, c("who", "predicted_success")])
sink()
cat(readLines("results/insights.txt"), sep = "\n")
