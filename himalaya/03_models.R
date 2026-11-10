# 03_models.R -- four course methods, each tuned without touching the test set:
#   elastic-net logistic regression (units 2-3), pruned tree (unit 5),
#   random forest (unit 6), gradient boosting (unit 7)
# Run from the himalaya/ folder after 02_prepare.R:  Rscript 03_models.R   (~2 min)
suppressPackageStartupMessages({ library(dplyr); library(glmnet); library(rpart); library(ranger)
                                 library(gbm); library(pROC); library(parallel) })
set.seed(2026)
d <- readRDS("data/model_data.rds")
dir.create("results", showWarnings = FALSE)

# --- split by EXPEDITION (climbers of one expedition share weather, route, team) ---
ex      <- unique(d$expedition_id)
test_ex <- sample(ex, round(0.2 * length(ex)))
train <- filter(d, !expedition_id %in% test_ex)
test  <- filter(d,  expedition_id %in% test_ex)
fold_of_ex <- setNames(sample(rep(1:5, length.out = n_distinct(train$expedition_id))), unique(train$expedition_id))
foldid <- unname(fold_of_ex[train$expedition_id])        # grouped 5-fold CV for tuning
cat("train", nrow(train), "climbers | test", nrow(test), "climbers,", length(test_ex), "expeditions\n")

predictors <- c("year", "season", "sex", "age", "age_missing", "citizenship", "role", "solo", "peak",
                "peak_height", "eight_thousander", "climbed_before", "team_size", "hired_staff",
                "staff_ratio", "commercial")
f_rf  <- reformulate(predictors, "success")
y_tr  <- as.integer(train$success == "yes"); y_te <- as.integer(test$success == "yes")

logloss <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
metrics <- function(p, y = y_te) c(log_loss = logloss(p, y), brier = mean((p - y)^2),
                                   auc = as.numeric(auc(y, p, quiet = TRUE)), accuracy = mean((p > 0.5) == y))
pred <- list(baseline = rep(mean(y_tr), nrow(test)))

# --- 1. Logistic regression, elastic net: alpha grid x lambda by grouped CV --------
f_glm <- ~ splines::ns(year, 4) + splines::ns(age, 4) + splines::ns(peak_height, 3) + season + sex + age_missing +
  citizenship + role + solo + peak + eight_thousander + climbed_before + log1p(team_size) + log1p(hired_staff) +
  log1p(staff_ratio) + commercial
X_tr <- model.matrix(f_glm, train)[, -1]; X_te <- model.matrix(f_glm, test)[, -1]
alphas <- c(0, .25, .5, .75, 1)
cvs <- lapply(alphas, function(a) cv.glmnet(X_tr, y_tr, family = "binomial", alpha = a, foldid = foldid))
tune_en <- data.frame(alpha = alphas, cv_deviance = sapply(cvs, function(cv) min(cv$cvm)))
best_a  <- which.min(tune_en$cv_deviance); cv_en <- cvs[[best_a]]
pred$elastic_net <- as.numeric(predict(cv_en, X_te, s = "lambda.1se", type = "response"))
cat("\nElastic net: alpha =", alphas[best_a], " lambda.1se =", signif(cv_en$lambda.1se, 3), " non-zero:",
    sum(coef(cv_en, s = "lambda.1se") != 0) - 1, "of", ncol(X_tr), "\n"); print(tune_en)

# --- 2. Classification tree: cost-complexity pruning, 1-SE rule ------------------
tree <- rpart(f_rf, data = train, method = "class", control = rpart.control(cp = 0.0003, minbucket = 50, xval = foldid))
cpt <- tree$cptable; best <- which.min(cpt[, "xerror"])
cp_1se <- cpt[which(cpt[, "xerror"] <= cpt[best, "xerror"] + cpt[best, "xstd"])[1], "CP"]
tree_p <- prune(tree, cp = cp_1se)
pred$tree <- predict(tree_p, test)[, "yes"]
cat("\nTree: 1-SE cp =", signif(cp_1se, 3), " leaves:", sum(tree_p$frame$var == "<leaf>"), "\n")

# --- 3. Random forest: mtry x node size tuned on held-out EXPEDITIONS (CV fold 5) ---
# The out-of-bag error is also recorded, but NOT used: OOB leaves out single climbers, so
# teammates of the same expedition are in the bag and the OOB error is far too optimistic.
tune_rf <- expand.grid(mtry = c(2, 4, 7), min.node.size = c(25, 100, 400, 1000))
tune_rf[c("val_logloss", "oob_logloss")] <- t(sapply(seq_len(nrow(tune_rf)), function(g) {
  fit <- ranger(f_rf, train[foldid != 5, ], num.trees = 300, mtry = tune_rf$mtry[g],
                min.node.size = tune_rf$min.node.size[g], probability = TRUE, num.threads = 0, seed = 1)
  c(logloss(predict(fit, train[foldid == 5, ])$predictions[, "yes"], y_tr[foldid == 5]),
    logloss(fit$predictions[, "yes"], y_tr[foldid != 5]))
}))
bg <- tune_rf[which.min(tune_rf$val_logloss), ]
cat("\nRandom forest (validation fold = held-out expeditions; OOB for comparison):\n")
print(tune_rf[order(tune_rf$val_logloss), ])
rf <- ranger(f_rf, train, num.trees = 500, mtry = bg$mtry, min.node.size = bg$min.node.size,
             probability = TRUE, importance = "permutation", num.threads = 0, seed = 1)
pred$random_forest <- predict(rf, test)$predictions[, "yes"]

# --- 4. Gradient boosting: depth x shrinkage x trees, validated on CV fold 5 -----
gtr <- train[, c(predictors)]; gtr$y <- y_tr
grid_gbm <- expand.grid(depth = c(2, 4, 6), shrinkage = c(.02, .1))
fits <- mclapply(seq_len(nrow(grid_gbm)), function(g) {
  set.seed(1)
  fit <- gbm(y ~ ., "bernoulli", data = gtr[foldid != 5, ], n.trees = 3000, interaction.depth = grid_gbm$depth[g],
             shrinkage = grid_gbm$shrinkage[g], n.minobsinnode = 20, bag.fraction = .5)
  P <- predict(fit, gtr[foldid == 5, ], n.trees = seq(50, 3000, 50), type = "response")
  data.frame(depth = grid_gbm$depth[g], shrinkage = grid_gbm$shrinkage[g], trees = seq(50, 3000, 50),
             val_logloss = apply(P, 2, logloss, y = y_tr[foldid == 5]))
}, mc.cores = 6)
curve_gbm <- do.call(rbind, fits)
bb <- curve_gbm[which.min(curve_gbm$val_logloss), ]
cat("\nBoosting (validation fold): best depth", bb$depth, "shrinkage", bb$shrinkage, "trees", bb$trees, "\n")
print(curve_gbm |> group_by(depth, shrinkage) |> slice_min(val_logloss, n = 1) |> arrange(val_logloss))
set.seed(1)
gb <- gbm(y ~ ., "bernoulli", data = gtr, n.trees = bb$trees, interaction.depth = bb$depth,
          shrinkage = bb$shrinkage, n.minobsinnode = 20, bag.fraction = .5)
pred$boosting <- predict(gb, test, n.trees = bb$trees, type = "response")

# --- leakage comparison: same boosting model + the oxygen variables -------------
# (both are recorded only if oxygen was actually used, i.e. high on the mountain)
leak <- lapply(list(exp_oxygen = "exp_oxygen", both_oxygen = c("exp_oxygen", "oxygen_member")), function(v) {
  dl <- train[, c(predictors, v)]; dl$y <- y_tr; set.seed(1)
  fit <- gbm(y ~ ., "bernoulli", data = dl, n.trees = bb$trees, interaction.depth = bb$depth,
             shrinkage = bb$shrinkage, n.minobsinnode = 20, bag.fraction = .5)
  predict(fit, test, n.trees = bb$trees, type = "response")
})

# --- test-set results (scored ONCE, after all tuning) ----------------------------
out <- round(do.call(rbind, lapply(pred, metrics)), 4)
out_leak <- round(do.call(rbind, lapply(leak, metrics)), 4)
rownames(out_leak) <- paste0("LEAKY_boosting_plus_", rownames(out_leak))
cat("\n=== Test set ===\n"); print(rbind(out, out_leak))
write.csv(rbind(out, out_leak), "results/test_metrics.csv")
write.csv(tune_en, "results/tuning_elastic_net.csv", row.names = FALSE)
write.csv(tune_rf, "results/tuning_random_forest.csv", row.names = FALSE)
write.csv(curve_gbm, "results/tuning_boosting.csv", row.names = FALSE)
saveRDS(list(train = train, test = test, foldid = foldid, predictors = predictors, f_glm = f_glm,
             cv_en = cv_en, cvs = cvs, alphas = alphas, tree_full = tree, tree = tree_p, cp_1se = cp_1se,
             rf = rf, rf_params = bg, gbm = gb, gbm_params = bb, pred = pred, leak = leak),
        "results/models.rds")
cat("\nwritten results/test_metrics.csv, results/tuning_*.csv, results/models.rds\n")
