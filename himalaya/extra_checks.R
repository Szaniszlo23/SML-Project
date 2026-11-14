# =====================================================================================
# Himalayan expeditions: extra checks for questions (backup slides)
#
# Uses the objects in results/models.rds (written by analysis.R) and refits only what the
# checks need, about 2-3 minutes:   Rscript extra_checks.R
#   1. Random vs grouped 5-fold CV for the tuned random forest and boosting
#   2. Unpenalised logistic regression with the same terms as the elastic net
# Output: results/extra_checks.txt and results/extra_checks.rds (used by figures.R)
# =====================================================================================

suppressPackageStartupMessages({ library(dplyr); library(glmnet); library(ranger); library(gbm); library(parallel) })
invisible(list2env(readRDS("results/models.rds"), environment()))
cores <- if (.Platform$OS.type == "windows") 1 else max(1, detectCores() - 1)
sink("results/extra_checks.txt", split = TRUE)

logloss <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
auc     <- function(p, y) { n1 <- sum(y); (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * (length(y) - n1)) }

# --- 1. Random vs grouped CV with the tuned settings --------------------------------------
# Grouped folds keep whole expeditions together (as in the analysis); random folds split
# climbers of one expedition across folds, so team-mates sit in training and validation.
set.seed(7)
folds <- list(grouped = foldid, random = sample(rep(1:5, length.out = nrow(train))))
f_all <- reformulate(predictors, "success")
gbm_train <- cbind(train[predictors], y = y_tr)
jobs <- expand.grid(fold = 1:5, scheme = names(folds), model = c("random_forest", "boosting"), stringsAsFactors = FALSE)
cv <- bind_rows(mclapply(seq_len(nrow(jobs)), function(j) {
  k <- jobs$fold[j]; f <- folds[[jobs$scheme[j]]]; tr <- f != k
  p <- if (jobs$model[j] == "random_forest") {
    fit <- ranger(f_all, train[tr, ], num.trees = 500, mtry = best_rf$mtry, min.node.size = best_rf$node_size,
                  probability = TRUE, seed = 1, num.threads = 1)
    predict(fit, train[!tr, ])$predictions[, "yes"]
  } else {
    set.seed(1)
    fit <- gbm(y ~ ., "bernoulli", data = gbm_train[tr, ], n.trees = best_gbm$trees, interaction.depth = best_gbm$depth,
               shrinkage = best_gbm$shrinkage, n.minobsinnode = 20, bag.fraction = 0.5)
    predict(fit, gbm_train[!tr, ], n.trees = best_gbm$trees, type = "response")
  }
  data.frame(jobs[j, ], logloss = logloss(p, y_tr[!tr]), auc = auc(p, y_tr[!tr]))
}, mc.cores = cores))
cv_compare <- cv |> group_by(model, scheme) |> summarise(cv_logloss = mean(logloss), cv_auc = mean(auc), .groups = "drop")
test_ll <- c(random_forest = logloss(pred$random_forest, y_te), boosting = logloss(pred$boosting, y_te))
test_auc <- c(random_forest = auc(pred$random_forest, y_te), boosting = auc(pred$boosting, y_te))
cv_compare$test_logloss <- test_ll[cv_compare$model]; cv_compare$test_auc <- test_auc[cv_compare$model]
cat("=== Random vs grouped 5-fold CV (tuned settings) vs the test set (new expeditions) ===\n")
print(as.data.frame(cv_compare), digits = 4, row.names = FALSE)
# The test set is one 20% draw of expeditions; compare its difficulty with the CV folds
base_cv <- mean(sapply(1:5, function(k) logloss(rep(mean(y_tr[foldid != k]), sum(foldid == k)), y_tr[foldid == k])))
base_te <- logloss(pred$baseline, y_te)
cat(sprintf("Baseline log loss: grouped CV %.4f, test %.4f (summit rate train %.3f, test %.3f)\n",
            base_cv, base_te, mean(y_tr), mean(y_te)))
cat(sprintf("Boosting log loss relative to the baseline: grouped CV %.3f, test %.3f\n",
            1 - cv_compare$cv_logloss[cv_compare$model == "boosting" & cv_compare$scheme == "grouped"] / base_cv,
            1 - test_ll[["boosting"]] / base_te))

# --- 2. Unpenalised logistic regression with the elastic-net terms ------------------------
f_glm <- ~ splines::ns(year, 4) + splines::ns(age, 4) + splines::ns(peak_height, 3) + season + sex + age_missing +
  citizenship + role + solo + peak + eight_thousander + climbed_before + log1p(team_size) + log1p(hired_staff) +
  log1p(staff_ratio) + commercial
design <- function(terms, data) model.matrix(terms, model.frame(terms, data))[, -1]
tt   <- terms(model.frame(f_glm, train))
X_tr <- design(tt, train); X_te <- design(tt, test)
logit <- glm(y_tr ~ X_tr, family = binomial())
p_glm <- as.numeric(plogis(cbind(1, X_te) %*% coef(logit)))
p_min <- as.numeric(predict(cv_en, X_te, s = "lambda.min", type = "response"))
glm_compare <- data.frame(model = c("logistic regression (no penalty)", "elastic net (lambda min)", "elastic net (lambda 1-SE)"),
                          terms = c(ncol(X_tr), sum(coef(cv_en, s = "lambda.min") != 0) - 1, sum(coef(cv_en, s = "lambda.1se") != 0) - 1),
                          test_logloss = c(logloss(p_glm, y_te), logloss(p_min, y_te), logloss(pred$elastic_net, y_te)),
                          test_auc = c(auc(p_glm, y_te), auc(p_min, y_te), auc(pred$elastic_net, y_te)))
cat("\n=== Unpenalised logistic regression vs elastic net (test set) ===\n")
print(glm_compare, digits = 4, row.names = FALSE)

sink()
saveRDS(list(cv_compare = cv_compare, glm_compare = glm_compare), "results/extra_checks.rds")
