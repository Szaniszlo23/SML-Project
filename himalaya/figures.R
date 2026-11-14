# =====================================================================================
# Himalayan expeditions: all figures for the slides
#
# Draws figures/fig*.png from the fitted objects in results/models.rds, which analysis.R
# writes. Nothing is refitted, so this takes seconds:   Rscript figures.R
# Figures are saved at the size they have on the slides (inches), so all text has the
# same size; the font is the one used in the slides (Latin Modern Sans) where available.
# =====================================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(glmnet); library(rpart); library(rpart.plot); library(gbm)
})
invisible(list2env(readRDS("results/models.rds"), environment()))
dir.create("figures", showWarnings = FALSE)

# --- style ---------------------------------------------------------------------------
ink <- "grey15"; grey <- "grey55"; light <- "grey82"; accent <- "#1F4E8C"
# Latin Modern Sans, the LaTeX font of the slides (ships with TeX Live); default sans if not found
lm_dir <- Sys.getenv("LM_DIR", "/usr/local/texlive/2023/texmf-dist/fonts/opentype/public/lm")
font <- "sans"
if (file.exists(file.path(lm_dir, "lmsans10-regular.otf"))) {
  try(systemfonts::register_font("Latin Modern Sans", plain = file.path(lm_dir, "lmsans10-regular.otf"),
                                 bold = file.path(lm_dir, "lmsans10-bold.otf"),
                                 italic = file.path(lm_dir, "lmsans10-oblique.otf"),
                                 bolditalic = file.path(lm_dir, "lmsans10-boldoblique.otf")), silent = TRUE)
  font <- "Latin Modern Sans"
}
theme_set(theme_classic(base_size = 8, base_family = font) +
            theme(axis.line = element_line(linewidth = 0.3, colour = "grey25"),
                  axis.ticks = element_line(linewidth = 0.3, colour = "grey25"),
                  axis.text = element_text(size = 7, colour = "grey20"), axis.title = element_text(size = 7.5),
                  plot.title = element_text(size = 8, face = "bold", margin = margin(b = 4)),
                  plot.title.position = "plot",
                  strip.background = element_blank(), strip.text = element_text(size = 7.5, face = "bold", hjust = 0),
                  legend.text = element_text(size = 7), legend.title = element_text(size = 7),
                  legend.key.size = unit(0.3, "cm"), legend.background = element_blank(),
                  plot.margin = margin(4, 6, 4, 4)))
update_geom_defaults("text", list(family = font, size = 2.4, colour = "grey20"))
pct      <- function(x) scales::percent(x, accuracy = 1)
save_fig <- function(name, plot, w, h) ggsave(file.path("figures", name), plot, width = w, height = h, dpi = 400,
                                              bg = "white", device = ragg::agg_png)
rate     <- function(df, g) df |> group_by({{ g }}) |> summarise(n = n(), success = mean(success == "yes"), .groups = "drop")
greys    <- function(n) grey.colors(n, start = 0.75, end = 0.1)
# ordered palette for tuning parameters (light to dark), qualitative one for the four models
seq_cols <- function(n) scales::viridis_pal(begin = 0.15, end = 0.85, option = "D")(n)
model_cols <- c("Elastic net" = "#E69F00", "Pruned tree" = "#009E73", "Random forest" = "#CC79A7", "Boosting" = accent)
model_names <- c(baseline = "Summit rate only", elastic_net = "Elastic net", tree = "Pruned tree",
                 random_forest = "Random forest", boosting = "Boosting")
predict_gb <- function(fit, data) predict(fit, data, n.trees = fit$n.trees, type = "response")
base_rate <- mean(d$success == "yes")
raw_d <- mutate(d, age = ifelse(age_missing == 1, NA, age))     # before the median imputation


# =====================================================================================
# Exploratory analysis
# =====================================================================================
# Fig 1: summit rate by year, peak, season, age and hired support
avg <- geom_hline(yintercept = base_rate, linetype = 2, colour = grey, linewidth = 0.3)
e1 <- ggplot(filter(rate(raw_d, year), n >= 50), aes(year, success)) + avg +
  geom_line(colour = ink, linewidth = 0.4) + geom_point(colour = ink, size = 0.5) +
  scale_y_continuous(labels = pct, limits = c(0, 0.7), expand = c(0, 0)) + scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  labs(title = "(a) Year", x = NULL, y = "summit rate")
bar <- function(df, title, flip = FALSE) {
  p <- ggplot(df, aes(g, success)) + geom_col(fill = grey, width = 0.65) + avg +
    scale_y_continuous(labels = pct, limits = c(0, 0.6), breaks = c(0, 0.2, 0.4, 0.6), expand = c(0, 0)) +
    labs(title = title, x = NULL, y = NULL)
  if (flip) p + coord_flip() else p
}
top10 <- names(sort(table(raw_d$peak_name), decreasing = TRUE))[1:10]
g1 <- raw_d |> mutate(g = ifelse(peak_name %in% top10, peak_name, "All other peaks")) |> rate(g) |>
  mutate(g = reorder(g, success)) |> bar("(b) Peak", flip = TRUE)
g2 <- raw_d |> mutate(g = factor(substr(season, 1, 3), c("Spr", "Sum", "Aut", "Win"))) |> rate(g) |> bar("(c) Season")
g3 <- raw_d |> filter(!is.na(age)) |>
  mutate(g = cut(age, c(0, 30, 40, 50, 60, 100), labels = c("<30", "30s", "40s", "50s", "60+"), right = FALSE)) |>
  rate(g) |> bar("(d) Age")
g4 <- raw_d |> mutate(g = cut(staff_ratio, c(-Inf, 0, .5, 1, 2, Inf), labels = c("0", "<.5", ".5-1", "1-2", ">2"))) |>
  rate(g) |> bar("(e) Hired staff per climber")
save_fig("fig01_eda.png", ((e1 | g1) + plot_layout(widths = c(1.45, 1))) / (g2 | g3 | g4) + plot_layout(heights = c(1.4, 1)), 5.6, 2.35)

# Fig 3: oxygen use is leakage. Among climbers who failed on 8000 m peaks, it depends on how high they got
ox <- raw_d |> filter(success == "no", peak_height >= 8000, !is.na(highpoint_metres)) |>
  mutate(g = cut(highpoint_metres, c(0, 6500, 7000, 7500, 8000, 9000), labels = c("<6.5", "6.5-7", "7-7.5", "7.5-8", ">8"))) |>
  group_by(g) |> summarise(oxygen = mean(oxygen_member))
save_fig("fig03_oxygen_leakage.png",
         ggplot(ox, aes(g, oxygen)) + geom_col(fill = grey, width = 0.65) +
           geom_text(aes(label = pct(oxygen)), vjust = -0.6) +
           scale_y_continuous(labels = pct, limits = c(0, 0.75), expand = c(0, 0)) +
           labs(x = "highest point reached (km)", y = "share who used oxygen"), 2.7, 1.9)

# Fig 4: climbers of one expedition share their fate
cl <- raw_d |> group_by(expedition_id) |> filter(n() > 1) |> summarise(share = mean(success == "yes"))
save_fig("fig04_clustering.png",
         ggplot(cl, aes(share)) + geom_histogram(breaks = seq(-0.05, 1.05, 0.1), fill = grey, colour = "white", linewidth = 0.3) +
           scale_x_continuous(labels = pct, breaks = seq(0, 1, 0.25)) + scale_y_continuous(expand = c(0, 0)) +
           labs(x = "share of the team that reached the summit", y = "expeditions"), 2.5, 1.9)


# =====================================================================================
# Tuning
# =====================================================================================
min_dev <- min(sapply(cv_list, function(cv) min(cv$cvm)))
t1 <- bind_rows(lapply(seq_along(alphas), function(i)
  data.frame(alpha = factor(alphas[i]), loglambda = log(cv_list[[i]]$lambda), dev = cv_list[[i]]$cvm))) |>
  ggplot(aes(loglambda, dev, colour = alpha)) + geom_line(linewidth = 0.5) +
  geom_vline(xintercept = log(cv_en$lambda.1se), linetype = 2, linewidth = 0.3) +
  scale_colour_manual(values = seq_cols(5)) + coord_cartesian(ylim = c(min_dev, min_dev + 0.1)) +
  labs(title = "(a) Elastic net", x = expression(log(lambda)), y = "CV deviance", colour = expression(alpha))
t2 <- ggplot(cpt, aes(nsplit + 1, xerror)) +
  geom_errorbar(aes(ymin = xerror - xstd, ymax = xerror + xstd), width = 0, colour = grey, linewidth = 0.3) +
  geom_line(colour = ink, linewidth = 0.4) + geom_point(colour = ink, size = 0.5) +
  geom_vline(xintercept = n_leaves, linetype = 2, linewidth = 0.3) + scale_x_log10() +
  labs(title = "(b) Classification tree", x = "leaves (log scale)", y = "rel. CV error")
t3 <- tune_rf |> pivot_longer(c(cv_logloss, oob_logloss), names_to = "type", values_to = "ll") |>
  mutate(type = ifelse(type == "cv_logloss", "grouped CV", "out-of-bag")) |>
  ggplot(aes(factor(node_size), ll, colour = factor(mtry), linetype = type, group = interaction(mtry, type))) +
  geom_line(linewidth = 0.45) + geom_point(size = 0.8) +
  scale_colour_manual(values = seq_cols(4)) + scale_linetype_manual(values = c("grouped CV" = 1, "out-of-bag" = 2)) +
  labs(title = "(c) Random forest", x = "minimum node size", y = "CV log loss", colour = "m", linetype = NULL)
t4 <- ggplot(tune_gbm, aes(trees, cv_logloss, colour = factor(depth), linetype = factor(shrinkage))) +
  geom_line(linewidth = 0.45) + scale_colour_manual(values = seq_cols(3)) +
  geom_point(data = best_gbm, aes(trees, cv_logloss), inherit.aes = FALSE, colour = "black", shape = 21, fill = "white", size = 1.6, stroke = 0.6) +
  coord_cartesian(ylim = c(best_gbm$cv_logloss, best_gbm$cv_logloss + 0.03)) +
  labs(title = "(d) Gradient boosting", x = "number of trees M", y = "CV log loss",
       colour = "J", linetype = expression(nu))
# Fig 4: tuning of all four methods (grouped 5-fold CV)
tuning_legends <- theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(0, 0, 0, 0),
                        legend.spacing.y = unit(0, "pt"), legend.key.width = unit(0.35, "cm"),
                        legend.key.height = unit(0.2, "cm"), legend.text = element_text(size = 6.5),
                        legend.box.spacing = unit(2, "pt"))
save_fig("fig06_tuning.png", (t1 + t2) / (t3 + t4) & tuning_legends, 3.65, 2.6)


# =====================================================================================
# Assessment on the test set
# =====================================================================================
# Fig 7: test log loss and AUC with bootstrap intervals (final model highlighted)
cmp <- boot_ci |> mutate(model = factor(model_names[model], rev(model_names)), final = model == "Boosting")
r1 <- ggplot(cmp, aes(log_loss, model, colour = final)) +
  geom_pointrange(aes(xmin = ll_lo, xmax = ll_hi), size = 0.15, linewidth = 0.45) +
  labs(title = "(a) Log loss", x = NULL, y = NULL)
r2 <- ggplot(filter(cmp, model != "Summit rate only"), aes(auc, model, colour = final)) +
  geom_pointrange(aes(xmin = auc_lo, xmax = auc_hi), size = 0.15, linewidth = 0.45) +
  labs(title = "(b) AUC", x = NULL, y = NULL)
save_fig("fig07_comparison.png", (r1 / r2 + plot_layout(heights = c(5, 4))) &
           scale_colour_manual(values = c(ink, accent), guide = "none"), 2.5, 1.95)

# Fig 8: calibration by decile of the prediction
cal <- bind_rows(lapply(names(pred)[-1], function(m) data.frame(model = model_names[[m]], p = pred[[m]], y = y_te))) |>
  mutate(model = factor(model, model_names[-1])) |> group_by(model) |> mutate(bin = ntile(p, 10)) |>
  group_by(model, bin) |> summarise(predicted = mean(p), observed = mean(y), .groups = "drop")
save_fig("fig08_calibration.png",
         ggplot(cal, aes(predicted, observed)) + geom_abline(linetype = 2, colour = grey, linewidth = 0.3) +
           geom_line(colour = ink, linewidth = 0.4) + geom_point(colour = ink, size = 0.8) + facet_wrap(~ model, nrow = 1) +
           scale_x_continuous(labels = pct, limits = c(0, 0.85), breaks = c(0, 0.4, 0.8)) +
           scale_y_continuous(labels = pct, limits = c(0, 0.85), breaks = c(0, 0.4, 0.8)) +
           coord_equal() + theme(panel.spacing = unit(1.2, "lines")) +
           labs(x = "mean predicted probability", y = "observed summit rate"), 5.6, 1.9)

# Fig 9: observed and predicted summit rate by decile of the boosting prediction
dec <- data.frame(p = pred$boosting, y = y_te) |> mutate(decile = ntile(p, 10)) |> group_by(decile) |>
  summarise(predicted = mean(p), observed = mean(y))
save_fig("fig09_deciles.png",
         ggplot(dec, aes(factor(decile), observed)) + geom_col(fill = light, width = 0.65) +
           geom_hline(yintercept = mean(y_te), linetype = 2, colour = grey, linewidth = 0.3) +
           geom_point(aes(y = predicted), colour = accent, size = 1.3) +
           geom_text(aes(y = pmax(observed, predicted), label = pct(observed)), vjust = -0.9) +
           scale_y_continuous(labels = pct, limits = c(0, 0.85), expand = c(0, 0)) +
           labs(x = "decile of predicted probability (boosting)", y = "summit rate"), 3.5, 2.1)

# Fig 17: ROC curves, and the boosting prediction by observed outcome
roc_df <- function(p, y) { o <- order(-p)
  data.frame(fpr = c(0, cumsum(1 - y[o]) / sum(1 - y)), tpr = c(0, cumsum(y[o]) / sum(y))) }
roc <- bind_rows(lapply(names(pred)[-1], function(m)
  data.frame(model = model_names[[m]], roc_df(pred[[m]], y_te)))) |> mutate(model = factor(model, model_names[-1]))
d1 <- ggplot(roc, aes(fpr, tpr, colour = model, linetype = model)) +
  geom_abline(linetype = 2, colour = light, linewidth = 0.3) + geom_line(linewidth = 0.45) +
  scale_colour_manual(values = model_cols) +
  scale_linetype_manual(values = c("Elastic net" = 1, "Pruned tree" = 1, "Random forest" = 1, "Boosting" = 1)) +
  coord_equal() + theme(legend.position = "inside", legend.position.inside = c(0.66, 0.24)) +
  labs(title = "(a) ROC curves", x = "1 - specificity", y = "sensitivity", colour = NULL, linetype = NULL)
d2 <- data.frame(p = pred$boosting, outcome = factor(y_te, 0:1, c("no summit", "summit"))) |>
  ggplot(aes(outcome, p)) +
  geom_boxplot(fill = "white", colour = ink, width = 0.45, linewidth = 0.35, outlier.size = 0.2, outlier.colour = grey) +
  scale_y_continuous(labels = pct, limits = c(0, 1)) +
  labs(title = "(b) Boosting", x = NULL, y = "predicted probability")
save_fig("fig17_discrimination.png", d1 + d2 + plot_layout(widths = c(1.35, 1)), 3.5, 2.0)

# Fig 18: elastic-net coefficient paths, the six largest non-spline terms labelled at the right end
fit_en <- cv_en$glmnet.fit
path <- as.data.frame(t(as.matrix(fit_en$beta))) |> mutate(loglambda = log(fit_en$lambda)) |>
  pivot_longer(-loglambda, names_to = "term", values_to = "beta")
b_1se <- as.matrix(coef(cv_en, s = "lambda.1se"))[-1, 1]
show  <- names(sort(abs(b_1se[!grepl("ns\\(", names(b_1se))]), decreasing = TRUE))[1:6]
nice  <- function(x) x |> sub("^season", "season: ", x = _) |> sub("^peak", "peak: ", x = _) |>
  sub("^citizenship", "citizenship: ", x = _) |> sub("^sexM", "male", x = _) |> sub("^rolele", "role: le", x = _) |>
  sub("^log1p\\((.*)\\)$", "log(1 + \\1)", x = _) |> sub("staff_ratio", "staff per climber", x = _)
path_show <- filter(path, term %in% show) |> mutate(label = nice(term))
ends <- path_show |> filter(loglambda == min(loglambda)) |> arrange(beta)
ends$y <- ends$beta
for (i in seq_len(nrow(ends))[-1]) ends$y[i] <- max(ends$y[i], ends$y[i - 1] + 0.13 * diff(range(path$beta)))
save_fig("fig18_en_path.png",
         ggplot(path, aes(loglambda, beta, group = term)) +
           geom_line(colour = light, linewidth = 0.25) +
           geom_line(data = path_show, colour = ink, linewidth = 0.45) +
           geom_vline(xintercept = log(c(cv_en$lambda.min, cv_en$lambda.1se)), linetype = c(3, 2), linewidth = 0.3) +
           geom_segment(data = ends, aes(x = loglambda - 0.1, xend = loglambda - 0.75, y = beta, yend = y),
                        inherit.aes = FALSE, colour = grey, linewidth = 0.25) +
           geom_text(data = ends, aes(x = loglambda - 0.85, y = y, label = label), hjust = 0, size = 2.1) +
           scale_x_reverse(expand = expansion(mult = c(0.02, 0.75))) +
           labs(x = expression(log(lambda)), y = "coefficient"), 3.1, 1.85)


# =====================================================================================
# Insights from the final model
# =====================================================================================
# Fig 10: permutation importance of groups of predictors
block_lab <- c("When: year" = "Year", "Where: peak and its height" = "Peak", "Support: hired staff, commercial" = "Hired support",
               "When: season" = "Season", "Team size" = "Team size", "Who: age, sex, citizenship, role" = "Climber")
save_fig("fig10_importance.png",
         ggplot(imp_block, aes(increase, reorder(block_lab[block], increase))) + geom_col(fill = grey, width = 0.6) +
           geom_text(aes(label = sprintf("%.3f", increase)), hjust = -0.2) +
           scale_x_continuous(expand = expansion(mult = c(0, 0.3)), n.breaks = 3) +
           labs(x = "increase in test log loss", y = NULL), 2.1, 2.0)

# Fig 11: partial dependence of the boosting prediction
pd <- function(v) { g <- plot(gb, i.var = v, n.trees = gb$n.trees, return.grid = TRUE, type = "response")
                    data.frame(var = v, x = as.character(g[[1]]), p = g$y) }
pd_num <- bind_rows(lapply(c("year", "age", "staff_ratio"), pd)) |> mutate(x = as.numeric(x)) |>
  filter((var == "year" & x >= 1960) | (var == "age" & x >= 18 & x <= 70) | (var == "staff_ratio" & x <= 2)) |>
  mutate(var = factor(var, c("year", "staff_ratio", "age"), c("Year", "Staff ratio", "Age")))
p1 <- ggplot(pd_num, aes(x, p)) + geom_line(colour = ink, linewidth = 0.45) + facet_wrap(~ var, scales = "free_x") +
  scale_y_continuous(labels = pct, limits = c(0.1, 0.55), breaks = c(0.1, 0.3, 0.5)) +
  scale_x_continuous(breaks = function(l) if (l[2] > 1900) c(1970, 1995, 2020) else if (l[2] > 10) c(20, 40, 60) else c(0, 1, 2)) +
  labs(x = NULL, y = "predicted probability") + theme(panel.spacing = unit(0.8, "lines"))
pd_cat <- bind_rows(lapply(c("peak", "season"), pd)) |> mutate(var = factor(var, c("peak", "season"), c("Peak", "Season")))
p2 <- ggplot(pd_cat, aes(p, reorder(x, p))) +
  geom_segment(aes(x = 0.1, xend = p, yend = reorder(x, p)), colour = light, linewidth = 0.3) +
  geom_point(colour = ink, size = 0.9) + facet_wrap(~ var, scales = "free_y") +
  scale_x_continuous(labels = pct, limits = c(0.1, 0.55), breaks = c(0.1, 0.3, 0.5)) + labs(x = "predicted probability", y = NULL) +
  theme(panel.spacing = unit(0.8, "lines"), axis.text.y = element_text(size = 6))
save_fig("fig11_partial_dependence.png", p1 / p2 + plot_layout(heights = c(1, 1.5)), 3.6, 2.45)

# Fig 12: the first splits of the pruned tree
tree_top <- prune(tree, cp = tree$cptable[max(which(tree$cptable[, "nsplit"] <= 4)), "CP"])
tree_top$frame$var <- sub("staff_ratio", "staff per climber", tree_top$frame$var)
ragg::agg_png("figures/fig12_tree.png", width = 3.2, height = 2.1, units = "in", res = 400)
par(family = font)
rpart.plot(tree_top, type = 2, extra = 107, box.palette = "Grays", roundint = FALSE, tweak = 1.05, branch.lwd = 0.8)
invisible(dev.off())

# Figs 13 and 19: impurity-based vs permutation importance (random forest, boosting)
imp_plot <- function(imp, order_by, fills) imp |> pivot_longer(-variable) |>
  ggplot(aes(value, factor(variable, imp$variable[order(imp[[order_by]])]), fill = name)) +
  geom_col(position = "dodge", width = 0.75) + scale_fill_manual(values = fills) +
  scale_x_continuous(expand = c(0, 0)) + theme(legend.position = "top", legend.justification = "left") +
  labs(x = "importance (largest = 1)", y = NULL, fill = NULL)
save_fig("fig13_importance_rf.png", imp_plot(imp_rf, "permutation", c(gini = light, permutation = ink)), 2.75, 2.35)
save_fig("fig19_importance_gbm.png",
         imp_plot(rename(imp_gb, `relative influence` = relative_influence), "relative influence",
                  c(`relative influence` = light, permutation = ink)), 2.75, 2.35)

# Fig 14: one climber, different plans
save_fig("fig14_plans.png",
         ggplot(plans, aes(p, factor(label, rev(label)))) + geom_col(fill = grey, width = 0.6) +
           geom_text(aes(label = pct(p)), hjust = -0.25) +
           scale_x_continuous(labels = pct, limits = c(0, 1), expand = expansion(mult = c(0, 0.04))) +
           labs(x = "predicted summit probability", y = NULL), 5.6, 2.1)


# Fig 20: is hired staff only a proxy for the era or for commercial expeditions?
st <- raw_d |> mutate(type = ifelse(commercial == 1, "commercial", "non-commercial"))
s1 <- st |> group_by(year, type) |> summarise(n = n(), staff = mean(staff_ratio), .groups = "drop") |> filter(n >= 30) |>
  ggplot(aes(year, staff, colour = type)) + geom_line(linewidth = 0.45) +
  scale_colour_manual(values = c(commercial = accent, `non-commercial` = grey)) +
  theme(legend.position = "inside", legend.position.inside = c(0.3, 0.85)) +
  labs(title = "(a) Hired staff per climber over time", x = NULL, y = "mean staff per climber", colour = NULL)
s2 <- st |> filter(year >= 2000) |>
  mutate(g = cut(staff_ratio, c(-Inf, 0, .5, 1, Inf), labels = c("0", "<.5", ".5-1", ">1"))) |>
  group_by(type, g) |> summarise(n = n(), success = mean(success == "yes"), .groups = "drop") |> filter(n >= 50) |>
  ggplot(aes(g, success, fill = type)) + geom_col(position = position_dodge(0.75), width = 0.7) +
  scale_fill_manual(values = c(commercial = accent, `non-commercial` = grey), guide = "none") +
  scale_y_continuous(labels = pct, limits = c(0, 0.7), expand = c(0, 0)) +
  labs(title = "(b) Summit rate since 2000, by staff per climber", x = "hired staff per climber", y = NULL)
save_fig("fig20_staff.png", s1 + s2, 5.6, 2.0)

# =====================================================================================
# Robustness
# =====================================================================================
# Fig 15: leakage check
lk <- data.frame(model = factor(rownames(leak_metrics), rev(rownames(leak_metrics))), auc = leak_metrics[, "auc"])
save_fig("fig15_leakage.png",
         ggplot(lk, aes(auc, model, fill = model == "Pre-climb variables only")) + geom_col(width = 0.6) +
           geom_text(aes(label = sprintf("%.3f", auc)), hjust = -0.2) +
           scale_fill_manual(values = c(light, ink), guide = "none") + coord_cartesian(xlim = c(0.5, 0.9)) +
           scale_x_continuous(expand = c(0, 0)) + labs(x = "test AUC", y = NULL), 2.7, 1.15)

# Fig 16: time split, observed vs predicted summit rate per year
ts <- data.frame(year = future$year, Observed = y_fut, `Predicted (boosting)` = pred_fut$boosting, check.names = FALSE) |>
  group_by(year) |> summarise(across(everything(), mean)) |> pivot_longer(-year)
save_fig("fig16_time_split.png",
         ggplot(ts, aes(year, value, colour = name, linetype = name)) + geom_line(linewidth = 0.5) + geom_point(size = 0.9) +
           geom_text(data = filter(ts, year == 2019), aes(label = name), hjust = 0, nudge_x = 0.15) +
           scale_colour_manual(values = c(Observed = ink, `Predicted (boosting)` = accent), guide = "none") +
           scale_linetype_manual(values = c(Observed = 1, `Predicted (boosting)` = 2), guide = "none") +
           scale_y_continuous(labels = pct, limits = c(0, 0.75)) +
           scale_x_continuous(limits = c(2015, 2021.8), breaks = 2015:2019) +
           labs(x = NULL, y = "summit rate"), 2.7, 1.55)

cat("Figures written to figures/\n")
