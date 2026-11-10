# SML Project – Round 2: Creative Ideas That Use the Whole Course (Units 1–8)

Companion to `dataset_ideas.md` (round 1). This file adds:
- **Part A:** a map of *every* unit (slides 1–8) to a concrete project component, with R functions
- **Part B:** 11 new, more creative datasets (verified), each showing which units it uses best
- **Part C:** "twists" that work with any dataset and make the talk memorable
- **Part D:** final ranking across round 1 + round 2

"Checked" = downloaded and inspected in R on 2026-11-06. "Verified" = facts confirmed from the official source page.

---

## Part A — The full-course toolbox (what each unit gives us)

| Unit | Content (from slides) | How to use it in the project | R |
|---|---|---|---|
| **1 Intro** | Least squares vs k-NN, statistical decision theory, loss functions (0-1, squared, deviance), curse of dimensionality, bias–variance, **double descent** | k-NN and a linear model as **baselines**. Justify the loss function. Show k-NN degrading when noise features are added (curse of dimensionality). Optional: mini double-descent experiment with min-norm least squares | `class::knn`, `FNN::knn.reg`, `MASS::ginv` |
| **2 Linear models** | OLS, **subset selection**, **ridge**, **lasso**, **elastic net**, LAR, coordinate descent, **degrees of freedom**, group lasso, SCAD | Coefficient paths. Lasso vs elastic net when predictors are correlated. Best subset for small p. Report df(λ). **Group lasso** for categorical variables (the slides' "Extensions") | `leaps::regsubsets`, `glmnet`, `lars`, `grpreg` |
| **3 GLMs** | Logit/probit, latent variable view, interpretation (odds ratios), deviance, **(quasi-)complete separation**, exponential family, **regularized GLMs** | Pick the right family: binomial / **Poisson** / multinomial. Interpret odds ratios. Check for separation (and say how penalization fixes it). Regularized GLM as the interpretable benchmark | `glm`, `glmnet(family=…)` |
| **4 Assessment & selection** | Bias–variance, optimism, **Cp/AIC/BIC**, effective number of parameters, **CV** (choice of K), **bootstrap / .632**, accuracy/sensitivity/specificity/F1/kappa, **ROC/AUC**, **calibration**, **Brier score**, **log score**, c-index | Strict **train/test separation**: tune inside CV, assess once on the test set. Compare CV vs OOB vs .632 bootstrap. Report AUC **and** Brier / log score **and** a calibration plot. **One-SE rule**. Bootstrap CI for test metrics | `caret`, `mlr3`, `boot`, `ROCR`/`pROC` |
| **5 Trees** | CART, cost-complexity pruning, Gini/deviance, categorical predictors, **loss matrix**, **missing values / surrogate splits**, ctree | Pruned tree as the interpretable model (show the cp plot). **Loss matrix** for asymmetric costs. Surrogates when NAs exist. `ctree` vs `rpart` (variable selection bias) | `rpart(parms=list(loss=…))`, `partykit::ctree` |
| **6 Bagging / RF** | Bagging, why it works, decorrelated trees, **OOB error**, **Gini vs permutation importance** (bias toward continuous / many-level variables) | Bagging = RF with mtry = p, so show how mtry decorrelates the trees. OOB vs CV. **Show Gini importance bias** next to permutation importance (a direct slide topic) | `randomForest(importance=TRUE)`, `ranger` |
| **7 Boosting** | **AdaBoost**, exponential vs deviance loss, robustness, **Huber**, gradient boosting, tree size, **shrinkage ν vs M**, subsampling, **relative influence**, **partial dependence** | Tune depth × ν × M (heatmap). AdaBoost vs Bernoulli deviance (robustness to label noise). Huber loss for regression with outliers. **PDPs** for interpretation. `mboost` with linear/spline base learners | `gbm(distribution=…)`, `mboost`, `xgboost` |
| **8 Neural nets** | Single hidden layer, back-propagation, **starting values**, **weight decay**, **early stopping**, **input scaling**, **multiple minima**, number of units, dropout/SGD, CNN/RNN | Tune size × decay. **Standardize inputs**. Run several random starts and average (multiple minima). Early stopping on a validation set | `nnet`, `keras`/`torch` |

**Practical rule for 15 minutes:** the main story uses 4–5 models (e.g. k-NN baseline → elastic-net GLM →
pruned tree → RF → boosting → NN). Each unit's "special topic" (double descent, loss matrix, Gini bias,
AdaBoost vs deviance, multiple minima, …) gets **one backup slide**. Many of these are also **exam questions**
(Q3, Q5–Q12 in last year's theory list), so the project doubles as exam preparation.

---

## Part B — 11 new creative ideas

### B1. ⚽ Expected goals (xG): what is the probability that a shot becomes a goal? ★★★ my favourite
- **Task:** Binary classification (goal / no goal), and the **probability** is what matters.
- **Data (checked):** StatsBomb Open Data (free, JSON, via `StatsBombR`). **80 competition-seasons, 3,961 free matches**
  (Champions League, La Liga incl. the Messi years, World Cups 2018 and 2022, Euros, Bundesliga, women's leagues, …).
  About 12–25 shots per match, so **roughly 50–90k shots**. Each shot has location, body part, technique, shot type
  (open play / free kick / penalty), first-time, under pressure, and a **freeze frame** (positions of the keeper and
  all nearby defenders). It also includes **`statsbomb_xg`, the professional model's prediction, which is a built-in benchmark.**
- **Feature engineering (the fun part):** distance and angle to goal, number of defenders in the shooting
  triangle, keeper distance, header vs foot, assisted by a through ball/cross.
- **Units used:** U3 logistic GLM (the industry baseline). U2 lasso/elastic net with spline features of distance/angle.
  U4 **calibration, Brier score, log score** (xG must be calibrated, so this is exactly the "probabilistic predictions"
  slides). U5–7 trees/RF/GBM with **PDP of distance × angle**. U8 NN. U1 k-NN on (x, y) location as a baseline.
- **Results you can show:** (1) your xG vs StatsBomb's xG (log loss / Brier / AUC), (2) an xG map of the pitch,
  (3) **player finishing skill = goals − xG** (e.g. Messi over-performance), (4) model calibration plot.
- **Pitfalls:** exclude penalties (or model them separately: xG ≈ 0.76 constant). Never use outcome-derived
  fields (end_location, goalkeeper outcome). **Grouped CV by match.** Data skews toward Barcelona / Messi
  (free La Liga data is Barça matches), so mention it as a limitation.
- **Effort:** medium (JSON → table with StatsBombR in about 1 hour). Licence: free for research with attribution.

### B2. 🔭 MAGIC gamma telescope: cosmic gamma rays vs hadron background ★★ uses all units neatly
- **Task:** Binary classification with **asymmetric costs**.
- **Data (checked):** 19,020 events × 10 image features (Hillas parameters), **0 NA**, 115 duplicate rows.
  12,332 gamma / 6,688 hadron. UCI ID 159, CC BY 4.0.
- **Unique hook:** the dataset's own documentation says **classifying a hadron as gamma is worse**, and asks
  for evaluation via the **ROC at FPR = 0.01, 0.02, 0.05, 0.1, 0.2**. That maps straight onto
  **U5 loss matrix** and **U4 ROC / threshold choice**.
- **Units:** U1 k-NN (only 10 features, so k-NN is competitive, which is an interesting point), U3 logistic, U5 loss-matrix tree,
  U6 RF, U7 **AdaBoost vs gradient boosting** (exponential vs deviance), U8 NN.
- **Expected:** boosting/RF AUC ≈ 0.93–0.94; logistic clearly worse (non-linear physics).
- **Risk:** very low. Story: "a telescope needs a filter that almost never lets background through".

### B3. 🎮 Dota 2: can the hero draft predict who wins?
- **Task:** Binary classification.
- **Data (checked):** UCI ID 367, **92,650 train / 10,294 test** games, 113 hero columns coded +1 / −1 / 0
  (2 heroes never appear), plus game mode/type/cluster. **0 NA**, 52.7% team-1 wins.
- **Units:** U2/U3 **lasso logistic: coefficients = hero strength ranking** (very interpretable). U1 sparse high-dim.
  U7 boosting and U8 NN test whether **hero synergies (interactions)** add anything. U4 bootstrap CI of the accuracy gain.
- **Expected and honest result:** accuracy ≈ 58–60%. The draft matters but explains little, and the linear model may
  match the complex ones. Presenting that properly is a strong "insight on the final model".
- **Risk:** low. Fun, distinctive topic.

### B4. 🕹️ League of Legends: who wins, judging from the first 10 minutes?
- **Task:** Binary classification. **Data:** 9,879 high-elo ranked games, 38 team stats at minute 10 + `blueWins`
  (Kaggle "LoL Diamond Ranked Games (10 min)", also on OpenML ID 43635).
- **Unique hook (Unit 2/3):** the data contains **exact linear dependencies** (blueGoldDiff = blueTotalGold − redTotalGold,
  blueKills = redDeaths, …). Plain logit returns NA / aliased coefficients, which demonstrates why ridge/lasso and
  variable selection exist.
- **Expected:** accuracy ≈ 72–73%. Gold and XP difference dominate (PDP shows the S-curve).
- **Risk:** low. Small and clean, but less original than Dota.

### B5. ⚛️ Higgs boson: do neural nets learn the physicists' features?
- **Task:** Binary classification (signal vs background), 11 million simulated LHC collisions (UCI ID 280, CC BY 4.0).
  **Subsample 100–200k.**
- **Experiment:** 21 low-level detector features vs 7 high-level, physicist-engineered features vs all 28.
  Baldi, Sadowski & Whiteson (2014, *Nature Communications*) found that deep nets on raw features
  match or beat hand-crafted features. Can you reproduce that with `nnet`/`keras` vs `gbm`?
- **Units:** U8 is front and centre (scaling, weight decay, size, multiple minima), U7 boosting as rival, U2 lasso baseline.
- **Caveat:** Monte-Carlo simulated (but it is the real physics simulation). The NN needs compute. Good if you like NNs.

### B6. 📱 Human Activity Recognition: what is the person doing, from phone sensors?
- **Task:** 6-class classification (walking, upstairs, downstairs, sitting, standing, lying).
- **Data:** UCI ID 240, 10,299 windows × **561 features**, 30 people, official train/test split **by person**.
- **Units:** U1 curse of dimensionality (k-NN with 561 features), U2 multinomial lasso (how many of 561 do we need?),
  U6 RF importance, U8 NN. U4: **why a random split would be over-optimistic** (same person in train and test).
- **Expected:** ~95% accuracy; sitting vs standing is the hard pair (confusion matrix story).
- **Risk:** low. Very clean.

### B7. 🎵 Guess the decade: predict a song's release year from its sound
- **Task:** Regression (year 1922–2011). **Data:** Million Song Dataset subset, UCI ID 203, 515,345 songs × 90 timbre
  features, **no NA**. Official split (first 463,715 train) **avoids the "producer effect"** (no artist in both sets).
- **Units:** U2 lasso baseline, U7 GBM + **Huber loss** (old songs are outliers, which links to the robustness slides),
  U8 NN, U1 k-NN. U4: an MAE vs RMSE discussion.
- **Insight:** which timbre dimensions changed over time (loudness war!). **Risk:** low; subsample for speed.

### B8. 🧬 Cancer type from 20,531 genes (p ≫ n)
- **Task:** 5-class classification (breast, kidney, colon, lung, prostate tumours). **Data:** UCI ID 401, **801 patients ×
  20,531 genes**, no NA.
- **Why creative:** the textbook **p ≫ n** case. U2 elastic net vs lasso (correlated genes → grouping effect),
  U1 curse of dimensionality + **double descent**, U3 **(quasi-)complete separation** (classes are almost perfectly separable,
  so unpenalized multinomial logit breaks; show it).
- **Question:** "How few genes do we need?" Plot accuracy vs number of genes selected by the lasso
  (~99% accuracy with a handful). **Risk:** accuracy is near-perfect, so the story must be sparsity/interpretation.

### B9. ♟️ How hard is a chess puzzle?
- **Task:** Regression: puzzle **Rating** (Glicko-2 difficulty). **Data (verified):** Lichess puzzle database,
  **6,057,356 puzzles** (updated 2026-07-29), CC0. Columns: FEN, Moves, Rating, RatingDeviation, Popularity, NbPlays,
  **Themes** (fork, pin, sacrifice, mateIn2, quietMove, zugzwang, …), OpeningTags.
- **Features:** about 60 theme dummies, solution length, material count from FEN, opening.
- **Units:** U2 lasso on theme dummies ("which tactics are hardest?"), U5 tree, U6 RF, U7 GBM + PDP,
  U3/U4 use **RatingDeviation** to filter or weight reliable labels. Subsample 50–100k.
- **Risk:** low–medium (a little feature engineering). Very original.

### B10. 🏔️ Avalanche danger level from weather and snowpack (Swiss Alps)
- **Task:** Ordinal classification (danger levels 1–4). **Data (verified):** EnviDat DOI 10.16904/envidat.330,
  Pérez-Guillén et al. (2022, NHESS): IMIS station weather + simulated snow profiles + official
  bulletin danger levels, 1997–2020, ~180 stations. A follow-up NN paper reports ~77% validation accuracy.
- **Units:** U5 **loss matrix: under-forecasting danger is far worse than over-forecasting**, U6 RF (used in the
  original paper → benchmark), U7 boosting, U8 NN, U4 split **by winter season** (no time leakage), U3 multinomial.
- **Why great:** Alpine, Austria-relevant, life-and-death cost asymmetry.
- **Risk:** medium. A larger download and the WSL data licence must be checked; level 4 is rare.

### B11. 🌌 How far away is that galaxy? Photometric redshift from 5 colours
- **Task:** Regression (redshift) from u, g, r, i, z magnitudes, or 3-class star/galaxy/quasar **without** redshift.
- **Data:** SDSS DR17, 100,000 objects (Kaggle "Stellar Classification SDSS17", public domain).
- **Units:** U1 **k-NN is the classic photo-z method** (great baseline story), U6 RF, U7 GBM, U8 NN; colour indices
  (u−g, g−r, …) as engineered features.
- **Caveat:** redshift almost determines the class, so the classification version must exclude it.

### Also considered (not verified, more effort)
- **Vienna real-estate transactions (Kaufpreissammlung, Stadt Wien, CC BY):** very local, but I couldn't reach the
  download today, so it's an unknown risk.
- Kickstarter success (strong leakage story), EA FC player market value, UK used-car prices, Olympic medals,
  Open Food Facts NOVA processing level: all interesting but messier.

---

## Part C — Twists that work with any dataset
1. **Cost-sensitive decision (U5 loss matrix + U4):** turn probabilities into actions with a cost matrix and choose the threshold.
2. **Proper scoring (U4):** report Brier and log score, not only accuracy; show a calibration plot per model.
3. **Bootstrap the test set (U4):** a 95% CI for each model's test metric. Is the "best" model really better?
4. **One-SE rule (U2/U4/U5):** a simpler lasso / smaller tree with almost the same error.
5. **CV vs OOB vs .632 bootstrap (U4/U6):** three error estimates side by side.
6. **Gini vs permutation importance (U6):** show the bias the slides warn about on your own data.
7. **Shrinkage ν × iterations M heatmap (U7)** and **AdaBoost vs deviance under 10% label noise (U7 robustness).**
8. **NN multiple minima (U8):** 10 random starts → spread of test error → averaging helps.
9. **Learning curve:** test error vs training size n for each model (where does flexibility pay off?).
10. **Double-descent mini-demo (U1):** min-norm least squares with p growing past n on your features (backup slide).
11. **Group lasso (U2 extensions)** when you have many-level categoricals.
12. **Interpretation triangle:** lasso coefficients vs tree splits vs GBM PDPs. Do they tell the same story?

---

## Part D — Final ranking (round 1 + round 2)

| Rank | Project | Why | Effort | Risk |
|---|---|---|---|---|
| 1 | **⚽ xG football (B1)** | Most memorable. Built-in professional benchmark. Calibration-centric (U4). Every unit fits | Medium | Low |
| 2 | **🚗 Insurance claim frequency (round 1, #1)** | Business/WU fit. Poisson regularized GLM (U3). Published benchmarks | Medium | Low |
| 3 | **🔭 MAGIC telescope (B2)** | Cleanest way to use the loss matrix + ROC + AdaBoost; tiny and safe | Low | Very low |
| 4 | **🏔️ Avalanche danger (B10)** | Alpine, cost asymmetry, RF benchmark | Medium–high | Medium |
| 5 | **🎮 Dota 2 draft (B3)** | Fun, interpretable lasso, honest "complexity doesn't help" result | Low | Low |
| 6 | **♟️ Chess puzzle difficulty (B9)** | Very original regression | Medium | Low |
| 7 | **🎓 Student dropout (round 1, #4)** | Safe, policy-relevant | Low | Very low |

## Sources
- StatsBomb open data: https://github.com/statsbomb/open-data · StatsBombR: https://rdrr.io/github/statsbomb/StatsBombR/f/README.md
- MAGIC gamma: https://archive.ics.uci.edu/dataset/159 · cost note: https://rdrr.io/cran/evtree/man/MAGICGammaTelescope.html
- Dota 2: https://archive.ics.uci.edu/dataset/367/dota2+games+results
- LoL 10 min: https://api.openml.org/d/43635
- Higgs: https://archive.ics.uci.edu/dataset/280/higgs
- HAR smartphones: https://archive.ics.uci.edu/dataset/240
- YearPredictionMSD: https://archive.ics.uci.edu/dataset/203/yearpredictionmsd
- Cancer RNA-Seq: https://archive.ics.uci.edu/dataset/401/gene+expression+cancer+rna+seq
- Lichess puzzles: https://database.lichess.org/
- Avalanche data: https://envidat.ch/dataset/54d7dc19-e70c-4998-bfb6-caffe41c83e6 · NN follow-up: https://nhess.copernicus.org/articles/23/2523/2023/
- SDSS17: https://huggingface.co/datasets/Allanatrix/Astro/blob/main/README.md
