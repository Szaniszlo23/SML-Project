# SML Course Project – Dataset & Project Ideas

## What the project requires (from `project/projects-1.pdf`, `slides00.pdf`)
- Regression **or** classification task, application context, EDA, preprocessing / variable selection
- **≥ 3 methods from class, with hyperparameter tuning**. Methods covered in class: lasso / elastic net, regularized GLMs, CV and bootstrap, trees, bagging / random forests, boosting, neural nets
- Model selection and assessment, plus insights on the final model
- 15 min talk + 5 min Q&A on **2026-11-17**; PDF slides due **2026-11-16**; backup slides recommended
- Grading: dataset and method motivation 20% · correctness 30% · clarity 30% · Q&A 20%
- Course R stack: `glmnet`, `rpart`/`tree`/`partykit`, `randomForest`, `gbm`/`mboost`, `nnet`, `caret`/`mlr3`
- **Don't reuse class datasets:** prostate, SAheart, spam, diabetes (lars), Bikeshare, Wage, icu

Selection criteria: (1) clean and well documented, (2) a real application context, (3) room for a
"story" (leakage, interpretation, a benchmark to compare against), (4) fits the course methods.

"Checked" = I downloaded the dataset and verified it in R on 2026-11-06.

---

## Tier 1: Top recommendations

### 1. Car insurance pricing: French motor claim frequency (`freMTPL2freq`) ★ most distinctive
- **Task:** Regression on a count: number of claims per policy (Poisson), with `log(Exposure)` as an offset.
- **Data (checked):** 678,013 policies × 12 columns, **0 missing values**. Columns: ClaimNb, Exposure (policy-years),
  Area, VehPower, VehAge, DrivAge, BonusMalus, VehBrand, VehGas, Density, Region.
  95% of policies have 0 claims; overall frequency ≈ 0.10 claims per policy-year.
- **Source:** CASdatasets (Charpentier, *Computational Actuarial Science with R*); OpenML ID 41214. It is the
  standard benchmark in actuarial ML (Wüthrich, Noll, Salzmann, …).
- **Why it fits:** It maps directly onto the *regularized GLM* unit (Poisson lasso/elastic net in `glmnet`
  with an offset). `rpart(method="poisson")` and `gbm(distribution="poisson")` support it natively.
  It's also a business application, which suits WU.
- **Methods:** Poisson elastic net (with splines or binned age) → Poisson tree → gbm (Poisson) → nnet / random forest.
- **What to analyse:** The young driver effect, the shape of the bonus–malus effect, the non-linear density effect,
  interactions found by boosting (partial dependence plots), and the tariff relativities each model implies.
- **Expected results:** Boosting improves test Poisson deviance over the GLM by a few percent. Published
  benchmarks: null 0.4865, GLM 0.4662, boosting 0.4572 (test deviance). You can say "we reproduced the literature".
- **Cleaning needed (light):** cap Exposure at 1 (1,224 rows), cap ClaimNb at about 4, cap DrivAge/VehAge outliers,
  log(Density). Subsample (e.g. 100k rows) for tuning because of the size.
- **Risk:** Medium. You need to explain offsets and deviance clearly. Low "R²" is normal here, so present the
  results as deviance improvement and lift / Gini.

### 2. Predicting a superconductor's critical temperature ★ cleanest data
- **Task:** Regression: critical temperature Tc (Kelvin) from chemical composition features.
- **Data (checked):** 21,263 materials × 81 numeric features, **0 missing values**. Tc ranges 0–185 K
  (median 20). There are **30 feature pairs with |r| > 0.95**, a textbook case for elastic net vs lasso.
- **Source:** UCI ID 464, Hamidieh (2018), CC BY 4.0.
- **Methods:** Lasso vs elastic net (collinearity → choosing alpha), random forest (mtry), gbm (depth, shrinkage, trees), nnet.
- **Published benchmarks:** linear regression RMSE ≈ 17.6 K, XGBoost RMSE ≈ 9.5 K (R² ≈ 0.92).
  You can show whether you match them.
- **Interesting angle:** the same chemical formula appears several times (`unique_m.csv`). Splitting **by formula**
  instead of randomly avoids optimistic error estimates, which is good for the "correctness" grade.
  Also compare lasso stability against elastic net when features are correlated.
- **Insights:** Thermal conductivity and atomic-mass/radius features dominate. Show variable importance and partial dependence.
- **Risk:** Low. The only drawback is that features are engineered aggregates (mean, entropy, …), which are less intuitive.

### 3. Hotel booking cancellations (business classification)
- **Task:** Binary classification: will a booking be cancelled?
- **Data (checked):** 119,390 bookings × 32 columns, from two Portuguese hotels (city and resort), arrivals 2015–2017.
  Cancellation rate: 41.7% (city), 27.8% (resort). Only 4 NAs (children). Agent = "NULL" in 14% and company = "NULL"
  in 94% of rows (these mean "none"). 1,960 rows have adr ≤ 0. **31,994 exact duplicate rows**, probably group
  bookings; decide how to handle them and say so.
- **Source:** Antonio, de Almeida & Nunes (2019), *Data in Brief* 22:41–49, CC BY 4.0; TidyTuesday 2020-02-11 CSV.
- **Methods:** Logistic lasso, classification tree, random forest, gbm, nnet.
- **Strong story: data leakage.** Drop `reservation_status` and `reservation_status_date` (they *are* the outcome), and
  think carefully about `assigned_room_type`. Also explain the "non-refundable deposits cancel more" paradox.
  Examiners like seeing this kind of reasoning.
- **Evaluation:** Time-based split (train 2015–16, test 2017), AUC, and a cost-based threshold (overbooking policy).
- **Expected:** AUC ≈ 0.90+. Key drivers: lead time, deposit type, country, special requests, previous cancellations.
- **Risk:** Low. It is popular on Kaggle, so make it distinctive through the leakage, time-split, and cost angles.

### 4. Student dropout vs. graduation (Portugal)
- **Task:** 3-class classification (Dropout / Enrolled / Graduate), or binary Dropout vs. not.
- **Data (checked):** 4,424 students × 36 features, **0 missing values**. Classes: Dropout 1,421 · Enrolled 794 · Graduate 2,209.
  Features include demographics, parents' education and occupation, scholarship, debtor, tuition paid, admission grade,
  1st and 2nd semester credits and grades, and macro conditions (unemployment, inflation, GDP).
- **Source:** UCI ID 697, Realinho et al. (2021), CC BY 4.0.
- **Methods:** Multinomial elastic net, tree, random forest, gbm (multinomial), nnet.
- **Interesting angle:** Fit **two models**: (a) at enrollment only, and (b) after semester 1. This quantifies how much
  an early-warning system gains from waiting one semester. The result is concrete and policy-relevant.
- **Expected:** Accuracy ≈ 0.75–0.78. "Enrolled" is hard to predict, which the confusion matrix will show.
  Semester-1 grades, tuition up to date, and scholarship status dominate.
- **Cleaning:** Categorical columns are integer codes. Convert them to factors and group rare levels
  (e.g. parents' occupation has many codes).
- **Risk:** Low. It is relatable to students, so the motivation is easy.

### 5. Corporate bankruptcy prediction (Taiwan)
- **Task:** Binary classification, very imbalanced (220 bankrupt / 6,819 = 3.2%).
- **Data:** 95 financial ratios, no missing values. Taiwan Economic Journal, 1999–2009. UCI ID 572, CC BY 4.0.
- **Why it fits:** 95 correlated ratios make lasso variable selection a natural story ("which 8 ratios matter?").
  It is a WU finance context.
- **Methods:** Logistic lasso, random forest (class weights), gbm, nnet. Use repeated stratified CV (few events).
- **Evaluate with:** PR-AUC, recall at a fixed false-positive rate, and calibration. Don't use accuracy (96.8% is free).
- **Watch:** a few columns have strange scales or extreme values, and one column is constant (Net Income Flag).
  Check these in EDA.
- **Risk:** Medium. The small number of events means noisy estimates, which you need to discuss honestly.

---

## Tier 2: Interesting and clean, smaller story or more work

| # | Idea | Task | Data | What's interesting | Watch out |
|---|------|------|------|--------------------|-----------|
| 6 | **Online shoppers' purchase intention** (UCI 468) | Binary (15.5% buy) | 12,330 sessions × 18, no NA | E-commerce conversion. Session behaviour vs. month/visitor type | `PageValues` dominates and is close to leakage; compare models with and without it |
| 7 | **Kepler exoplanets: real planet or false positive?** (NASA Exoplanet Archive KOI table) | Binary | ~9,564 objects, transit and star properties | Fun science story, "ML for planet vetting" | Must drop `koi_fpflag_*`, `koi_score`, `kepler_name` (leakage); some NAs; pin the table version |
| 8 | **Gas turbine NOx emissions** (UCI 551) | Regression | 36,733 hourly rows × 11 sensors, 2011–2015 | Environmental. Official protocol: train 2011–13, test 2014–15 → **distribution shift** | Only 11 features; time dependence |
| 9 | **Correcting weather-model forecasts in Seoul** (UCI 514) | Regression (next-day Tmax) | 7,750 rows, 25 stations | "ML beats the physics model": raw model RMSE 2.08 °C is the benchmark | Few NAs; split by date, not randomly |
| 10 | **Spotify genre classification** (TidyTuesday 2020-01-21) | 6-class | 32,833 songs, ~5k/genre, 12 audio features | Fun and visual; confusion matrix tells a story (rock/rap easy, pop/latin confused) | Same track appears in several playlists → deduplicate by `track_id`; accuracy only ~55–60% |
| 11 | **Himalayan expeditions: who reaches the summit?** (TidyTuesday 2020-09-22, Himalayan Database) | Binary | ~76k climber records | Great storytelling: oxygen, age, season, peak height, era | Filter hired staff; climbers within one expedition are correlated → grouped CV; some NAs |
| 12 | **Parkinson's severity from voice** (UCI 189) | Regression (UPDRS) | 5,875 recordings from 42 patients | Medical and remote monitoring. **Random vs. patient-grouped CV** gives very different errors, a strong methodology point | Target is linearly interpolated between visits |
| 13 | **Dry bean variety from image shape** (UCI 602) | 7-class | 13,611 × 16, clean | Agricultural quality control; highly collinear shape features | Fairly easy (~93% accuracy), so less to discuss |
| 14 | **Diabetes risk from lifestyle (CDC BRFSS)** (UCI 891) | Binary / 3-class | 253,680 × 21, clean | Public health; large sample → subsample | Imbalanced (~14%); survey self-reports |
| 15 | **Vienna Airbnb prices** (Inside Airbnb, snapshot **2026-09-17**) | Regression (log price) | ~10k listings, 70+ columns | Local, current, very motivating (district, distance to Stephansplatz, host type) | **Most cleaning work:** price strings, amenities lists, NAs, outliers. Risky with 11 days left |
| 16 | **Online news virality** (UCI 332, Mashable) | Regression / binary | 39,644 × 61, no NA | Honest finding: popularity is barely predictable (R² ~0.1) | Weak signal makes for a less satisfying story |
| 17 | **ATP tennis match winner** (Jeff Sackmann GitHub) | Binary | Thousands of matches per year | Baseline "higher rank wins" ≈ 65%; can ML beat it? | Must randomize player1/player2 (otherwise the label leaks); CC BY-NC licence; some NAs |
| 18 | **What makes a good board game?** (TidyTuesday 2022-01-25, BoardGameGeek) | Regression (rating) | ~21k games | Lasso on category/mechanic dummies | Needs parsing of list columns |

---

## Datasets to avoid
- **Obesity levels (UCI 544):** 77% of rows are SMOTE-generated synthetic data.
- **Kaggle "stroke", "mobile price", "Paris housing", "bank churn (ABC)":** synthetic or unclear origin.
- **WHO Life Expectancy (Kaggle):** many errors and inconsistent units.
- **Credit-card fraud (Kaggle):** features are anonymised PCA components, so no interpretation is possible.
- **Titanic, Iris, Boston, Breast Cancer Wisconsin, Pima, Telco churn, Adult:** overused, trivial, or ethically problematic (Boston).
- **Anything already used in class** (see top).

---

## My recommendation
- **Most impressive / distinctive:** #1 Insurance claim frequency. It is unusual for students, matches the
  regularized GLM unit exactly, the data is verified clean, and there are published benchmarks to compare against.
- **Safest with a strong story:** #4 Student dropout (classification) or #2 Superconductors (regression).
- **Business classification:** #3 Hotel cancellations, built around leakage, a time-based split, and a cost-sensitive threshold.

### Generic 15-minute slide plan (fits any of the above)
1. Context and question (1) → 2. Data and EDA (2–3) → 3. Preprocessing / leakage / split strategy (1)
4. Methods and tuning grids (1–2) → 5. CV results table + test performance (1–2)
6. Interpretation: coefficients / variable importance / partial dependence (2) → 7. Conclusion and limitations (1)
Backup slides: full tuning curves, extra EDA, code snippets.

## Sources
- freMTPL2: https://api.openml.org/d/41214 · benchmarks: https://arxiv.org/pdf/2608.26703
- Superconductivity: https://archive.ics.uci.edu/dataset/464 · paper: https://ar5iv.arxiv.org/html/1803.10260
- Hotel bookings: https://repositorio.iscte-iul.pt/handle/10071/16929 · CSV: https://github.com/rfordatascience/tidytuesday (2020-02-11)
- Student dropout: https://archive.ics.uci.edu/dataset/697
- Taiwan bankruptcy: https://archive.ics.uci.edu/dataset/572
- Online shoppers: https://archive.ics.uci.edu/dataset/468
- Kepler KOI: https://exoplanetarchive.ipac.caltech.edu/docs/API_kepcandidate_columns.html
- Gas turbine: https://archive.ics.uci.edu/dataset/551
- Seoul temperature: https://archive.ics.uci.edu/dataset/514
- Spotify: https://rfordatascience.github.io/tidytuesday/data/2020/2020-01-21/readme.html
- Himalaya: https://rfordatascience.github.io/tidytuesday/data/2020/2020-09-22/readme.html
- Parkinson's: https://archive.ics.uci.edu/dataset/189
- Dry bean: https://archive.ics.uci.edu/dataset/602
- CDC diabetes: https://archive.ics.uci.edu/dataset/891
- Inside Airbnb Vienna: https://insideairbnb.com/get-the-data/
- Online news: https://archive.ics.uci.edu/dataset/332
- Tennis: https://github.com/JeffSackmann/tennis_atp
- Board games: https://rfordatascience.github.io/tidytuesday/data/2022/2022-01-25/readme.html
- Obesity (avoid): https://archive.ics.uci.edu/dataset/544
