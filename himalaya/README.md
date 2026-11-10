# Himalayan expeditions: will a climber reach the summit?

Non-hired climbers (clients and leaders), 1950–2019: 55,049 climbers, 9,883 expeditions, 33.8% reached the summit.
Only variables known before the climb are used. The oxygen variables are leaky and appear only in the leakage check.

Run the scripts in order from this folder:

1. `01_describe.R`: loads the data and describes every variable (`results/describe.txt`)
2. `02_prepare.R`: population, cleaning and features (`data/model_data.rds`)
3. `03_models.R`: elastic net, pruned tree, random forest, boosting, all tuned (`results/`, ~6 min)
4. `04_report_figures.R`: descriptive plots (`figures/rep_*`)
5. `05_insights.R`: tuning, comparison, importance, partial dependence, leakage, time split (`figures/mod_*`, `figures/ins_*`, `results/insights.txt`)

Slides: `slides/presentation.tex` (Beamer, uses `figures/`). Build with `cd slides && latexmk -pdf presentation.tex`.

Test set = 20% of expeditions (whole expeditions, never used for tuning):

| Model | Log loss | AUC | Accuracy |
|---|---|---|---|
| Always "no summit" | 0.629 | 0.500 | 0.678 |
| Elastic net | 0.583 | 0.695 | 0.698 |
| Pruned tree (30 leaves) | 0.579 | 0.685 | 0.716 |
| Random forest | 0.541 | 0.751 | 0.740 |
| **Boosting** | **0.534** | **0.756** | **0.739** |
