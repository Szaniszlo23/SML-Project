# Himalayan expeditions: how much of summit success is decided before the climb?

**Main question:** using only what is known before an expedition leaves, how well can we predict whether a
climber reaches the summit, and what drives the odds?

**Answer:** well enough to separate test climbers into groups with a 6% to 73% summit rate (boosting, AUC 0.76,
well calibrated). The mountain and the year matter most, then the climber and the hired support. Forecasts of
the *level* for future seasons are poor, because summit rates jumped after 2015.

Data: non-hired climbers (clients and leaders), 1950–2019: 55,049 climbers, 9,883 expeditions, 33.8% reached
the summit. Only variables known before the climb are used; the oxygen variables are leaky and appear only in
the leakage check.

## Files

* `analysis.R`: the whole analysis in one script (data, exploration, four tuned models, assessment, insights,
  robustness checks). Run from this folder with `Rscript analysis.R` (about 15 minutes on 8 cores).
* `figures/fig01` … `fig16`: figures written by the script, in the order of the story
* `results/results.txt`: everything the script prints; `results/*.csv`: test metrics and tuning grids
* `slides/presentation.tex` / `.pdf`: the talk (Beamer; build with `cd slides && latexmk -pdf presentation.tex`)
* `data/`: the three TidyTuesday tables (downloaded automatically if missing)
* `Himalaya_data_report.pdf`: earlier report describing every variable (first-pass numbers, superseded by the slides)

## Test set (20% of expeditions, never used for tuning)

| Model | Log loss | Brier | AUC | Accuracy |
|---|---|---|---|---|
| Summit rate only | 0.629 | 0.219 | 0.500 | 0.678 |
| Elastic net | 0.582 | 0.198 | 0.695 | 0.697 |
| Pruned tree (30 leaves) | 0.579 | 0.196 | 0.685 | 0.716 |
| Random forest | 0.543 | 0.181 | 0.748 | 0.736 |
| **Boosting** | **0.533** | **0.178** | **0.757** | **0.738** |
