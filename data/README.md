# Data folder

This folder holds the datasets used by the analysis scripts in `code/`.
The two panels in `data/final/` are the only inputs the current R pipeline
reads. Everything in `data/archive/` is legacy and is no longer used (see
`data/archive/README.md`).

## data/final/ (active inputs)

### panel_household_gov.csv
Household x survey-year panel, **governorate level** (18 governorates, waves
2006/07, 2012, 2017). One row per household-year. Renamed from `panel.csv`.

- **Answers:** how do drought (SPEI-4) and armed conflict jointly affect
  household welfare (log per-capita expenditure, poverty) and the occupational
  composition choice (stay farmer / urban migration / rural non-agri)?
- **Feeds:** the main regressions in `code/analysis/main_analysis.R` (governorate
  fixed-effects specifications), the descriptive tables and Figures 5/6/11 in
  `code/figures/descriptive_figures.R`, the land robustness in
  `code/analysis/land_lag_robustness.R`, the appendix extracts in
  `code/analysis/appendix_extracts.R`, and the household descriptive stats in
  `code/data_prep/conflict_measure.R`.

### panel_household_district.csv
Same household x year panel but **with a `district_id` (qhada) column**, so the
analysis can go one geographic level finer. Renamed from `panel_district.csv`.

- **Answers:** are the governorate-level results robust to using district fixed
  effects / district clustering (finer identification)?
- **Feeds:** the district-level specifications in
  `code/analysis/main_analysis.R`, the choropleth maps in `code/figures/maps.R`,
  and the population shares in `code/data_prep/population.R`.

## data/processed/ (built by code/data_prep, read by the analysis)

- `climate_extended.csv` — SPEI-4 by governorate-year with lags (dryness = -SPEI).
- `climate_gov_year.csv` — SPEI-4 by governorate-year (used by the maps).
- `conflict_gov_year.csv` — raw UCDP GED fatalities by governorate-year (input to
  the conflict measure and Figure 2).
- `conflict_gov_year_pc.csv` — **the paper's conflict measure**: deaths per
  100,000 inhabitants (`conflict_int`, linear) plus the binary high-conflict
  indicator (`high_conf`). Built by `code/data_prep/conflict_measure.R`.
- `population_gov_year.csv`, `population_district.csv` — official COSIT-anchored
  population used to normalise conflict. Built by `code/data_prep/population.R`.
