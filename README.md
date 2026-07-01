# Compound Drought–Conflict Shocks and Iraqi Household : Consumption, Poverty and Land Productivity (2007–2017)

**Author:** Alexandre Gars (ESSEC Business School)

This repository contains the data pipeline, analysis code, and compiled papers
for a study of how **drought and armed conflict jointly** affect the consumption,
poverty, and agricultural production of Iraqi households between 2006/07 and 2017.

---

## Summary

Climate shocks and armed conflict are usually studied in isolation, yet in Iraq
they repeatedly coincided: the severe 2007–2009 drought overlapped with the
post-invasion insurgency and, later, the rise of ISIS. The welfare cost of facing
both at once need not equal the sum of the two — conflict can destroy the
irrigation and input systems households rely on to cope with drought, while
drought can drain the savings households would draw on when displaced by conflict.

The study is built around a single coefficient, **β₃**, on the interaction
**dryness × conflict** in a two-way fixed-effects regression. A **negative** β₃ on
consumption means the compound shock *amplifies* the drought penalty; a
**positive** β₃ means it *attenuates* it.

### Main findings

- **The pooled interaction is not significant** (β̂₃ = +0.0016, n.s. on log per
  capita expenditure). Once conflict is measured as a linear death rate, a few
  extreme governorate-years dominate and the pooled compound effect washes out.
- **The pooled null hides sharply opposing regional patterns.** In the four
  ISIS-exposed governorates the compound shock **amplifies** the drought penalty
  (β̂₃ = −0.0167, *p* < 0.01; binary indicator −0.56, *p* < 0.10), and the rain-fed
  Kurdistan Region shows the same amplifying sign. Elsewhere in Iraq the
  interaction **attenuates** (non-ISIS binary +0.76, *p* < 0.01).
- **On the production side**, compound shocks contract the agricultural asset base:
  the cultivated-area interaction is **−0.433 (*p* < 0.01)**, the clearest
  compound-shock result in the paper, alongside negative conflict effects on land
  ownership, irrigation access, and livestock.
- The pattern fits an **income-portfolio** reading: the compound shock bites where
  farming is still the main source of household income, and fades where transfers
  and non-farm income have already displaced the farm channel.

---

## Data

| Source | Description | Role |
|---|---|---|
| **IHSES 2006/07** | Iraq Household Socio-Economic Survey, wave 1 (17,822 HH) | Consumption, poverty, demographics |
| **IHSES 2012** | Iraq Household Socio-Economic Survey, wave 2 (25,146 HH) | Consumption, poverty, land productivity |
| **SWIFT 2017** | Survey of Well-being via Instant and Frequent Tracking (1,500 long-form HH) | Consumption, wave-3 coverage |
| **SPEIbase v2.9** | Standardised Precipitation-Evapotranspiration Index (SPEI-4) | Drought / *dryness* = −SPEI-4 |
| **UCDP GED v25.1** | Uppsala Conflict Data Program, Georeferenced Event Dataset | Conflict fatalities |
| **COSIT censuses** | Iraq Central Statistical Organization population (1997, 2009, 2024) | Population to normalise conflict |
| **GADM 4.1** | Iraq administrative boundaries (18 governorates, 102 districts) | Maps |

Household welfare is **log per capita expenditure** (2006 prices, deflated by the
official CSO Paasche index) and a **poverty headcount**. Conflict intensity is
**deaths per 100,000 inhabitants**, entered linearly, with a binary high-conflict
indicator (= 1 above the 75th percentile, ≈ 8 deaths per 100,000). Everything is
measured at the **governorate–year** level; the district (qhada) level is a
robustness check.

> **Note on raw data.** The raw survey microdata, climate NetCDF files, and full
> conflict event logs (≈ 2 GB, and several files exceed GitHub's size limits) are
> **not** included. Only the constructed panels (`data/final/`, `data/processed/`)
> and the small climate/shapefile inputs the figure scripts need are versioned.
> The raw sources are available from their original providers (World Bank
> Microdata Library for IHSES/SWIFT, CSIC for SPEIbase, UCDP for GED, GADM for
> boundaries).

---

## Repository structure

```
├── code/
│   ├── data_prep/
│   │   ├── population.R          # COSIT-anchored governorate & district population panels
│   │   └── conflict_measure.R    # deaths-per-100k conflict intensity + binary indicator
│   ├── analysis/
│   │   ├── main_analysis.R       # progressive specs, subsamples, dynamics, land, demeaned (gov + district)
│   │   ├── land_lag_robustness.R # land-productivity and lag robustness checks
│   │   └── appendix_extracts.R   # Paasche deflator & poverty-headcount appendix tables
│   └── figures/
│       ├── descriptive_figures.R # SPEI trend, conflict, spectral indices, composition, welfare
│       ├── conflict_figures.R    # fatalities distribution, dryness/conflict time series
│       ├── maps.R                # governorate & district GADM maps
│       └── maps_continuous.R     # continuous dryness & per-capita conflict maps
├── data/
│   ├── final/                    # constructed household panels (panel.csv, panel_district.csv, ...)
│   ├── processed/                # governorate-year climate, conflict, and population panels
│   └── external/                 # small climate CSVs + GADM shapefiles (large raw data excluded)
├── outputs/
│   ├── figures/                  # figures used in final.pdf (PDF + PNG)
│   └── tables/                   # regression outputs (pc_*, land_lag, appx_*)
└── writing/
    ├── final.pdf                 # full paper (44 pp.)
    └── writing_sample.pdf        # condensed research summary (≤ 6 pp.)
```

The LaTeX sources are kept locally but excluded from the repository; the compiled
PDFs are the versioned deliverables.

---

## Reproducing the analysis

The pipeline is written in **R** (tested with R 4.5.1). Required packages:
`dplyr`, `readr`, `tidyr`, `fixest`, `ggplot2`, `sf`, `viridis`, `cowplot`,
`stringr`.

Run scripts from the repository root (paths inside are relative to the root):

```r
# 1. Build population and conflict measures
Rscript code/data_prep/population.R
Rscript code/data_prep/conflict_measure.R

# 2. Estimate the models (governorate + district)
Rscript code/analysis/main_analysis.R
Rscript code/analysis/land_lag_robustness.R
Rscript code/analysis/appendix_extracts.R

# 3. Regenerate figures
Rscript code/figures/descriptive_figures.R
Rscript code/figures/conflict_figures.R
Rscript code/figures/maps.R
Rscript code/figures/maps_continuous.R
```

Regression tables are written to `outputs/tables/` and figures to
`outputs/figures/`. Steps that consume raw microdata or NetCDF climate grids
require re-obtaining those sources (see the note above); the steps that build on
the constructed panels run against the versioned data as-is.

---

## Methodological reference

The two-way fixed-effects, continuous-treatment design follows the sub-national
climate-and-welfare literature; the closest crop-production precedent is Li et al.
(2022, *Nature Food*), "Civil war hinders crop production and threatens food
security in Syria."
