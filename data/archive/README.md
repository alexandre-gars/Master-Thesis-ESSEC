# Archived data (not used by the current pipeline)

None of the files here are read by any script in `code/`. They are kept only for reference.

- `panel.dta` — Stata export of the old `panel.csv` (now
  `data/final/panel_household_gov.csv`). Was read by Stata `.do` models that are no longer part of the repository.
- `panel_agri.dta` — Stata export of the agricultural/rural subset of the panel. Same legacy Stata workflow.
- `district_collapse.csv` — district-collapsed panel with shift-share (Bartik) and ACLED columns, built for the district instrumental-variables models that are not in the current R pipeline.
