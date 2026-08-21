# =============================================================================
# conflict_measure.R
# -----------------------------------------------------------------------------
# Goal of this script: turn raw conflict death counts into the conflict measure
# the paper actually uses, and print some descriptive numbers about it.
#
# The measure (decided 2026-06-11):
#   conflict_int = 100000 * fatalities / population   (deaths per 100,000 people)
# It is entered "linearly" in the regressions, meaning we use the number itself,
# not its logarithm. Fatalities come from UCDP GED (a conflict-events database),
# counted per governorate per year, and we divide by the official population from
# population.R so that big governorates are not automatically counted as more
# violent. We multiply by 100,000 instead of 1,000 just so the numbers read as
# whole figures rather than tiny decimals.
#
# We also build a simple yes/no "high conflict" flag:
#   high_conf = 1 if conflict_int is in the top 25% (p75) of the survey years.
# The top quartile is used (rather than the stricter top 10%) so that even small
# subsamples like Kurdistan still contain some "high conflict" observations,
# which is needed for the interaction terms in the regressions to be estimable.
#
# Inputs:
#   data/processed/conflict_gov_year.csv     (raw fatalities per governorate-year)
#   data/processed/population_gov_year.csv   (population, from population.R)
#   data/final/panel_household_gov.csv       (households, for descriptive stats)
# Output:
#   data/processed/conflict_gov_year_pc.csv  (conflict_int, its lags, high_conf)
# =============================================================================

# dplyr = data manipulation, readr = read/write csv, tidyr = reshaping helpers
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})

# folder where the output is written
out_dir <- "data/processed"

# read the raw fatalities (keep only the columns we need) and the population
conf <- read_csv("data/processed/conflict_gov_year.csv",
                 show_col_types = FALSE, progress = FALSE) %>%
  select(governorate, year, conflict_fatalities)
pop  <- read_csv("data/processed/population_gov_year.csv",
                 show_col_types = FALSE, progress = FALSE) %>%
  select(governorate, year, pop)

# join population onto fatalities (matching governorate and year), then compute
# deaths per 100,000 inhabitants. conflict_int is just a copy of that number, and
# it is the variable used everywhere else in the project.
cpc <- conf %>%
  left_join(pop, by = c("governorate", "year")) %>%
  mutate(
    deaths_per_100k = 1e5 * conflict_fatalities / pop,    # deaths per 100,000 inhab.
    conflict_int    = deaths_per_100k                     # linear intensity (no log)
  )

# -----------------------------------------------------------------------------
# lagged versions of the measure (conflict one, two and three years earlier).
# mk_lag() takes the conflict table, adds k to the year, and renames the column
# to conflict_int_lag{k}. When we later join it back on (governorate, year), a
# row in year t receives the conflict value from year t-k. These lags let the
# regressions test whether past conflict, not just current conflict, matters.
# -----------------------------------------------------------------------------
mk_lag <- function(df, k) {
  df %>% select(governorate, year, conflict_int) %>%
    mutate(year = year + k) %>%
    rename(!!paste0("conflict_int_lag", k) := conflict_int)
}
cpc <- cpc %>%
  left_join(mk_lag(cpc, 1), by = c("governorate", "year")) %>%
  left_join(mk_lag(cpc, 2), by = c("governorate", "year")) %>%
  left_join(mk_lag(cpc, 3), by = c("governorate", "year"))

# -----------------------------------------------------------------------------
# describe the fatalities and the intensity measure.
# summary() prints min/quartiles/mean/max; quantile() prints chosen percentiles.
# We do this for all governorate-years and again just for the survey years,
# because the regressions only ever use the survey years (2007, 2012, 2017).
# -----------------------------------------------------------------------------
cat("distribution (all gov-years 2004-2017)\n")
cat("N gov-years:", nrow(cpc), "\n")
cat("\n-- raw fatality counts --\n"); print(summary(cpc$conflict_fatalities))
cat("share == 0 :", round(mean(cpc$conflict_fatalities == 0, na.rm = TRUE), 3), "\n")
cat("\n-- conflict_int = deaths per 100,000 inhabitants --\n")
print(summary(cpc$conflict_int))
cat("quantiles:\n")
print(round(quantile(cpc$conflict_int, c(0,.25,.5,.75,.9,.95,.99,1), na.rm = TRUE), 4))

# keep only the three survey years and describe conflict within them
surv <- cpc %>% filter(year %in% c(2007, 2012, 2017))
cat("\n-- survey years (2007/2012/2017): conflict_int --\n")
print(summary(surv$conflict_int))
cat("quantiles (survey gov-years):\n")
print(round(quantile(surv$conflict_int, c(0,.25,.5,.75,.9,.95,1), na.rm = TRUE), 4))
cat("share survey gov-years with 0 fatalities:",
    round(mean(surv$conflict_fatalities == 0), 3), "\n")
cat("mean conflict_int (survey gov-years):", round(mean(surv$conflict_int), 4), "\n")

# -----------------------------------------------------------------------------
# build the binary high-conflict flag.
# quantile(..., 0.75) is the value below which 75% of observations fall (the top
# quartile cut-off). We compute it two ways but use the survey-years cut-off as
# the threshold, then flag every governorate-year at or above it as high_conf=1.
# -----------------------------------------------------------------------------
thr_all  <- quantile(cpc$conflict_int,  0.75, na.rm = TRUE)   # cut-off over all gov-years
thr_surv <- quantile(surv$conflict_int, 0.75, na.rm = TRUE)   # cut-off over survey gov-years
THR <- thr_surv   # threshold used in the regressions (top quartile of survey gov-years)
cpc <- cpc %>% mutate(high_conf = as.integer(conflict_int >= THR))
cat("\nbinary high-conflict indicator (p75)\n")
cat("p75 conflict_int (all gov-years) :", round(thr_all, 4), "deaths/100k\n")
cat("p75 conflict_int (survey gov-yrs):", round(thr_surv, 4), "deaths/100k  <-- threshold\n")
cat("share high_conf==1 (all gov-years)   :", round(mean(cpc$high_conf, na.rm=TRUE), 3), "\n")
cat("share high_conf==1 (survey gov-years):",
    round(mean(cpc$high_conf[cpc$year %in% c(2007,2012,2017)], na.rm=TRUE), 3), "\n")

# save the finished conflict table (intensity, its three lags, and the flag)
write_csv(cpc, file.path(out_dir, "conflict_gov_year_pc.csv"))
cat("\nWrote conflict_gov_year_pc.csv (", nrow(cpc), "rows )\n")

# -----------------------------------------------------------------------------
# household-level descriptive statistics for the paper's descriptive tables.
# We read the household panel and attach each household's conflict_int/high_conf
# by matching its governorate and year.
# -----------------------------------------------------------------------------
panel <- read_csv("data/final/panel_household_gov.csv", show_col_types = FALSE, progress = FALSE) %>%
  left_join(cpc %>% select(governorate, year, conflict_int, high_conf),
            by = c("governorate", "year"))

# the paper's descriptive tables use the agricultural / rural household sample:
# any household that either works in agriculture (occ_agri==1) or lives in a
# rural area (rural==1).
agri <- panel %>% filter(occ_agri == 1 | rural == 1)
cat("\ndesc: agri/rural household sample\n")
cat("N agri/rural rows:", nrow(agri), "\n")
ci <- agri$conflict_int
cat(sprintf("conflict_int (deaths/100k): mean=%.4f sd=%.4f median=%.4f min=%.4f max=%.4f\n",
            mean(ci,na.rm=TRUE), sd(ci,na.rm=TRUE), median(ci,na.rm=TRUE),
            min(ci,na.rm=TRUE), max(ci,na.rm=TRUE)))
cat("share high_conf==1 (agri sample):", round(mean(agri$high_conf, na.rm=TRUE),3), "\n")

# average conflict exposure by occupational-composition group (stay farmer /
# urban migration / rural non-agri), to see who lives with more violence
cat("\n-- conflict_int by occupational composition (strategic_choice_label) --\n")
comp <- panel %>% filter(!is.na(strategic_choice_label)) %>%
  group_by(strategic_choice_label) %>%
  summarise(n=n(),
            mean_conflict_int = mean(conflict_int, na.rm=TRUE),
            mean_high_conf    = mean(high_conf, na.rm=TRUE), .groups="drop")
print(as.data.frame(comp), digits=4)

cat("\nDone.\n")
