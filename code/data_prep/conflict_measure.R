## 27_conflict_pc.R  (REVISED 2026-06-09)
## Build the conflict-intensity measure used in final.tex and report the
## fatalities distribution + the household-sample descriptive statistics.
##
## NEW measure (user request 2026-06-11):
##   conflict_int = 1e5 * fatalities_gt / pop_gt       (deaths per 100,000 inhab.)
##   entered LINEARLY (no log). Fatalities are UCDP GED governorate-year counts,
##   normalised by official COSIT governorate population, so the rate is constant
##   within a governorate-year. Rescaled from deaths/1,000 to deaths/100,000 so the
##   intensity values read as whole numbers rather than small decimals.
##
## Binary high-conflict indicator (for the S4 interaction "binary" case):
##   high_conf = 1(conflict_int >= p75 of survey gov-years)  -- top quartile.
##   p75 keeps enough treated cells for the interaction to be estimable in every
##   subsample (KRG, non-ISIS), unlike the stricter p90, while still flagging
##   genuinely elevated violence (the UCDP minor-armed-conflict floor of 25
##   battle deaths per year sits well below this cut).
##
## Output: data/processed/conflict_gov_year_pc.csv
## Author: A. Gars

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})

out_dir <- "data/processed"

conf <- read_csv("data/processed/conflict_gov_year.csv",
                 show_col_types = FALSE, progress = FALSE) %>%
  select(governorate, year, conflict_fatalities)
pop  <- read_csv("data/processed/population_gov_year.csv",
                 show_col_types = FALSE, progress = FALSE) %>%
  select(governorate, year, pop)

cpc <- conf %>%
  left_join(pop, by = c("governorate", "year")) %>%
  mutate(
    deaths_per_100k = 1e5 * conflict_fatalities / pop,    # deaths per 100,000 inhab.
    conflict_int    = deaths_per_100k                     # linear intensity (no log)
  )

## ---- lags of the new measure (year shift within governorate) -------------
mk_lag <- function(df, k) {
  df %>% select(governorate, year, conflict_int) %>%
    mutate(year = year + k) %>%
    rename(!!paste0("conflict_int_lag", k) := conflict_int)
}
cpc <- cpc %>%
  left_join(mk_lag(cpc, 1), by = c("governorate", "year")) %>%
  left_join(mk_lag(cpc, 2), by = c("governorate", "year")) %>%
  left_join(mk_lag(cpc, 3), by = c("governorate", "year"))

## ============================================================
## Fatalities / intensity distribution
## ============================================================
cat("================ DISTRIBUTION (all gov-years 2004-2017) ================\n")
cat("N gov-years:", nrow(cpc), "\n")
cat("\n-- raw fatality counts --\n"); print(summary(cpc$conflict_fatalities))
cat("share == 0 :", round(mean(cpc$conflict_fatalities == 0, na.rm = TRUE), 3), "\n")
cat("\n-- conflict_int = deaths per 100,000 inhabitants --\n")
print(summary(cpc$conflict_int))
cat("quantiles:\n")
print(round(quantile(cpc$conflict_int, c(0,.25,.5,.75,.9,.95,.99,1), na.rm = TRUE), 4))

surv <- cpc %>% filter(year %in% c(2007, 2012, 2017))
cat("\n-- survey years (2007/2012/2017): conflict_int --\n")
print(summary(surv$conflict_int))
cat("quantiles (survey gov-years):\n")
print(round(quantile(surv$conflict_int, c(0,.25,.5,.75,.9,.95,1), na.rm = TRUE), 4))
cat("share survey gov-years with 0 fatalities:",
    round(mean(surv$conflict_fatalities == 0), 3), "\n")
cat("mean conflict_int (survey gov-years):", round(mean(surv$conflict_int), 4), "\n")

## ============================================================
## Binary high-conflict indicator: top quartile (p75)
## ============================================================
thr_all  <- quantile(cpc$conflict_int,  0.75, na.rm = TRUE)   # all gov-years
thr_surv <- quantile(surv$conflict_int, 0.75, na.rm = TRUE)   # survey gov-years
THR <- thr_surv   # threshold used in the regressions (top quartile of survey gov-years)
cpc <- cpc %>% mutate(high_conf = as.integer(conflict_int >= THR))
cat("\n================ BINARY HIGH-CONFLICT INDICATOR (p75) ================\n")
cat("p75 conflict_int (all gov-years) :", round(thr_all, 4), "deaths/100k\n")
cat("p75 conflict_int (survey gov-yrs):", round(thr_surv, 4), "deaths/100k  <-- THRESHOLD\n")
cat("share high_conf==1 (all gov-years)   :", round(mean(cpc$high_conf, na.rm=TRUE), 3), "\n")
cat("share high_conf==1 (survey gov-years):",
    round(mean(cpc$high_conf[cpc$year %in% c(2007,2012,2017)], na.rm=TRUE), 3), "\n")

write_csv(cpc, file.path(out_dir, "conflict_gov_year_pc.csv"))
cat("\nWrote conflict_gov_year_pc.csv (", nrow(cpc), "rows )\n")

## ============================================================
## HOUSEHOLD-SAMPLE DESCRIPTIVE STATISTICS (for desc tables in the paper)
## ============================================================
panel <- read_csv("data/final/panel.csv", show_col_types = FALSE, progress = FALSE) %>%
  left_join(cpc %>% select(governorate, year, conflict_int, high_conf),
            by = c("governorate", "year"))

## agricultural / rural household sample used in the descriptive tables
agri <- panel %>% filter(occ_agri == 1 | rural == 1)
cat("\n================ DESC: agri/rural household sample ================\n")
cat("N agri/rural rows:", nrow(agri), "\n")
ci <- agri$conflict_int
cat(sprintf("conflict_int (deaths/100k): mean=%.4f sd=%.4f median=%.4f min=%.4f max=%.4f\n",
            mean(ci,na.rm=TRUE), sd(ci,na.rm=TRUE), median(ci,na.rm=TRUE),
            min(ci,na.rm=TRUE), max(ci,na.rm=TRUE)))
cat("share high_conf==1 (agri sample):", round(mean(agri$high_conf, na.rm=TRUE),3), "\n")

cat("\n-- conflict_int by occupational composition (strategic_choice_label) --\n")
comp <- panel %>% filter(!is.na(strategic_choice_label)) %>%
  group_by(strategic_choice_label) %>%
  summarise(n=n(),
            mean_conflict_int = mean(conflict_int, na.rm=TRUE),
            mean_high_conf    = mean(high_conf, na.rm=TRUE), .groups="drop")
print(as.data.frame(comp), digits=4)

cat("\nDone.\n")
