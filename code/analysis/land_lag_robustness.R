# =============================================================================
# This is a robustness check for the "cultivated area" regression in the paper's
# land table (tab:land). The question it answers: when we add one-year lags of
# the two shocks (last year's dryness and last year's conflict) to the model,
# does the current-year interaction "dryness x conflict" change much? If it
# barely moves, the interaction is not just picking up last year's conditions.
#
# The interaction itself stays current-year (2012) only; only the two main
# effects get a lag. The land outcome is observed in the 2012 wave only, so this
# is a single cross-section (no fixed effects) with standard errors clustered by
# governorate.
#
# Inputs:
#   data/final/panel_household_gov.csv
#   data/processed/climate_extended.csv
#   data/processed/conflict_gov_year_pc.csv
# Outputs:
#   outputs/tables/land_lag_robustness.rds  (the two fitted models)
#   outputs/tables/land_lag_robustness.csv  (their coefficients)
# =============================================================================

# dplyr/readr/tidyr for data, fixest for the regression
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(fixest)
})
out_dir <- "outputs/tables"; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# climate (SPEI) and conflict tables, joined onto the panel inside prep_panel
clim <- read_csv("data/processed/climate_extended.csv", show_col_types = FALSE, progress = FALSE)
cpc  <- read_csv("data/processed/conflict_gov_year_pc.csv", show_col_types = FALSE, progress = FALSE)

# prep_panel(): same idea as in main_analysis.R but trimmed down. It reads the
# panel, builds dryness (= -SPEI) and its lags, joins the conflict variables,
# builds log household size and log cultivated area, fills missing controls with
# 0, and adds the current-year interaction dry_x_int.
prep_panel <- function(path) {
  panel <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  lag3_clim <- clim %>% select(governorate, year, spei3_annual_mean) %>%
    mutate(year = year + 3L) %>% rename(spei3_lag3 = spei3_annual_mean)
  panel <- panel %>%
    mutate(dryness = -spei3_annual_mean,
           dryness_lag1 = -spei3_lag1,
           dryness_lag2 = -spei3_lag2) %>%
    left_join(lag3_clim, by = c("governorate", "year")) %>%
    mutate(dryness_lag3 = -spei3_lag3)
  panel <- panel %>%
    left_join(cpc %>% select(governorate, year, conflict_int,
                             conflict_int_lag1, conflict_int_lag2, conflict_int_lag3,
                             high_conf),
              by = c("governorate", "year"))
  panel <- panel %>%
    mutate(ln_hhsize = log(hh_size),
           ln_agri_area = log(1 + replace_na(agri_area_donum, 0)))
  ctrl_cols <- c("age_head","male_head","edu_head","dep_ratio",
                 "n_young_male","has_agri_land","has_irrigation")
  for (col in ctrl_cols) if (col %in% names(panel)) panel[[col]] <- replace_na(panel[[col]], 0)
  panel %>% mutate(dry_x_int = dryness * conflict_int)
}

panel <- prep_panel("data/final/panel_household_gov.csv")

# household controls used in the land regression (as one formula string)
land_ctrl <- "age_head + male_head + edu_head + dep_ratio + ln_hhsize + n_young_male + has_irrigation"

# build the estimation sample: 2012 wave only, households that actually farm
# (positive cultivated area), and with non-missing current and one-year-lag shocks
la <- panel %>%
  filter(wave == 2, agri_area_donum > 0, !is.na(agri_area_donum),
         !is.na(dryness), !is.na(conflict_int),
         !is.na(dryness_lag1), !is.na(conflict_int_lag1))

# centre each shock on its sample mean (subtract the mean). Centring does not
# change the interaction coefficient, but it makes each main effect readable as
# the effect when the other shock is at its average value. m() does the centring.
m <- function(x) x - mean(x)
la <- la %>% mutate(
  dry_c   = m(dryness),        conf_c   = m(conflict_int),
  dryL1_c = m(dryness_lag1),   confL1_c = m(conflict_int_lag1),
  dxc_c   = dry_c * conf_c)     # interaction = current-year dryness x conflict only

# two models: (1) current-year shocks only, (2) also adding the one-year lags
f1 <- as.formula(paste("ln_agri_area ~ dry_c + conf_c + dxc_c +", land_ctrl))
f2 <- as.formula(paste("ln_agri_area ~ dry_c + conf_c + dxc_c + dryL1_c + confL1_c +", land_ctrl))

fit1 <- feols(f1, data = la, cluster = ~governorate)
fit2 <- feols(f2, data = la, cluster = ~governorate)

# --- helpers to print a small side-by-side comparison table ---
# grab(): pull estimate, standard error and p-value for variable v, or NA if the
# variable is not in the model (lags are absent from model 1)
grab <- function(fit, v) { ct <- coeftable(fit); if (v %in% rownames(ct)) ct[v, c(1,2,4)] else c(NA,NA,NA) }
# stars(): significance stars from a p-value
stars <- function(p) ifelse(is.na(p),"", ifelse(p<.01,"***", ifelse(p<.05,"**", ifelse(p<.10,"*",""))))
# row(): one formatted line showing variable v in both models (1) and (2)
row <- function(v,lab){ a<-grab(fit1,v); b<-grab(fit2,v)
  sprintf("%-28s | %8.4f%-3s (%.4f) | %8.4f%-3s (%.4f)",
          lab, a[1], stars(a[3]), a[2], b[1], stars(b[3]), b[2]) }

# print the comparison table to the console
cat("\nln(cultivated area) - IHSES 2012, governorate-clustered SE (", nobs(fit1), "->", nobs(fit2), "obs )\n")
cat("Variable                     |     (1) 2012 only      |   (2) + one-year lags\n")
cat("-------------------------------------------------------------------------\n")
cat(row("dry_c",   "Dryness (2012)"),        "\n")
cat(row("conf_c",  "Conflict (2012)"),       "\n")
cat(row("dxc_c",   "Dryness x Conflict (2012)"), "\n")
cat(row("dryL1_c", "Dryness (t-1, 2011)"),   "\n")
cat(row("confL1_c","Conflict (t-1, 2011)"),  "\n")
cat("-------------------------------------------------------------------------\n")
# no fixed effects here (single cross-section), so the number of clusters is just
# the number of distinct governorates in the estimation sample
cat(sprintf("N = %d ; clusters (gov) = %d\n", nobs(fit1), length(unique(la$governorate))))

# save the two fitted models (rds) and their coefficients stacked together (csv)
saveRDS(list(fit1=fit1, fit2=fit2, n=nobs(fit1)), file.path(out_dir, "land_lag_robustness.rds"))
write_csv(
  bind_rows(
    tibble(model="2012_only", variable=rownames(coeftable(fit1)),
           coef=coeftable(fit1)[,1], se=coeftable(fit1)[,2], p=coeftable(fit1)[,4]),
    tibble(model="with_lag1", variable=rownames(coeftable(fit2)),
           coef=coeftable(fit2)[,1], se=coeftable(fit2)[,2], p=coeftable(fit2)[,4])),
  file.path(out_dir, "land_lag_robustness.csv"))
cat("Saved outputs/tables/land_lag_robustness.csv\n")
