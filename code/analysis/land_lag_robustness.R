## 31_land_lag_robustness.R
## Robustness for the cultivated-area (ln Area) regression of Table tab:land:
## does adding ONE-YEAR LAGS of the two main shocks (dryness_{t-1}, conflict_{t-1})
## change the contemporaneous compound interaction (Dryness x Conflict, 2012)?
## The interaction itself stays 2012-only (per request): only the MAIN effects get lags.
## IHSES 2012 cross-section, no FE (single year), governorate-clustered SE.
## Author: A. Gars

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(fixest)
})
out_dir <- "outputs/tables"; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

clim <- read_csv("data/processed/climate_extended.csv", show_col_types = FALSE, progress = FALSE)
cpc  <- read_csv("data/processed/conflict_gov_year_pc.csv", show_col_types = FALSE, progress = FALSE)

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

panel <- prep_panel("data/final/panel.csv")

land_ctrl <- "age_head + male_head + edu_head + dep_ratio + ln_hhsize + n_young_male + has_irrigation"

## land sample: 2012 only, positive cultivated area, valid contemporaneous AND lag-1 shocks
la <- panel %>%
  filter(wave == 2, agri_area_donum > 0, !is.na(agri_area_donum),
         !is.na(dryness), !is.na(conflict_int),
         !is.na(dryness_lag1), !is.na(conflict_int_lag1))

## centre all shocks on the land-sample mean (matches the paper's centred land table;
## the interaction coef is unchanged by centring, main effects read at the other's mean)
m <- function(x) x - mean(x)
la <- la %>% mutate(
  dry_c   = m(dryness),        conf_c   = m(conflict_int),
  dryL1_c = m(dryness_lag1),   confL1_c = m(conflict_int_lag1),
  dxc_c   = dry_c * conf_c)     # interaction = 2012 x 2012 only

f1 <- as.formula(paste("ln_agri_area ~ dry_c + conf_c + dxc_c +", land_ctrl))
f2 <- as.formula(paste("ln_agri_area ~ dry_c + conf_c + dxc_c + dryL1_c + confL1_c +", land_ctrl))

fit1 <- feols(f1, data = la, cluster = ~governorate)
fit2 <- feols(f2, data = la, cluster = ~governorate)

grab <- function(fit, v) { ct <- coeftable(fit); if (v %in% rownames(ct)) ct[v, c(1,2,4)] else c(NA,NA,NA) }
stars <- function(p) ifelse(is.na(p),"", ifelse(p<.01,"***", ifelse(p<.05,"**", ifelse(p<.10,"*",""))))
row <- function(v,lab){ a<-grab(fit1,v); b<-grab(fit2,v)
  sprintf("%-28s | %8.4f%-3s (%.4f) | %8.4f%-3s (%.4f)",
          lab, a[1], stars(a[3]), a[2], b[1], stars(b[3]), b[2]) }

cat("\n=========================================================================\n")
cat("ln(cultivated area) — IHSES 2012, governorate-clustered SE (", nobs(fit1), "->", nobs(fit2), "obs )\n")
cat("Variable                     |     (1) 2012 only      |   (2) + one-year lags\n")
cat("-------------------------------------------------------------------------\n")
cat(row("dry_c",   "Dryness (2012)"),        "\n")
cat(row("conf_c",  "Conflict (2012)"),       "\n")
cat(row("dxc_c",   "Dryness x Conflict (2012)"), "\n")
cat(row("dryL1_c", "Dryness (t-1, 2011)"),   "\n")
cat(row("confL1_c","Conflict (t-1, 2011)"),  "\n")
cat("-------------------------------------------------------------------------\n")
cat(sprintf("N = %d ; clusters (gov) = %d\n", nobs(fit1), fit1$fixef_sizes %||% 18))
cat("=========================================================================\n")

saveRDS(list(fit1=fit1, fit2=fit2, n=nobs(fit1)), file.path(out_dir, "land_lag_robustness.rds"))
write_csv(
  bind_rows(
    tibble(model="2012_only", variable=rownames(coeftable(fit1)),
           coef=coeftable(fit1)[,1], se=coeftable(fit1)[,2], p=coeftable(fit1)[,4]),
    tibble(model="with_lag1", variable=rownames(coeftable(fit2)),
           coef=coeftable(fit2)[,1], se=coeftable(fit2)[,2], p=coeftable(fit2)[,4])),
  file.path(out_dir, "land_lag_robustness.csv"))
cat("Saved outputs/tables/land_lag_robustness.csv\n")
