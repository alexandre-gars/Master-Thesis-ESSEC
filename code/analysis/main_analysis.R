# =============================================================================
# main_analysis.R
# -----------------------------------------------------------------------------
# This is the main regression script of the paper. It estimates how drought
# (dryness) and armed conflict, together, affect household welfare.
#
# Two outcomes are studied in every specification:
#   ln_pcep : log of per-capita expenditure (a continuous welfare measure)
#   poor    : a 0/1 indicator of whether the household is poor
#
# The key idea is the interaction term "dryness * conflict": it tests whether
# drought hurts more when conflict is also high. We report the interaction in
# two flavours:
#   (a) intensity : dryness * conflict_int   (conflict = deaths per 100,000)
#   (b) binary    : dryness * high_conf       (conflict = top-quartile flag)
#
# The whole analysis is run twice, at two geographic levels, by the run_level()
# function: once with governorate fixed effects and clustering, and once with
# district fixed effects and clustering (a finer, more demanding robustness).
#
# Inputs:
#   data/final/panel_household_gov.csv        (household panel, gov level)
#   data/final/panel_household_district.csv   (household panel, district level)
#   data/processed/climate_extended.csv       (SPEI-4 and its lags)
#   data/processed/conflict_gov_year_pc.csv   (conflict_int, its lags, high_conf)
# Outputs: several csv tables in outputs/tables/ (pc_gov_*, pc_dis_*, pc_lincom).
# =============================================================================

# dplyr/readr/tidyr for data work, fixest for fast fixed-effects regressions
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(fixest)
})
set.seed(42)                 # fix the random seed so any random step is reproducible
out_dir <- "outputs/tables"  # folder where result tables are written
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# climate (SPEI) and conflict tables are read once here and reused for both levels
clim <- read_csv("data/processed/climate_extended.csv", show_col_types = FALSE, progress = FALSE)
cpc  <- read_csv("data/processed/conflict_gov_year_pc.csv", show_col_types = FALSE, progress = FALSE)

# -----------------------------------------------------------------------------
# prep_panel(): read a household panel and add every variable the regressions
# need. It is a function because we call it once per geographic level.
# -----------------------------------------------------------------------------
prep_panel <- function(path) {
  panel <- read_csv(path, show_col_types = FALSE, progress = FALSE)

  # "dryness" is just the negative of SPEI, so that a higher number means drier
  # (SPEI is negative when dry). We build the contemporaneous value plus lags.
  # The 3-year lag is not stored in the panel, so we build it here by taking the
  # climate table, shifting its year forward by 3, and joining it back on.
  lag3_clim <- clim %>% select(governorate, year, spei3_annual_mean) %>%
    mutate(year = year + 3L) %>% rename(spei3_lag3 = spei3_annual_mean)
  panel <- panel %>%
    mutate(dryness = -spei3_annual_mean,
           dryness_lag1 = -spei3_lag1,
           dryness_lag2 = -spei3_lag2) %>%
    left_join(lag3_clim, by = c("governorate", "year")) %>%
    mutate(dryness_lag3 = -spei3_lag3)

  # attach the conflict variables (current, three lags, and the high-conflict
  # flag) by matching governorate and year
  panel <- panel %>%
    left_join(cpc %>% select(governorate, year, conflict_int,
                             conflict_int_lag1, conflict_int_lag2, conflict_int_lag3,
                             high_conf),
              by = c("governorate", "year"))

  # log of household size, and log of cultivated area (1 + area so that a zero
  # area does not become log(0) = -infinity)
  panel <- panel %>%
    mutate(ln_hhsize = log(hh_size),
           ln_agri_area = log(1 + replace_na(agri_area_donum, 0)))

  # some control variables have missing values; replace those with 0 so the
  # regression sample is not dropped for a missing control
  ctrl_cols <- c("age_head","male_head","edu_head","dep_ratio",
                 "n_young_male","has_agri_land","has_irrigation")
  for (col in ctrl_cols) if (col %in% names(panel)) panel[[col]] <- replace_na(panel[[col]], 0)

  # build the two interaction terms (intensity and binary versions)
  panel <- panel %>% mutate(dry_x_int = dryness * conflict_int,
                            dry_x_bin = dryness * high_conf)

  # region dummies used later to split the sample into subsamples
  # krg = Kurdistan, isis = the governorates most exposed to ISIS, south = south
  panel <- panel %>%
    mutate(krg = as.integer(governorate %in% c(1L,4L,5L)),
           isis = as.integer(governorate %in% c(3L,6L,7L,8L)),
           south = as.integer(governorate %in% c(13L,14L,18L)))
  panel   # the function returns the prepared panel
}

# the household control variables entered in every regression, as one string
# "age_head + male_head + ..." that we can drop into a formula
controls <- c("age_head","male_head","edu_head","dep_ratio",
              "ln_hhsize","n_young_male","has_agri_land",
              "ln_agri_area","has_irrigation")
ctrl_str <- paste(controls, collapse = " + ")

# -----------------------------------------------------------------------------
# tidy_fe(): take a fitted regression and return a tidy one-row-per-coefficient
# table (estimate, standard error, t-stat, p-value, number of obs, R-squared).
# coeftable() pulls the coefficient matrix out of a fixest model.
# -----------------------------------------------------------------------------
tidy_fe <- function(fit, label, outcome) {
  est <- coeftable(fit)
  tibble(label = label, outcome = outcome, variable = rownames(est),
         coef = est[,1], se = est[,2], t = est[,3], p = est[,4],
         n = nobs(fit), r2 = tryCatch(fitstat(fit,"r2")$r2, error=function(e) NA))
}

# -----------------------------------------------------------------------------
# run_pair(): estimate the same right-hand side for both outcomes (expenditure
# and poverty). "rhs" is the string of variables of interest, ctrl_str adds the
# controls, and "| fe_var + year" tells fixest to absorb those fixed effects.
# cluster=... makes the standard errors robust to correlation within fe_var.
# It returns both fitted models plus their tidy coefficient tables.
# -----------------------------------------------------------------------------
run_pair <- function(data, label, rhs, fe_var, cl_var) {
  f_pce <- as.formula(paste("ln_pcep ~", rhs, "+", ctrl_str, "|", fe_var, "+ year"))
  f_pov <- as.formula(paste("poor    ~", rhs, "+", ctrl_str, "|", fe_var, "+ year"))
  fit_p <- feols(f_pce, data = data, cluster = as.formula(paste0("~", cl_var)))
  fit_v <- feols(f_pov, data = data, cluster = as.formula(paste0("~", cl_var)))
  list(pce = fit_p, pov = fit_v,
       tidy = bind_rows(tidy_fe(fit_p, label, "ln_pcep"),
                        tidy_fe(fit_v, label, "poor")))
}

# -----------------------------------------------------------------------------
# lincom_dry(): the effect of dryness is not a single number when there is an
# interaction, because it depends on the level of conflict m. The total slope is
# b[dryness] + m * b[interaction]. This function computes that combined slope and
# its correct standard error (using the variance-covariance matrix vcov), then a
# z-stat and p-value. This is a "linear combination" (lincom) of coefficients.
# -----------------------------------------------------------------------------
lincom_dry <- function(fit, m, outcome, spec, dvar="dryness", ixvar="dry_x_int") {
  b <- coef(fit); V <- vcov(fit)
  est <- b[dvar] + m * b[ixvar]                              # combined slope at conflict = m
  va  <- V[dvar,dvar] + m^2 * V[ixvar,ixvar] + 2*m*V[dvar,ixvar]  # its variance
  se  <- sqrt(va); z <- est/se
  tibble(spec=spec, outcome=outcome, m=m, lincom=unname(est),
         se=unname(se), z=unname(z), p=2*pnorm(-abs(unname(z))))
}

# -----------------------------------------------------------------------------
# run_level(): runs the entire analysis for one geographic level ("gov" or
# "dis"). It produces the main table, the marginal effects, the subsamples, the
# dynamic (lagged) table, the demeaned table and the land table, and returns
# them all in a list.
# -----------------------------------------------------------------------------
run_level <- function(level) {
  # pick the panel, the fixed-effect variable and the cluster variable for this
  # level, and a short tag used in output file names
  if (level == "gov") {
    panel <- prep_panel("data/final/panel_household_gov.csv")
    fe_var <- "governorate"; cl_var <- "governorate"; tag <- "gov"
  } else {
    panel <- prep_panel("data/final/panel_household_district.csv")
    fe_var <- "district_id"; cl_var <- "district_id"; tag <- "dis"
  }

  # keep only rows with all the pieces the regressions need (no missing outcome,
  # dryness, conflict or flag); at district level also require a district id
  samp <- panel %>% filter(!is.na(ln_pcep), !is.na(poor), !is.na(dryness),
                           !is.na(conflict_int), !is.na(high_conf))
  if (level == "dis") samp <- samp %>% filter(!is.na(district_id))

  # reference conflict levels we will evaluate the dryness effect at: the mean
  # (C_bar), the 90th percentile (a "high conflict" level), and the share of
  # observations flagged high_conf
  cbar <- mean(samp$conflict_int); c90 <- quantile(samp$conflict_int, 0.90)
  hi_share <- mean(samp$high_conf)
  cat("\n###### level:", level, " N =", nrow(samp),
      if (level=="dis") paste("| districts:", length(unique(samp$district_id))) else "", "######\n")
  cat(sprintf("mean conflict_int (C_bar)=%.4f | p90 conflict_int=%.4f | share high_conf=%.3f\n",
              cbar, c90, hi_share))

  # ----- the progressive specifications, from simple to full -----
  # S1  : dryness only
  # S2  : conflict only
  # S3  : dryness + conflict (both, no interaction)
  # S4  : full model with the intensity interaction
  # S4b : full model with the binary interaction
  s1  <- run_pair(samp, "S1",  "dryness", fe_var, cl_var)
  s2  <- run_pair(samp, "S2",  "conflict_int", fe_var, cl_var)
  s3  <- run_pair(samp, "S3",  "dryness + conflict_int", fe_var, cl_var)
  s4  <- run_pair(samp, "S4",  "dryness + conflict_int + dry_x_int", fe_var, cl_var)
  s4b <- run_pair(samp, "S4b", "dryness + high_conf + dry_x_bin",    fe_var, cl_var)
  main_tbl <- bind_rows(s1$tidy, s2$tidy, s3$tidy, s4$tidy, s4b$tidy)
  write_csv(main_tbl, file.path(out_dir, paste0("pc_", tag, "_main.csv")))

  # marginal effect of dryness at different conflict levels:
  # for the intensity model at conflict = 0, mean and p90; for the binary model
  # at high_conf = 0 (low) and 1 (high). We do this for both outcomes.
  lc <- bind_rows(
    lincom_dry(s4$pce, 0,    "ln_pcep","int_C0"),
    lincom_dry(s4$pce, cbar, "ln_pcep","int_Cbar"),
    lincom_dry(s4$pce, c90,  "ln_pcep","int_C90"),
    lincom_dry(s4$pov, 0,    "poor",   "int_C0"),
    lincom_dry(s4$pov, cbar, "poor",   "int_Cbar"),
    lincom_dry(s4$pov, c90,  "poor",   "int_C90"),
    lincom_dry(s4b$pce,0, "ln_pcep","bin_low",  ixvar="dry_x_bin"),
    lincom_dry(s4b$pce,1, "ln_pcep","bin_high", ixvar="dry_x_bin"),
    lincom_dry(s4b$pov,0, "poor",   "bin_low",  ixvar="dry_x_bin"),
    lincom_dry(s4b$pov,1, "poor",   "bin_high", ixvar="dry_x_bin"))
  lc$level <- level

  # ----- subsamples: re-estimate the full model on regional subsets -----
  # this checks the interaction is not driven by one region only
  subs <- list(Full=samp, KRG=filter(samp,krg==1), NonKRG=filter(samp,krg==0),
               ISIS=filter(samp,isis==1), NonISIS=filter(samp,isis==0))
  # run_sub() runs one right-hand side over every subsample; it skips subsamples
  # with fewer than 50 rows and catches any estimation error instead of stopping
  run_sub <- function(rhs, suffix){
    bind_rows(lapply(names(subs), function(nm){
      d <- subs[[nm]]; if (nrow(d) < 50) return(NULL)
      tryCatch(run_pair(d, paste0(nm,suffix), rhs, fe_var, cl_var)$tidy,
               error=function(e){cat("sub",nm,suffix,"fail\n");NULL})
    }))
  }
  sub_int <- run_sub("dryness + conflict_int + dry_x_int", "")      # intensity version
  sub_bin <- run_sub("dryness + high_conf + dry_x_bin",    ".bin")  # binary version
  sub_tbl <- bind_rows(sub_int, sub_bin)
  write_csv(sub_tbl, file.path(out_dir, paste0("pc_", tag, "_sub.csv")))

  # ----- dynamic specification: add lagged main effects, no interaction -----
  # d0 has only current shocks; d1/d2/d3 progressively add 1, 2 and 3 year lags,
  # to see how long the effects of past drought and conflict persist
  d0 <- "dryness + conflict_int"
  d1 <- paste(d0, "+ dryness_lag1 + conflict_int_lag1")
  d2 <- paste(d1, "+ dryness_lag2 + conflict_int_lag2")
  d3 <- paste(d2, "+ dryness_lag3 + conflict_int_lag3")
  dyn_tbl <- bind_rows(
    run_pair(samp, "L0", d0, fe_var, cl_var)$tidy,
    run_pair(samp, "L01", d1, fe_var, cl_var)$tidy,
    run_pair(samp, "L02", d2, fe_var, cl_var)$tidy,
    run_pair(samp, "L03", d3, fe_var, cl_var)$tidy)
  write_csv(dyn_tbl, file.path(out_dir, paste0("pc_", tag, "_dyn.csv")))

  # ----- demeaned (within-group) version of the main model -----
  # here we subtract each group's mean from dryness and conflict by hand, then
  # interact the demeaned variables. This is an alternative way to remove group
  # differences, keeping only year fixed effects in the regression.
  grp <- fe_var
  dm <- samp %>% group_by(.data[[grp]]) %>%
    mutate(dry_dm  = dryness - mean(dryness),
           conf_dm = conflict_int - mean(conflict_int),
           bin_dm  = high_conf - mean(high_conf)) %>%
    ungroup() %>%
    mutate(dry_x_conf_dm = dry_dm * conf_dm,
           dry_x_bin_dm  = dry_dm * bin_dm)
  # dem_run() estimates both outcomes with year fixed effects only, on the
  # demeaned data
  dem_run <- function(rhs, lab){
    fp <- feols(as.formula(paste("ln_pcep ~", rhs, "+", ctrl_str, "| year")),
                data=dm, cluster=as.formula(paste0("~",cl_var)))
    fv <- feols(as.formula(paste("poor ~", rhs, "+", ctrl_str, "| year")),
                data=dm, cluster=as.formula(paste0("~",cl_var)))
    bind_rows(tidy_fe(fp,lab,"ln_pcep"), tidy_fe(fv,lab,"poor"))
  }
  dem_tbl <- bind_rows(
    dem_run("dry_dm + conf_dm + dry_x_conf_dm", "demean_int"),
    dem_run("dry_dm + bin_dm + dry_x_bin_dm",   "demean_bin"))
  write_csv(dem_tbl, file.path(out_dir, paste0("pc_", tag, "_demean.csv")))

  # ----- land productivity outcomes (IHSES 2012 cross-section only) -----
  # land variables are only observed in the 2012 wave (wave==2), so this block is
  # a single cross-section with no fixed effects. "centered" variables subtract
  # the mean so the main effects can be read at the other variable's average.
  land_ctrl <- paste(c("age_head","male_head","edu_head","dep_ratio",
                       "ln_hhsize","n_young_male"), collapse=" + ")
  ls0 <- panel %>% filter(wave==2, !is.na(dryness), !is.na(conflict_int))
  if (level=="dis") ls0 <- ls0 %>% filter(!is.na(district_id))
  dbar <- mean(ls0$dryness); cbar2 <- mean(ls0$conflict_int)
  ls0 <- ls0 %>% mutate(dry_c = dryness - dbar, conf_c = conflict_int - cbar2,
                        dxc_c = dry_c*conf_c)
  la <- ls0 %>% filter(agri_area_donum>0 & !is.na(agri_area_donum))  # only farms with land
  # run_land() estimates one land outcome; "centered" chooses centred variables,
  # "extra" allows an extra control, "d" is the data (farms only vs all rural)
  run_land <- function(y, lab, d, extra="", centered=FALSE){
    vars <- if(centered) "dry_c + conf_c + dxc_c" else "dryness + conflict_int + dry_x_int"
    rhs <- paste(vars, "+", land_ctrl); if(nchar(extra)>0) rhs<-paste(rhs,"+",extra)
    fit <- feols(as.formula(paste(y,"~",rhs)), data=d, cluster=as.formula(paste0("~",cl_var)))
    tidy_fe(fit, lab, y)
  }
  land_c_tbl <- bind_rows(
    run_land("ln_agri_area","ln_area",la,"has_irrigation",TRUE),  # size of cultivated area
    run_land("has_agri_land","has_land",ls0,centered=TRUE),       # owns land (yes/no)
    run_land("has_irrigation","has_irrig",ls0,centered=TRUE),     # has irrigation (yes/no)
    run_land("has_livestock","has_lvk",ls0,centered=TRUE))        # has livestock (yes/no)
  write_csv(land_c_tbl, file.path(out_dir, paste0("pc_", tag, "_land_centered.csv")))

  # return everything this level produced
  list(lincom=lc, cbar=cbar, c90=c90, hi_share=hi_share,
       main=main_tbl, sub=sub_tbl, dyn=dyn_tbl, dem=dem_tbl, land_c=land_c_tbl)
}

# run the whole analysis at both levels and save the combined marginal effects
gov <- run_level("gov")
dis <- run_level("dis")
write_csv(bind_rows(gov$lincom, dis$lincom), file.path(out_dir, "pc_lincom.csv"))

# -----------------------------------------------------------------------------
# print readable summaries to the console so results can be copied into the paper
# -----------------------------------------------------------------------------
# fmt(): format one coefficient as "estimate*** (se)" with significance stars
fmt <- function(b,s,p){
  st <- ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
  sprintf("% .4f%s (%.4f)", b, st, s)
}
# show_block(): print a titled block, one line per specification, showing the
# requested variables for each outcome, plus N and R-squared
show_block <- function(tdy, vars, title){
  cat("\n", title, "\n", sep = "")
  t2 <- tdy %>% filter(variable %in% vars)
  for(oc in unique(t2$outcome)){
    cat("[", oc, "]\n")
    sub <- t2 %>% filter(outcome==oc)
    for(lb in unique(sub$label)){
      r <- sub %>% filter(label==lb)
      cat(sprintf("  %-9s", lb))
      for(v in vars){ rr <- r %>% filter(variable==v)
        cat(sprintf(" | %s=%s", v, if(nrow(rr)) fmt(rr$coef,rr$se,rr$p) else "."))}
      cat(sprintf("  [N=%s R2=%.3f]\n", r$n[1], r$r2[1]))
    }
  }
}

# print each block (main, subsamples, dynamic, demeaned, land) for both levels
for(lv in list(list("GOVERNORATE",gov), list("DISTRICT",dis))){
  nm <- lv[[1]]; o <- lv[[2]]
  cat("\n\n", nm, "\n", sep = "")
  show_block(o$main, c("dryness","conflict_int","dry_x_int","high_conf","dry_x_bin"), paste(nm,"MAIN S1-S4b"))
  show_block(o$sub,  c("dryness","conflict_int","dry_x_int","high_conf","dry_x_bin"), paste(nm,"SUBSAMPLES (int + .bin)"))
  show_block(o$dyn,  c("dryness","conflict_int","dryness_lag1","conflict_int_lag1",
                       "dryness_lag2","conflict_int_lag2","dryness_lag3","conflict_int_lag3"), paste(nm,"DYNAMIC"))
  show_block(o$dem,  c("dry_dm","conf_dm","dry_x_conf_dm","bin_dm","dry_x_bin_dm"), paste(nm,"DEMEANED"))
  show_block(o$land_c, c("dry_c","conf_c","dxc_c"), paste(nm,"LAND CENTERED"))
}

# print the marginal effects table and the reference conflict levels per level
cat("\nlincom (marginal effect of dryness)\n")
print(as.data.frame(bind_rows(gov$lincom, dis$lincom)), row.names=FALSE, digits=4)
cat("\nGOV  C_bar=",round(gov$cbar,4)," p90=",round(gov$c90,4)," hi_share=",round(gov$hi_share,3),"\n")
cat("DIS  C_bar=",round(dis$cbar,4)," p90=",round(dis$c90,4)," hi_share=",round(dis$hi_share,3),"\n")
