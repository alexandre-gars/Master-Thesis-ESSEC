## 28_analysis_pc.R  (REVISED 2026-06-09)
## Full regression suite for final.tex with the LINEAR conflict-intensity
## measure conflict_int = 1e5*fat/pop (deaths per 100,000) built in 27_conflict_pc.R,
## plus a BINARY high-conflict indicator high_conf = 1(conflict_int >= p90).
##
## For every specification that contains an interaction we now report TWO cases:
##   (a) INTENSITY : interaction = dryness * conflict_int
##   (b) BINARY    : interaction = dryness * high_conf
## The non-interaction specifications (S1-S3) and the dynamic / land tables use
## the intensity measure only (per user instruction: binary applies to Main,
## Subsamples, Demeaned).
##
## Runs gov (gov FE / gov cluster) and district (district FE / district cluster).
## Author: A. Gars

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(fixest)
})
set.seed(42)
out_dir <- "outputs/tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

clim <- read_csv("data/processed/climate_extended.csv", show_col_types = FALSE, progress = FALSE)
cpc  <- read_csv("data/processed/conflict_gov_year_pc.csv", show_col_types = FALSE, progress = FALSE)

## ---------- common preparation ----------
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
  panel <- panel %>% mutate(dry_x_int = dryness * conflict_int,
                            dry_x_bin = dryness * high_conf)
  panel <- panel %>%
    mutate(krg = as.integer(governorate %in% c(1L,4L,5L)),
           isis = as.integer(governorate %in% c(3L,6L,7L,8L)),
           south = as.integer(governorate %in% c(13L,14L,18L)))
  panel
}

controls <- c("age_head","male_head","edu_head","dep_ratio",
              "ln_hhsize","n_young_male","has_agri_land",
              "ln_agri_area","has_irrigation")
ctrl_str <- paste(controls, collapse = " + ")

tidy_fe <- function(fit, label, outcome) {
  est <- coeftable(fit)
  tibble(label = label, outcome = outcome, variable = rownames(est),
         coef = est[,1], se = est[,2], t = est[,3], p = est[,4],
         n = nobs(fit), r2 = tryCatch(fitstat(fit,"r2")$r2, error=function(e) NA))
}

run_pair <- function(data, label, rhs, fe_var, cl_var) {
  f_pce <- as.formula(paste("ln_pcep ~", rhs, "+", ctrl_str, "|", fe_var, "+ year"))
  f_pov <- as.formula(paste("poor    ~", rhs, "+", ctrl_str, "|", fe_var, "+ year"))
  fit_p <- feols(f_pce, data = data, cluster = as.formula(paste0("~", cl_var)))
  fit_v <- feols(f_pov, data = data, cluster = as.formula(paste0("~", cl_var)))
  list(pce = fit_p, pov = fit_v,
       tidy = bind_rows(tidy_fe(fit_p, label, "ln_pcep"),
                        tidy_fe(fit_v, label, "poor")))
}

## generic lincom of dryness slope at moderator value m: b[d] + m*b[ix]
lincom_dry <- function(fit, m, outcome, spec, dvar="dryness", ixvar="dry_x_int") {
  b <- coef(fit); V <- vcov(fit)
  est <- b[dvar] + m * b[ixvar]
  va  <- V[dvar,dvar] + m^2 * V[ixvar,ixvar] + 2*m*V[dvar,ixvar]
  se  <- sqrt(va); z <- est/se
  tibble(spec=spec, outcome=outcome, m=m, lincom=unname(est),
         se=unname(se), z=unname(z), p=2*pnorm(-abs(unname(z))))
}

run_level <- function(level) {
  if (level == "gov") {
    panel <- prep_panel("data/final/panel.csv")
    fe_var <- "governorate"; cl_var <- "governorate"; tag <- "gov"
  } else {
    panel <- prep_panel("data/final/panel_district.csv")
    fe_var <- "district_id"; cl_var <- "district_id"; tag <- "dis"
  }
  samp <- panel %>% filter(!is.na(ln_pcep), !is.na(poor), !is.na(dryness),
                           !is.na(conflict_int), !is.na(high_conf))
  if (level == "dis") samp <- samp %>% filter(!is.na(district_id))
  cbar <- mean(samp$conflict_int); c90 <- quantile(samp$conflict_int, 0.90)
  hi_share <- mean(samp$high_conf)
  cat("\n###### LEVEL:", level, " N =", nrow(samp),
      if (level=="dis") paste("| districts:", length(unique(samp$district_id))) else "", "######\n")
  cat(sprintf("mean conflict_int (C_bar)=%.4f | p90 conflict_int=%.4f | share high_conf=%.3f\n",
              cbar, c90, hi_share))

  ## ----- progressive specs: S1-S3 + S4(intensity) + S4b(binary) -----
  s1  <- run_pair(samp, "S1",  "dryness", fe_var, cl_var)
  s2  <- run_pair(samp, "S2",  "conflict_int", fe_var, cl_var)
  s3  <- run_pair(samp, "S3",  "dryness + conflict_int", fe_var, cl_var)
  s4  <- run_pair(samp, "S4",  "dryness + conflict_int + dry_x_int", fe_var, cl_var)
  s4b <- run_pair(samp, "S4b", "dryness + high_conf + dry_x_bin",    fe_var, cl_var)
  main_tbl <- bind_rows(s1$tidy, s2$tidy, s3$tidy, s4$tidy, s4b$tidy)
  write_csv(main_tbl, file.path(out_dir, paste0("pc_", tag, "_main.csv")))

  ## lincoms: intensity S4 at C=0 / Cbar / p90 ; binary S4b at high=0 / high=1
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

  ## ----- subsamples: S4 (intensity) AND S4b (binary) -----
  subs <- list(Full=samp, KRG=filter(samp,krg==1), NonKRG=filter(samp,krg==0),
               ISIS=filter(samp,isis==1), NonISIS=filter(samp,isis==0))
  run_sub <- function(rhs, suffix){
    bind_rows(lapply(names(subs), function(nm){
      d <- subs[[nm]]; if (nrow(d) < 50) return(NULL)
      tryCatch(run_pair(d, paste0(nm,suffix), rhs, fe_var, cl_var)$tidy,
               error=function(e){cat("sub",nm,suffix,"fail\n");NULL})
    }))
  }
  sub_int <- run_sub("dryness + conflict_int + dry_x_int", "")
  sub_bin <- run_sub("dryness + high_conf + dry_x_bin",    ".bin")
  sub_tbl <- bind_rows(sub_int, sub_bin)
  write_csv(sub_tbl, file.path(out_dir, paste0("pc_", tag, "_sub.csv")))

  ## ----- dynamic: lagged MAIN effects (intensity), no interaction -----
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

  ## ----- demeaned (within group) main spec: intensity AND binary -----
  grp <- fe_var
  dm <- samp %>% group_by(.data[[grp]]) %>%
    mutate(dry_dm  = dryness - mean(dryness),
           conf_dm = conflict_int - mean(conflict_int),
           bin_dm  = high_conf - mean(high_conf)) %>%
    ungroup() %>%
    mutate(dry_x_conf_dm = dry_dm * conf_dm,
           dry_x_bin_dm  = dry_dm * bin_dm)
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

  ## ----- land productivity (IHSES 2012 cross-section, intensity) -----
  land_ctrl <- paste(c("age_head","male_head","edu_head","dep_ratio",
                       "ln_hhsize","n_young_male"), collapse=" + ")
  ls0 <- panel %>% filter(wave==2, !is.na(dryness), !is.na(conflict_int))
  if (level=="dis") ls0 <- ls0 %>% filter(!is.na(district_id))
  dbar <- mean(ls0$dryness); cbar2 <- mean(ls0$conflict_int)
  ls0 <- ls0 %>% mutate(dry_c = dryness - dbar, conf_c = conflict_int - cbar2,
                        dxc_c = dry_c*conf_c)
  la <- ls0 %>% filter(agri_area_donum>0 & !is.na(agri_area_donum))
  run_land <- function(y, lab, d, extra="", centered=FALSE){
    vars <- if(centered) "dry_c + conf_c + dxc_c" else "dryness + conflict_int + dry_x_int"
    rhs <- paste(vars, "+", land_ctrl); if(nchar(extra)>0) rhs<-paste(rhs,"+",extra)
    fit <- feols(as.formula(paste(y,"~",rhs)), data=d, cluster=as.formula(paste0("~",cl_var)))
    tidy_fe(fit, lab, y)
  }
  land_c_tbl <- bind_rows(
    run_land("ln_agri_area","ln_area",la,"has_irrigation",TRUE),
    run_land("has_agri_land","has_land",ls0,centered=TRUE),
    run_land("has_irrigation","has_irrig",ls0,centered=TRUE),
    run_land("has_livestock","has_lvk",ls0,centered=TRUE))
  write_csv(land_c_tbl, file.path(out_dir, paste0("pc_", tag, "_land_centered.csv")))

  list(lincom=lc, cbar=cbar, c90=c90, hi_share=hi_share,
       main=main_tbl, sub=sub_tbl, dyn=dyn_tbl, dem=dem_tbl, land_c=land_c_tbl)
}

gov <- run_level("gov")
dis <- run_level("dis")
write_csv(bind_rows(gov$lincom, dis$lincom), file.path(out_dir, "pc_lincom.csv"))

## ================= PRINT SUMMARIES FOR TRANSCRIPTION =================
fmt <- function(b,s,p){
  st <- ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
  sprintf("% .4f%s (%.4f)", b, st, s)
}
show_block <- function(tdy, vars, title){
  cat("\n====", title, "====\n")
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

for(lv in list(list("GOVERNORATE",gov), list("DISTRICT",dis))){
  nm <- lv[[1]]; o <- lv[[2]]
  cat("\n\n===================", nm, "===================\n")
  show_block(o$main, c("dryness","conflict_int","dry_x_int","high_conf","dry_x_bin"), paste(nm,"MAIN S1-S4b"))
  show_block(o$sub,  c("dryness","conflict_int","dry_x_int","high_conf","dry_x_bin"), paste(nm,"SUBSAMPLES (int + .bin)"))
  show_block(o$dyn,  c("dryness","conflict_int","dryness_lag1","conflict_int_lag1",
                       "dryness_lag2","conflict_int_lag2","dryness_lag3","conflict_int_lag3"), paste(nm,"DYNAMIC"))
  show_block(o$dem,  c("dry_dm","conf_dm","dry_x_conf_dm","bin_dm","dry_x_bin_dm"), paste(nm,"DEMEANED"))
  show_block(o$land_c, c("dry_c","conf_c","dxc_c"), paste(nm,"LAND CENTERED"))
}

cat("\n==== LINCOM (marginal effect of dryness) ====\n")
print(as.data.frame(bind_rows(gov$lincom, dis$lincom)), row.names=FALSE, digits=4)
cat("\nGOV  C_bar=",round(gov$cbar,4)," p90=",round(gov$c90,4)," hi_share=",round(gov$hi_share,3),"\n")
cat("DIS  C_bar=",round(dis$cbar,4)," p90=",round(dis$c90,4)," hi_share=",round(dis$hi_share,3),"\n")
