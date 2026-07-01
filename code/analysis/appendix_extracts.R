## 32_appendix_extracts.R
## One-off extraction to document, for the defense backup slides and the
## thesis appendix:
##   (A) the CSO Paasche price index actually stored in the panel (by gov-year);
##   (B) the official poverty headcount by governorate and survey wave;
##   (C) how the occupational-composition label (stay-farmer / urban-settled /
##       rural non-agri) is built, reverse-engineered from the panel primitives.
## Author: A. Gars

suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr) })
out_dir <- "outputs/tables"; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gov_names <- c("Sulaymaniyah","At-Ta'mim (Kirkuk)","Nineveh","Dohuk","Erbil",
               "Salah ad-Din","Diyala","Anbar","Baghdad","Babil","Karbala",
               "Wasit","Maysan","Dhi-Qar","Al-Muthanna","Al-Qadisiyah",
               "Najaf","Basra")

p <- read_csv("data/final/panel.csv", show_col_types = FALSE, progress = FALSE)
p <- p %>% mutate(govn = gov_names[governorate])

## ---------------------------------------------------------------------------
## (A) CSO Paasche price index by governorate-year
## ---------------------------------------------------------------------------
cat("\n================ (A) PAASCHE PRICE INDEX ================\n")
cat("Distinct paasche values overall:", paste(sort(unique(round(p$paasche,4))), collapse=", "), "\n\n")
paasche_tab <- p %>%
  group_by(wave, year, governorate, govn) %>%
  summarise(paasche = mean(paasche, na.rm=TRUE), n = n(), .groups="drop") %>%
  arrange(wave, governorate)
print(paasche_tab, n = 100)
write_csv(paasche_tab, file.path(out_dir, "appx_paasche_index.csv"))

## national mean paasche by wave
cat("\nMean paasche by wave (deflator base 2006):\n")
print(p %>% group_by(wave, year) %>% summarise(mean_paasche=mean(paasche,na.rm=TRUE),
        min=min(paasche,na.rm=TRUE), max=max(paasche,na.rm=TRUE), .groups="drop"))

## ---------------------------------------------------------------------------
## (B) Poverty headcount by governorate and wave
## ---------------------------------------------------------------------------
cat("\n================ (B) POVERTY HEADCOUNT BY GOVERNORATE x WAVE ================\n")
pov <- p %>% filter(!is.na(poor)) %>%
  group_by(governorate, govn) %>%
  summarise(
    pov_2007 = mean(poor[wave==1], na.rm=TRUE),
    pov_2012 = mean(poor[wave==2], na.rm=TRUE),
    pov_2017 = mean(poor[wave==3], na.rm=TRUE),
    n = n(), .groups="drop") %>%
  arrange(governorate)
print(pov, n=100)
write_csv(pov, file.path(out_dir, "appx_poverty_by_gov.csv"))

cat("\nNational poverty rate by wave:\n")
print(p %>% filter(!is.na(poor)) %>% group_by(wave,year) %>%
        summarise(poverty_rate=mean(poor), n=n(), .groups="drop"))

## ---------------------------------------------------------------------------
## (C) Occupational composition label: reverse-engineer the rule
## ---------------------------------------------------------------------------
cat("\n================ (C) STRATEGIC-CHOICE / COMPOSITION LABEL ================\n")
cat("Label codes present:\n")
print(p %>% filter(!is.na(strategic_choice)) %>%
        count(strategic_choice, strategic_choice_label))

## cross-tab against the primitives that should define it
prim <- p %>% filter(!is.na(strategic_choice)) %>%
  mutate(sc = factor(strategic_choice, levels=c(1,2,3),
                     labels=c("StayFarmer","UrbanSettled","RuralNonAgri")))
cat("\nMean of each primitive variable within each label:\n")
print(prim %>% group_by(sc) %>%
        summarise(n=n(),
                  migration_flag = mean(migration_flag, na.rm=TRUE),
                  urban          = mean(urban, na.rm=TRUE),
                  rural          = mean(rural, na.rm=TRUE),
                  has_agri_land  = mean(has_agri_land, na.rm=TRUE),
                  occ_agri       = mean(occ_agri, na.rm=TRUE),
                  industry_agri  = mean(industry_agri, na.rm=TRUE),
                  migration_dest_urban = mean(migration_dest_urban, na.rm=TRUE)))

## exact logical reconstruction test
chk <- prim %>% mutate(
  rule = case_when(
    migration_flag == 1 & migration_dest_urban == 1 ~ "UrbanSettled",
    migration_flag == 1 & (migration_dest_urban == 0 | is.na(migration_dest_urban)) ~ "RuralNonAgri",
    migration_flag == 0 & (has_agri_land == 1 | occ_agri == 1) ~ "StayFarmer",
    TRUE ~ "RuralNonAgri"))
cat("\nReconstruction rule vs stored label (agreement matrix):\n")
print(table(stored = chk$sc, reconstructed = chk$rule))
cat("\nAgreement rate:", round(mean(as.character(chk$sc)==chk$rule, na.rm=TRUE),3), "\n")

## counts by wave (for the figure 5 cross-check)
cat("\nComposition counts by wave:\n")
print(prim %>% count(wave, sc) %>% pivot_wider(names_from=sc, values_from=n, values_fill=0))

write_csv(prim %>% count(wave, sc) %>%
            pivot_wider(names_from=sc, values_from=n, values_fill=0),
          file.path(out_dir, "appx_composition_by_wave.csv"))
cat("\nSaved: appx_paasche_index.csv, appx_poverty_by_gov.csv, appx_composition_by_wave.csv\n")
