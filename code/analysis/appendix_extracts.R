# =============================================================================
# This script produces two small tables for the appendix of the thesis, 
# from the household panel:
#   (A) the CSO Paasche price index (the deflator used to turn nominal spending
#       into real spending), reported by governorate and year;
#   (B) the poverty headcount (share of poor households) by governorate and wave.
#
# Input:  data/final/panel_household_gov.csv
# Outputs: outputs/tables/appx_paasche_index.csv
#          outputs/tables/appx_poverty_by_gov.csv
# =============================================================================

# dplyr for summarising, readr for reading/writing csv
suppressPackageStartupMessages({ library(dplyr); library(readr) })
out_dir <- "outputs/tables"; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# the panel stores governorates as numbers 1..18; this vector lets us attach a
# readable name by indexing gov_names[governorate]
gov_names <- c("Sulaymaniyah","At-Ta'mim (Kirkuk)","Nineveh","Dohuk","Erbil",
               "Salah ad-Din","Diyala","Anbar","Baghdad","Babil","Karbala",
               "Wasit","Maysan","Dhi-Qar","Al-Muthanna","Al-Qadisiyah",
               "Najaf","Basra")

# read the panel and add the readable governorate name as a new column "govn"
p <- read_csv("data/final/panel_household_gov.csv", show_col_types = FALSE, progress = FALSE)
p <- p %>% mutate(govn = gov_names[governorate])

# -----------------------------------------------------------------------------
# (A) Paasche price index by governorate-year.
# The index is the same for every household in a governorate-year, so grouping
# and averaging just recovers that single value. We also print how many distinct
# values exist and the national mean by wave, then save the table.
# -----------------------------------------------------------------------------
cat("\n(A) paasche price index\n")
cat("Distinct paasche values overall:", paste(sort(unique(round(p$paasche,4))), collapse=", "), "\n\n")
paasche_tab <- p %>%
  group_by(wave, year, governorate, govn) %>%
  summarise(paasche = mean(paasche, na.rm=TRUE), n = n(), .groups="drop") %>%
  arrange(wave, governorate)
print(paasche_tab, n = 100)
write_csv(paasche_tab, file.path(out_dir, "appx_paasche_index.csv"))

# national average of the deflator in each wave (min and max show its spread)
cat("\nMean paasche by wave (deflator base 2006):\n")
print(p %>% group_by(wave, year) %>% summarise(mean_paasche=mean(paasche,na.rm=TRUE),
        min=min(paasche,na.rm=TRUE), max=max(paasche,na.rm=TRUE), .groups="drop"))

# -----------------------------------------------------------------------------
# (B) poverty headcount by governorate and wave.
# For each governorate we take the mean of the 0/1 poverty indicator within each
# wave, which gives the share of poor households. poor[wave==1] selects only the
# rows from wave 1 (2007), and so on for 2012 and 2017.
# -----------------------------------------------------------------------------
cat("\n(B) poverty headcount by governorate x wave\n")
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

# national poverty rate in each wave (a quick check against known figures)
cat("\nNational poverty rate by wave:\n")
print(p %>% filter(!is.na(poor)) %>% group_by(wave,year) %>%
        summarise(poverty_rate=mean(poor), n=n(), .groups="drop"))

cat("\nSaved: appx_paasche_index.csv, appx_poverty_by_gov.csv\n")
