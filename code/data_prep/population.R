## 26_population.R
## Build an official-anchored population panel for Iraqi governorates and qhada.
##
## Governorate population: COSIT census/estimates (1997 census, 2009 estimate,
## 2024 census) compiled by citypopulation.de. Halabja (created 2014) is folded
## back into Sulaymaniyah so the series is consistent with the panel's 18
## governorates. Annual population 2004-2017 is obtained by log-linear
## interpolation between anchors.
##
## District (qhada) population: the panel stores only numeric qhada codes with no
## name crosswalk to official district tables, so official district figures cannot
## be joined directly. We instead allocate each governorate's official population
## to its qhada in proportion to the survey's weighted household population
## (sum of weight*hh_size, pooled over the 2007 and 2012 waves that carry
## sampling weights). District totals therefore sum to the official governorate
## population.
##
## Outputs:
##   data/processed/population_gov_year.csv   (governorate, year, pop)
##   data/processed/population_district.csv   (district_id, governorate, year, pop)
##
## Author: A. Gars

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})

out_dir <- "data/processed"

## ---- official governorate population anchors (panel gov coding 1-18) -----
## name | 1997 census | 2009 estimate | 2024 census  (COSIT via citypopulation.de)
## gov 1 (Sulaymaniyah) folds in Halabja for 2009 (+90,858) and 2024 (+113,926).
anchors <- tribble(
  ~governorate, ~gov_name,          ~p1997,    ~p2009,    ~p2024,
  1L, "As-Sulaymaniyah",  1362739,  1784853,  2401724,   # incl. Halabja
  2L, "Kirkuk (Ta'mim)",   753171,  1325853,  2034627,
  3L, "Ninawa",           2042852,  3138303,  4261980,
  4L, "Dahuk",             402970,  1040969,  1599871,
  5L, "Erbil",            1095992,  1532081,  2517534,
  6L, "Salah ad-Din",      904432,  1337786,  1774542,
  7L, "Diyala",           1135223,  1371035,  1934504,
  8L, "Al-Anbar",         1023736,  1483359,  2004418,
  9L, "Baghdad",          5423964,  6702538,  9780429,
  10L,"Babil",            1181751,  1729666,  2482324,
  11L,"Karbala",           594235,  1013254,  1754065,
  12L,"Wasit",             783614,  1150079,  1644273,
  13L,"Maysan",            637126,   922890,  1293636,
  14L,"Dhi Qar",          1184796,  1744398,  2499468,
  15L,"Al-Muthanna",       436825,   683126,  1043087,
  16L,"Al-Qadisiyah",      751331,  1077614,  1477310,
  17L,"An-Najaf",          775042,  1221228,  1950833,
  18L,"Al-Basrah",        1556445,  2405434,  3664168
)

## ---- log-linear interpolation to 2004-2017 -------------------------------
interp_pop <- function(p1997, p2009, p2024, year) {
  g_early <- (p2009 / p1997)^(1 / (2009 - 1997))   # annual growth 1997-2009
  g_late  <- (p2024 / p2009)^(1 / (2024 - 2009))   # annual growth 2009-2024
  ifelse(year <= 2009,
         p1997 * g_early^(year - 1997),
         p2009 * g_late ^(year - 2009))
}

years <- 2004:2017
gov_pop <- anchors %>%
  crossing(year = years) %>%
  mutate(pop = round(interp_pop(p1997, p2009, p2024, year))) %>%
  select(governorate, gov_name, year, pop) %>%
  arrange(governorate, year)

write_csv(gov_pop, file.path(out_dir, "population_gov_year.csv"))
cat("Wrote population_gov_year.csv  (", nrow(gov_pop), "rows )\n")
cat("Spot check 2007 / 2012 / 2017 totals (millions):\n")
print(gov_pop %>% filter(year %in% c(2007, 2012, 2017)) %>%
        group_by(year) %>% summarise(total_m = round(sum(pop)/1e6, 2)))

## ---- district shares from survey weighted population ---------------------
pd <- read_csv("data/final/panel_district.csv", show_col_types = FALSE, progress = FALSE)

shares <- pd %>%
  filter(!is.na(district_id), !is.na(weight), !is.na(hh_size)) %>%
  mutate(wpop = weight * hh_size) %>%
  group_by(governorate, district_id) %>%
  summarise(wpop = sum(wpop, na.rm = TRUE), .groups = "drop") %>%
  group_by(governorate) %>%
  mutate(share = wpop / sum(wpop)) %>%
  ungroup()

## any district with no weighted obs (2017-only): fall back to equal share
all_dist <- pd %>% filter(!is.na(district_id)) %>%
  distinct(governorate, district_id)
missing <- anti_join(all_dist, shares, by = c("governorate", "district_id"))
if (nrow(missing) > 0) {
  cat("Districts with no weighted obs (equal-share fallback):", nrow(missing), "\n")
  eq <- missing %>% group_by(governorate) %>% mutate(share = 1 / n(), wpop = NA_real_) %>% ungroup()
  shares <- bind_rows(shares, eq) %>%
    group_by(governorate) %>% mutate(share = share / sum(share)) %>% ungroup()
}

cat("District share coverage: ", nrow(shares), "districts across ",
    length(unique(shares$governorate)), "governorates\n")

## ---- district population = gov pop * within-gov share --------------------
district_pop <- shares %>%
  select(governorate, district_id, share) %>%
  inner_join(gov_pop %>% select(governorate, year, gov_pop = pop),
             by = "governorate") %>%
  mutate(pop = round(gov_pop * share)) %>%
  select(district_id, governorate, year, pop) %>%
  arrange(district_id, year)

write_csv(district_pop, file.path(out_dir, "population_district.csv"))
cat("Wrote population_district.csv (", nrow(district_pop), "rows,",
    length(unique(district_pop$district_id)), "districts )\n")
