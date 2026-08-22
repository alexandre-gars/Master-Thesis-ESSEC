# =============================================================================
# Goal of this script: build a yearly population figure for every Iraqi
# governorate (and every district) from 2004 to 2017. We need population because
# the conflict measure used in the paper is "deaths per 100,000 inhabitants", so
# we must divide fatalities by population. This script produces that population.
#
# How it works:
#   1. We only have a few official population "anchor" points (census 1997,
#      estimate 2009, census 2024). We fill in every year in between by assuming
#      the population grows at a constant yearly rate (log-linear interpolation).
#   2. Governorate population is official. District population is not available
#      by name, so we split each governorate's population across its districts
#      using how many people the survey observed in each district.
#
# Inputs:
#   data/final/panel_household_district.csv   (to get district weights)
# Outputs:
#   data/processed/population_gov_year.csv    (governorate, year, pop)
#   data/processed/population_district.csv    (district_id, governorate, year, pop)
# =============================================================================

# load the packages we use: dplyr (data manipulation), readr (read/write csv),
# tidyr (reshaping, here the crossing() helper). suppressPackageStartupMessages
# just hides the "package loaded" messages so the console stays clean.
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})

# folder where all outputs of this script are written
out_dir <- "data/processed"

# -----------------------------------------------------------------------------
# Step 1: official population anchor points for the 18 governorates.
# Source: COSIT (Iraqi statistics office) via citypopulation.de.
# Each row is one governorate with its population in 1997, 2009 and 2024.
# Halabja governorate was created in 2014; to keep a stable list of 18
# governorates we add its people back into Sulaymaniyah (governorate 1).
# -----------------------------------------------------------------------------
anchors <- tribble(
  ~governorate, ~gov_name,          ~p1997,    ~p2009,    ~p2024,
  1L, "As-Sulaymaniyah",  1362739,  1784853,  2401724,   # includes Halabja
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

# -----------------------------------------------------------------------------
# Step 2: turn the 3 anchor points into a value for every year 2004-2017.
# We assume constant yearly growth between anchors. The growth rate g is the
# factor by which population multiplies each year, so pop(year) = pop(anchor) *
# g^(number of years since the anchor). We use one growth rate for 1997-2009 and
# another for 2009-2024, and pick the right one depending on the year.
# -----------------------------------------------------------------------------
interp_pop <- function(p1997, p2009, p2024, year) {
  g_early <- (p2009 / p1997)^(1 / (2009 - 1997))   # yearly growth 1997 to 2009
  g_late  <- (p2024 / p2009)^(1 / (2024 - 2009))   # yearly growth 2009 to 2024
  ifelse(year <= 2009,
         p1997 * g_early^(year - 1997),   # years up to 2009 use the early rate
         p2009 * g_late ^(year - 2009))   # years after 2009 use the late rate
}

# list of the years we want a population for
years <- 2004:2017

# build the governorate-year population table:
#   crossing() makes every (governorate, year) combination,
#   mutate() computes the interpolated population and rounds it to a whole number,
#   select()/arrange() keep and order the columns we care about.
gov_pop <- anchors %>%
  crossing(year = years) %>%
  mutate(pop = round(interp_pop(p1997, p2009, p2024, year))) %>%
  select(governorate, gov_name, year, pop) %>%
  arrange(governorate, year)

# save the governorate population and print a quick sanity check (national totals
# in millions for the three survey years, so we can eyeball that they look right)
write_csv(gov_pop, file.path(out_dir, "population_gov_year.csv"))
cat("Wrote population_gov_year.csv  (", nrow(gov_pop), "rows )\n")
cat("Spot check 2007 / 2012 / 2017 totals (millions):\n")
print(gov_pop %>% filter(year %in% c(2007, 2012, 2017)) %>%
        group_by(year) %>% summarise(total_m = round(sum(pop)/1e6, 2)))

# -----------------------------------------------------------------------------
# Step 3: split each governorate's population across its districts.
# We do not have official district population, so we use the survey itself: a
# district that contains more people in the survey (weighted by the sampling
# weight times household size) gets a larger share of the governorate total.
# -----------------------------------------------------------------------------
pd <- read_csv("data/final/panel_household_district.csv", show_col_types = FALSE, progress = FALSE)

# for each district, add up weight * household size to get its weighted number of
# people, then turn that into a share of its governorate (shares add up to 1
# within each governorate).
shares <- pd %>%
  filter(!is.na(district_id), !is.na(weight), !is.na(hh_size)) %>%
  mutate(wpop = weight * hh_size) %>%                    # weighted people per household
  group_by(governorate, district_id) %>%
  summarise(wpop = sum(wpop, na.rm = TRUE), .groups = "drop") %>%
  group_by(governorate) %>%
  mutate(share = wpop / sum(wpop)) %>%                   # district share of its governorate
  ungroup()

# some districts appear only in the 2017 wave, which has no sampling weights, so
# they got no weighted share above. For those we fall back to an equal share
# among the governorate's districts, then renormalise so shares still sum to 1.
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

# -----------------------------------------------------------------------------
# Step 4: district population = governorate population * district share.
# inner_join brings the yearly governorate population next to each district
# share, and we multiply the two. Because shares sum to 1 within a governorate,
# the district populations sum back to the official governorate total.
# -----------------------------------------------------------------------------
district_pop <- shares %>%
  select(governorate, district_id, share) %>%
  inner_join(gov_pop %>% select(governorate, year, gov_pop = pop),
             by = "governorate") %>%
  mutate(pop = round(gov_pop * share)) %>%
  select(district_id, governorate, year, pop) %>%
  arrange(district_id, year)

# save the district population table and print how many rows/districts we wrote
write_csv(district_pop, file.path(out_dir, "population_district.csv"))
cat("Wrote population_district.csv (", nrow(district_pop), "rows,",
    length(unique(district_pop$district_id)), "districts )\n")
