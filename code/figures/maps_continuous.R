# =============================================================================
# maps_continuous.R
# -----------------------------------------------------------------------------
# Draws two grids of maps, one small map per year from 2004 to 2017, for the 18
# governorates (GADM level-1 boundaries):
#   Figure 21: dryness (= -SPEI-4) by governorate and year.
#   Figure 22: conflict intensity (deaths per 100,000) by governorate and year.
# The conflict measure is the linear population-normalised one; the colour scale
# uses a square-root transform (not a log) so differences at low values are still
# visible while very high values do not dominate.
#
# Inputs:
#   data/external/shapefile/gadm41_IRQ_1.shp   (governorate polygons)
#   data/processed/climate_extended.csv         (SPEI -> dryness)
#   data/processed/conflict_gov_year_pc.csv      (conflict_int, deaths per 100k)
# Outputs (pdf and png) in outputs/figures/:
#   fig21_map_dryness_panel, fig22_map_conflict_panel
# =============================================================================

# sf handles the map polygons; dplyr/tidyr/readr for data; ggplot2 + viridis for
# the plots and colour scales
suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tidyr); library(ggplot2)
  library(viridis); library(readr)
})

# project folder and the two subfolders we read from / write to
ROOT <- "C:/Users/galex/Desktop/Thesis_Essec"
SHP  <- file.path(ROOT, "data", "external", "shapefile")
FIG  <- file.path(ROOT, "outputs", "figures")

# crosswalk from the shapefile's governorate name (NAME_1) to our numeric code
# 1..18, so the map polygons can be matched to the data. One row per governorate.
gadm_to_panel <- tibble::tribble(
  ~NAME_1,            ~gov_panel,
  "As-Sulaymaniyah",  1L,
  "At-Ta'mim",        2L,
  "Ninawa",           3L,
  "Dihok",            4L,
  "Arbil",            5L,
  "Sala ad-Din",      6L,
  "Diyala",           7L,
  "Al-Anbar",         8L,
  "Baghdad",          9L,
  "Babil",           10L,
  "Karbala'",        11L,
  "Wasit",           12L,
  "Maysan",          13L,
  "Dhi-Qar",         14L,
  "Al-Muthannia",    15L,
  "Al-Qadisiyah",    16L,
  "An-Najaf",        17L,
  "Al-Basrah",       18L
)

# read the governorate polygons and attach our numeric code. The stopifnot()
# check stops the script if any polygon failed to match (a safety guard against a
# misspelled name in the crosswalk above).
gov_sf <- st_read(file.path(SHP, "gadm41_IRQ_1.shp"), quiet = TRUE) %>%
  left_join(gadm_to_panel, by = "NAME_1")
stopifnot(!any(is.na(gov_sf$gov_panel)))

# the years to draw
YEARS <- 2004:2017

# dryness by governorate and year (transmute keeps only the columns we need and
# turns SPEI into dryness by negating it)
clim <- read_csv(file.path(ROOT, "data/processed/climate_extended.csv"),
                 show_col_types = FALSE, progress = FALSE) %>%
  transmute(gov_panel = as.integer(governorate), year = as.integer(year),
            dryness = -spei3_annual_mean) %>%
  filter(year %in% YEARS)

# conflict (deaths per 100,000) by governorate and year
conf <- read_csv(file.path(ROOT, "data/processed/conflict_gov_year_pc.csv"),
                 show_col_types = FALSE, progress = FALSE) %>%
  transmute(gov_panel = as.integer(governorate), year = as.integer(year),
            conflict_pc = conflict_int) %>%
  filter(year %in% YEARS)

# a stripped-down map theme: no axes, no gridlines, legend at the bottom
map_theme <- theme_minimal(base_size = 9) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.4, "cm"),
        strip.text = element_text(size = 9, face = "bold"))

# -----------------------------------------------------------------------------
# Figure 21: dryness maps, one panel per year (facet_wrap by year).
# The join is many-to-many because each governorate polygon is repeated once per
# year. A diverging blue-white-red scale centres at 0 (white = normal year).
# -----------------------------------------------------------------------------
dry_sf <- gov_sf %>% left_join(clim, by = "gov_panel",
                               relationship = "many-to-many")
p_dry <- ggplot(dry_sf) +
  geom_sf(aes(fill = dryness), colour = "white", linewidth = 0.12) +
  facet_wrap(~ year, ncol = 5) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, name = "Dryness (−SPEI-4)",
                       limits = c(-0.6, 1.9), na.value = "grey85") +
  map_theme + coord_sf(expand = FALSE)
ggsave(file.path(FIG, "fig21_map_dryness_panel.pdf"), p_dry,
       width = 11, height = 7.5, units = "in")
ggsave(file.path(FIG, "fig21_map_dryness_panel.png"), p_dry,
       width = 11, height = 7.5, units = "in", dpi = 150)
cat("Wrote fig21_map_dryness_panel\n")

# -----------------------------------------------------------------------------
# Figure 22: conflict maps, one panel per year.
# The colour scale uses a square-root transform so low-conflict years stay
# distinguishable; oob = squish caps values above the limit at the top colour.
# -----------------------------------------------------------------------------
conf_sf <- gov_sf %>% left_join(conf, by = "gov_panel",
                                relationship = "many-to-many")
p_conf <- ggplot(conf_sf) +
  geom_sf(aes(fill = conflict_pc), colour = "white", linewidth = 0.12) +
  facet_wrap(~ year, ncol = 5) +
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       name = "Conflict (deaths / 100,000)",
                       trans = "sqrt", breaks = c(0, 10, 50, 100, 200),
                       limits = c(0, 272), oob = scales::squish,
                       na.value = "grey85") +
  map_theme + coord_sf(expand = FALSE)
ggsave(file.path(FIG, "fig22_map_conflict_panel.pdf"), p_conf,
       width = 11, height = 7.5, units = "in")
ggsave(file.path(FIG, "fig22_map_conflict_panel.png"), p_conf,
       width = 11, height = 7.5, units = "in", dpi = 150)
cat("Wrote fig22_map_conflict_panel\n")
