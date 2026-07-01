# ─────────────────────────────────────────────────────────────────────────────
# 20_maps.R
# Real geospatial maps of Iraq for the paper:
#   * Governorate-level (GADM L1, 18 polygons) choropleth of SPEI, conflict,
#     welfare, share of households owning agricultural land.
#   * District-level (GADM L2, 102 polygons) choropleth of SPEI by wave.
#   * NDVI/EVI national time series vs SPEI.
# Inputs:
#   data/external/shapefile/gadm41_IRQ_1.shp     governorates
#   data/external/shapefile/gadm41_IRQ_2.shp     districts (qhada)
#   data/final/panel_district.csv                merged HH x year panel
#   data/processed/climate_gov_year.csv          SPEI by gov x year
#   data/processed/conflict_gov_year.csv         conflict by gov x year
#   data/external/climate/iraq_ndvi_evi_2006_2025.csv
# Outputs:
#   outputs/figures/fig13_gov_map_spei.pdf|png
#   outputs/figures/fig14_gov_map_conflict.pdf|png
#   outputs/figures/fig15_gov_map_welfare.pdf|png
#   outputs/figures/fig16_gov_map_land_share.pdf|png
#   outputs/figures/fig17_district_map_spei.pdf|png
#   outputs/figures/fig18_ndvi_evi_drought.pdf|png
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridis)
  library(cowplot)
  library(readr)
  library(stringr)
})

ROOT <- "C:/Users/galex/Desktop/Thesis_Essec"
SHP  <- file.path(ROOT, "data", "external", "shapefile")
DAT  <- file.path(ROOT, "data")
FIG  <- file.path(ROOT, "outputs", "figures")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

# ── Crosswalk: GADM NAME_1 → panel governorate code (1..18) ──────────────────
gadm_to_panel <- tibble::tribble(
  ~NAME_1,            ~gov_panel, ~gov_name,
  "As-Sulaymaniyah",  1L, "Sulaymaniyah",
  "At-Ta'mim",        2L, "Kirkuk",
  "Ninawa",           3L, "Nineveh",
  "Dihok",            4L, "Duhok",
  "Arbil",            5L, "Erbil",
  "Sala ad-Din",      6L, "Salah ad-Din",
  "Diyala",           7L, "Diyala",
  "Al-Anbar",         8L, "Anbar",
  "Baghdad",          9L, "Baghdad",
  "Babil",           10L, "Babylon",
  "Karbala'",        11L, "Karbala",
  "Wasit",           12L, "Wasit",
  "Maysan",          13L, "Maysan",
  "Dhi-Qar",         14L, "Dhi Qar",
  "Al-Muthannia",    15L, "Muthanna",
  "Al-Qadisiyah",    16L, "Qadisiyyah",
  "An-Najaf",        17L, "Najaf",
  "Al-Basrah",       18L, "Basra"
)

# ── Read shapefiles ──────────────────────────────────────────────────────────
gov_sf  <- st_read(file.path(SHP, "gadm41_IRQ_1.shp"), quiet = TRUE)
dist_sf <- st_read(file.path(SHP, "gadm41_IRQ_2.shp"), quiet = TRUE)

gov_sf <- gov_sf %>% left_join(gadm_to_panel, by = "NAME_1")
stopifnot(!any(is.na(gov_sf$gov_panel)))

dist_sf <- dist_sf %>%
  left_join(gadm_to_panel, by = "NAME_1") %>%
  filter(!is.na(gov_panel))

# ── Read panel data ──────────────────────────────────────────────────────────
panel <- read_csv(file.path(DAT, "final", "panel_district.csv"),
                  show_col_types = FALSE) %>%
  filter(!is.na(governorate), !is.na(year))

gov_panel_wave <- panel %>%
  group_by(gov_panel = as.integer(governorate), year = as.integer(year)) %>%
  summarise(
    spei      = mean(spei3_annual_mean, na.rm = TRUE),
    conflict  = mean(conflict_intensity, na.rm = TRUE),
    ln_pcep   = mean(ln_pcep, na.rm = TRUE),
    poor      = mean(poor, na.rm = TRUE),
    n_hh      = dplyr::n(),
    has_land  = mean(has_agri_land, na.rm = TRUE),
    has_irrig = mean(has_irrigation, na.rm = TRUE),
    .groups   = "drop"
  )

# ── Helper to build a map for a given outcome and wave ───────────────────────
common_theme <- theme_minimal(base_size = 10) +
  theme(
    axis.title       = element_text(size = 9),
    axis.text        = element_text(size = 7),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.2),
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 7),
    plot.title       = element_text(size = 11, face = "bold"),
    plot.subtitle    = element_text(size = 9),
    strip.text       = element_text(size = 10, face = "bold")
  )

# Plain-English axis labels for the maps
LON_LAB <- "Longitude (degrees east)"
LAT_LAB <- "Latitude (degrees north)"

make_gov_map <- function(data_var, fill_lab, palette, direction = 1,
                         midpoint = NULL, limits = NULL, title_prefix) {
  sf_with <- gov_sf %>% left_join(gov_panel_wave, by = "gov_panel",
                                  relationship = "many-to-many")
  p <- ggplot(sf_with) +
    geom_sf(aes(fill = .data[[data_var]]), colour = "white",
            linewidth = 0.25) +
    geom_sf_text(data = gov_sf,
                 aes(label = gov_name), size = 1.9,
                 colour = "black", check_overlap = TRUE) +
    facet_wrap(~ year, ncol = 3, labeller = labeller(year = function(y)
      paste0(title_prefix, " — ", y))) +
    labs(x = LON_LAB, y = LAT_LAB) +
    common_theme +
    coord_sf(expand = FALSE)
  if (is.null(midpoint)) {
    p <- p + scale_fill_viridis_c(option = palette, direction = direction,
                                  name = fill_lab,
                                  limits = limits,
                                  na.value = "grey85")
  } else {
    p <- p + scale_fill_gradient2(low = "#b2182b", mid = "white",
                                  high = "#2166ac",
                                  midpoint = midpoint, name = fill_lab,
                                  limits = limits, na.value = "grey85")
  }
  p
}

# ── Figure: SPEI by governorate × year ───────────────────────────────────────
p_spei <- make_gov_map("spei", "SPEI-4 annual mean",
                       palette = "B", direction = -1,
                       midpoint = 0,
                       limits = c(-1.8, 0.4),
                       title_prefix = "SPEI-4")
ggsave(file.path(FIG, "fig13_gov_map_spei.pdf"), p_spei,
       width = 12, height = 4.6, units = "in")
ggsave(file.path(FIG, "fig13_gov_map_spei.png"), p_spei,
       width = 12, height = 4.6, units = "in", dpi = 150)

# ── Figure: conflict intensity by governorate × year ─────────────────────────
p_conf <- make_gov_map("conflict", "log(1+fatalities)",
                       palette = "rocket", direction = -1,
                       limits = c(0, 8),
                       title_prefix = "Conflict intensity")
ggsave(file.path(FIG, "fig14_gov_map_conflict.pdf"), p_conf,
       width = 12, height = 4.6, units = "in")
ggsave(file.path(FIG, "fig14_gov_map_conflict.png"), p_conf,
       width = 12, height = 4.6, units = "in", dpi = 150)

# ── Figure: mean log PCE by governorate × year ───────────────────────────────
p_w <- make_gov_map("ln_pcep", "Mean log PCE",
                    palette = "G", direction = 1,
                    title_prefix = "Welfare")
ggsave(file.path(FIG, "fig15_gov_map_welfare.pdf"), p_w,
       width = 12, height = 4.6, units = "in")
ggsave(file.path(FIG, "fig15_gov_map_welfare.png"), p_w,
       width = 12, height = 4.6, units = "in", dpi = 150)

# ── Figure: share of households owning agricultural land (2012 only) ─────────
gov_land_2012 <- gov_sf %>%
  left_join(filter(gov_panel_wave, year == 2012), by = "gov_panel")
p_land <- ggplot(gov_land_2012) +
  geom_sf(aes(fill = has_land), colour = "white", linewidth = 0.25) +
  geom_sf_text(aes(label = gov_name), size = 2.2, colour = "black",
               check_overlap = TRUE) +
  scale_fill_viridis_c(option = "D", direction = 1,
                       name = "Share with land", na.value = "grey85",
                       labels = scales::percent_format(accuracy = 1)) +
  common_theme +
  labs(title = "Share of households owning agricultural land",
       subtitle = "IHSES 2012") +
  coord_sf(expand = FALSE)
ggsave(file.path(FIG, "fig16_gov_map_land_share.pdf"), p_land,
       width = 7, height = 7, units = "in")
ggsave(file.path(FIG, "fig16_gov_map_land_share.png"), p_land,
       width = 7, height = 7, units = "in", dpi = 150)

# ── Figure: district-level SPEI by year (assigned at gov level) ──────────────
# GADM L2 has 102 districts; we colour them by their governorate SPEI value
# (district-level SPEI is constant within governorate in our data).
dist_with <- dist_sf %>% left_join(gov_panel_wave, by = "gov_panel",
                                   relationship = "many-to-many")
p_dist <- ggplot(dist_with) +
  geom_sf(aes(fill = spei), colour = "white", linewidth = 0.12) +
  geom_sf(data = gov_sf, fill = NA, colour = "black", linewidth = 0.35) +
  facet_wrap(~ year, ncol = 3,
             labeller = labeller(year = function(y) paste0("SPEI-4 — ", y))) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                       midpoint = 0, name = "SPEI-4",
                       limits = c(-1.8, 0.4), na.value = "grey85") +
  common_theme +
  coord_sf(expand = FALSE)
ggsave(file.path(FIG, "fig17_district_map_spei.pdf"), p_dist,
       width = 12, height = 4.6, units = "in")
ggsave(file.path(FIG, "fig17_district_map_spei.png"), p_dist,
       width = 12, height = 4.6, units = "in", dpi = 150)

# ── Figure: NDVI/EVI vs SPEI national time series ────────────────────────────
ndvi <- read_csv(file.path(DAT, "external", "climate",
                           "iraq_ndvi_evi_2006_2025.csv"),
                 show_col_types = FALSE) %>%
  mutate(year = as.integer(substr(date, 1, 4)),
         month = as.integer(substr(date, 6, 7))) %>%
  group_by(year) %>%
  summarise(NDVI = mean(NDVI, na.rm = TRUE),
            EVI  = mean(EVI,  na.rm = TRUE),
            NDVI_growing = mean(NDVI[month %in% 3:6], na.rm = TRUE),
            .groups = "drop")

clim_nat <- read_csv(file.path(DAT, "processed", "climate_gov_year.csv"),
                     show_col_types = FALSE) %>%
  group_by(year) %>%
  summarise(spei = mean(spei3_annual_mean, na.rm = TRUE), .groups = "drop")
conf_nat <- read_csv(file.path(DAT, "processed", "conflict_gov_year.csv"),
                     show_col_types = FALSE) %>%
  group_by(year) %>%
  summarise(conflict = mean(conflict_intensity, na.rm = TRUE),
            .groups = "drop")

ts_dat <- ndvi %>% left_join(clim_nat, by = "year") %>%
  left_join(conf_nat, by = "year") %>%
  filter(!is.na(spei))

# Scaling factor for dual axis: align SPEI to NDVI range
ndvi_range <- range(ts_dat$NDVI, na.rm = TRUE)
spei_range <- range(ts_dat$spei, na.rm = TRUE)
ratio_spei_to_ndvi <- diff(ndvi_range) / diff(spei_range)
offset_spei <- ndvi_range[1] - spei_range[1] * ratio_spei_to_ndvi

p_a <- ggplot(ts_dat, aes(x = year)) +
  geom_line(aes(y = NDVI, colour = "NDVI"), linewidth = 0.9) +
  geom_line(aes(y = EVI,  colour = "EVI"),  linewidth = 0.9, linetype = "dashed") +
  geom_point(aes(y = NDVI, colour = "NDVI")) +
  geom_point(aes(y = EVI,  colour = "EVI")) +
  geom_line(aes(y = spei * ratio_spei_to_ndvi + offset_spei,
                colour = "SPEI"),
            linewidth = 0.9) +
  scale_y_continuous(
    name = "NDVI / EVI annual mean",
    sec.axis = sec_axis(~ (. - offset_spei) / ratio_spei_to_ndvi,
                        name = "SPEI-4 annual mean")
  ) +
  scale_colour_manual(values = c("NDVI" = "#2b8a3e",
                                 "EVI" = "#74b816",
                                 "SPEI" = "#1c7ed6"),
                      name = NULL) +
  geom_hline(yintercept = -1.5 * ratio_spei_to_ndvi + offset_spei,
             linetype = "dotted", colour = "#fa5252") +
  labs(title = "Vegetation greenness and SPEI, Iraq 2006–2023",
       x = NULL) +
  common_theme +
  theme(legend.position = "bottom")

p_b <- ggplot(ts_dat, aes(x = spei, y = NDVI)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey40", linewidth = 0.5) +
  geom_point(aes(colour = year), size = 3) +
  geom_text(aes(label = year), nudge_y = 0.005, size = 2.8) +
  scale_colour_viridis_c(option = "C", end = 0.95, name = "Year") +
  labs(x = "SPEI-4 annual mean", y = "NDVI annual mean",
       title = "NDVI vs SPEI national scatter") +
  common_theme

p_ndvi <- plot_grid(p_a, p_b, ncol = 1, rel_heights = c(1, 1))
ggsave(file.path(FIG, "fig18_ndvi_evi_drought.pdf"), p_ndvi,
       width = 9, height = 7, units = "in")
ggsave(file.path(FIG, "fig18_ndvi_evi_drought.png"), p_ndvi,
       width = 9, height = 7, units = "in", dpi = 150)

cat("DONE — wrote:\n")
for (f in c("fig13_gov_map_spei", "fig14_gov_map_conflict",
            "fig15_gov_map_welfare", "fig16_gov_map_land_share",
            "fig17_district_map_spei", "fig18_ndvi_evi_drought")) {
  cat("  ", file.path(FIG, paste0(f, ".pdf")), "\n")
}
