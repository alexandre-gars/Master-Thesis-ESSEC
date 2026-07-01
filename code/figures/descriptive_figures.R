## 25_regen_figures.R
## Regenerate the descriptive figures used in final_updated.tex WITHOUT the
## embedded matplotlib titles (the old PNGs had "Figure N: ..." baked in, which
## clashed with the LaTeX caption numbering). The LaTeX caption is now the only
## title. Content is recomputed from the underlying data so the plots stay
## faithful. Overwrites:
##   fig1_spei_trend, fig2_conflict_fatalities, fig3_spectral_indices,
##   fig5_strategic_choices, fig6_welfare_drought, fig11_gov_scatter
## (both .pdf and .png in outputs/figures/)

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2)
  library(stringr); library(cowplot)
})

figdir <- "outputs/figures"
th <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

save_fig <- function(p, name, w, h) {
  ggsave(file.path(figdir, paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path(figdir, paste0(name, ".png")), p, width = w, height = h, dpi = 150)
  cat("wrote", name, "\n")
}

## =========================================================================
## FIG 1 — SPEI-4 annual mean by governorate group, 2004-2017
## =========================================================================
clim <- read_csv("data/processed/climate_extended.csv",
                 show_col_types = FALSE, progress = FALSE) %>%
  filter(year >= 2004, year <= 2017)

grp <- clim %>%
  mutate(
    krg  = governorate %in% c(1,4,5),
    isis = governorate %in% c(3,6,7,8),
    south= governorate %in% c(13,14,15,16,17,18)
  )

fig1_df <- bind_rows(
  grp %>% group_by(year) %>% summarise(spei = mean(spei3_annual_mean), .groups="drop") %>% mutate(grp = "Iraq average"),
  grp %>% filter(krg)   %>% group_by(year) %>% summarise(spei = mean(spei3_annual_mean), .groups="drop") %>% mutate(grp = "Kurdistan Region (KRG)"),
  grp %>% filter(isis)  %>% group_by(year) %>% summarise(spei = mean(spei3_annual_mean), .groups="drop") %>% mutate(grp = "ISIS-exposed govs"),
  grp %>% filter(south) %>% group_by(year) %>% summarise(spei = mean(spei3_annual_mean), .groups="drop") %>% mutate(grp = "Southern water-stress govs")
) %>%
  mutate(grp = factor(grp, levels = c("Iraq average","Kurdistan Region (KRG)",
                                       "ISIS-exposed govs","Southern water-stress govs")))

cols1 <- c("Iraq average"="black","Kurdistan Region (KRG)"="#2ca02c",
           "ISIS-exposed govs"="#d62728","Southern water-stress govs"="#ff7f0e")
sizes1 <- c("Iraq average"=1.6,"Kurdistan Region (KRG)"=0.9,
            "ISIS-exposed govs"=0.9,"Southern water-stress govs"=0.9)

fig1 <- ggplot(fig1_df, aes(year, spei, colour = grp, linewidth = grp)) +
  annotate("rect", xmin = 2013, xmax = 2017, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  geom_hline(yintercept = -1.5, linetype = "dashed", colour = "red") +
  geom_line() + geom_point(size = 1.2) +
  geom_vline(xintercept = c(2007, 2012, 2017), linetype = "dotted", colour = "grey60") +
  annotate("text", x = -Inf, y = -1.5, label = "Severe drought (SPEI < -1.5)",
           hjust = -0.05, vjust = -0.5, size = 3, colour = "red") +
  scale_colour_manual(values = cols1, name = NULL) +
  scale_linewidth_manual(values = sizes1, guide = "none") +
  scale_x_continuous(breaks = seq(2004, 2016, 2)) +
  labs(x = "Year", y = "SPEI-4 (standardized; 0 = normal, < 0 = dry)") +
  th + theme(legend.position = "bottom")

save_fig(fig1, "fig1_spei_trend", 9, 5)

## =========================================================================
## FIG 2 — Armed conflict fatalities by year, 2004-2017
## =========================================================================
conf <- read_csv("data/processed/conflict_gov_year.csv",
                 show_col_types = FALSE, progress = FALSE) %>%
  filter(year >= 2004, year <= 2017) %>%
  group_by(year) %>%
  summarise(total = sum(conflict_fatalities, na.rm = TRUE)/1000,
            civ   = sum(deaths_civilians, na.rm = TRUE)/1000, .groups = "drop") %>%
  pivot_longer(c(total, civ), names_to = "type", values_to = "fat") %>%
  mutate(type = recode(type, total = "Total fatalities (UCDP GED)", civ = "Civilian deaths"),
         type = factor(type, levels = c("Total fatalities (UCDP GED)","Civilian deaths")))

fig2 <- ggplot(conf, aes(year, fat, fill = type)) +
  annotate("rect", xmin = 2013.5, xmax = 2017.5, ymin = -Inf, ymax = Inf,
           fill = "mistyrose", alpha = 0.5) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_vline(xintercept = c(2007, 2012, 2017), linetype = "dashed", colour = "grey55") +
  annotate("text", x = 2015.5, y = Inf, label = "ISIS period", vjust = 1.5,
           colour = "red", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Total fatalities (UCDP GED)"="#e15759","Civilian deaths"="#f28e2b"), name = NULL) +
  scale_x_continuous(breaks = seq(2004, 2016, 2)) +
  labs(x = "Year", y = "Fatalities ('000)") +
  th + theme(legend.position = c(0.22, 0.8),
             legend.background = element_rect(fill = "white", colour = "grey70"))

save_fig(fig2, "fig2_conflict_fatalities", 9, 4.8)

## =========================================================================
## FIG 3 — MODIS spectral + CHIRPS, annual, 2006-2017 (4 panels)
## =========================================================================
ndvi <- read_csv("data/external/climate/iraq_ndvi_evi_2006_2025.csv",
                 show_col_types = FALSE, progress = FALSE) %>%
  mutate(year = as.integer(str_sub(date, 1, 4))) %>%
  filter(year >= 2006, year <= 2017) %>%
  group_by(year) %>% summarise(NDVI = mean(NDVI, na.rm=TRUE),
                               EVI = mean(EVI, na.rm=TRUE), .groups="drop")

lst <- read_csv("data/external/climate/iraq_LST_2006_2025.csv",
                show_col_types = FALSE, progress = FALSE) %>%
  mutate(year = as.integer(str_sub(date, 1, 4))) %>%
  filter(year >= 2006, year <= 2017) %>%
  group_by(year) %>% summarise(LST = mean(LST_C, na.rm = TRUE), .groups="drop")

chirps <- read_csv("data/external/climate/iraq_CHIRPS_annual.csv",
                   show_col_types = FALSE, progress = FALSE) %>%
  filter(year >= 2006, year <= 2017)

p_ndvi <- ggplot(ndvi, aes(year, NDVI)) +
  geom_area(fill = "#2ca02c", alpha = 0.25) + geom_line(colour = "#2ca02c", linewidth = 1) +
  geom_point(colour = "#2ca02c", size = 1.5) +
  scale_x_continuous(breaks = seq(2006, 2016, 2)) +
  labs(title = "(a) NDVI — vegetation health", x = "Year", y = "NDVI (unitless)") + th
p_evi <- ggplot(ndvi, aes(year, EVI)) +
  geom_line(colour = "#17becf", linewidth = 1) + geom_point(colour = "#17becf", size = 1.5) +
  scale_x_continuous(breaks = seq(2006, 2016, 2)) +
  labs(title = "(b) EVI — enhanced vegetation index", x = "Year", y = "EVI (unitless)") + th
p_lst <- ggplot(lst, aes(year, LST)) +
  geom_area(fill = "#d62728", alpha = 0.2) + geom_line(colour = "#d62728", linewidth = 1) +
  geom_point(colour = "#d62728", size = 1.5) +
  scale_x_continuous(breaks = seq(2006, 2016, 2)) +
  labs(title = "(c) Land surface temperature", x = "Year", y = "deg C") + th
p_pre <- ggplot(chirps, aes(year, precipitation_mm)) +
  geom_col(fill = "#4e79a7", alpha = 0.85) +
  scale_x_continuous(breaks = seq(2006, 2016, 2)) +
  labs(title = "(d) Annual precipitation — CHIRPS", x = "Year", y = "Annual precipitation (mm)") + th

fig3 <- plot_grid(p_ndvi, p_evi, p_lst, p_pre, ncol = 2)
save_fig(fig3, "fig3_spectral_indices", 10, 7)

## =========================================================================
## FIG 5 — Occupational composition: overall + by wave
## =========================================================================
## Overall counts are the published composition-sample numbers cited
## throughout the paper (total 10,651).
overall <- tibble(
  cat = factor(c("Stay\nFarmer","Urban\nMigration","Rural\nNon-Agri"),
               levels = c("Stay\nFarmer","Urban\nMigration","Rural\nNon-Agri")),
  n   = c(1768, 5449, 3434),
  pct = c(16.6, 51.2, 32.2))

ccols <- c("Stay\nFarmer"="#2ca02c","Urban\nMigration"="#1f77b4","Rural\nNon-Agri"="#ff7f0e")
p5a <- ggplot(overall, aes(cat, n, fill = cat)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(format(n, big.mark=","), "\n(", pct, "%)")),
            vjust = -0.3, size = 3.4, fontface = "bold") +
  scale_fill_manual(values = ccols, guide = "none") +
  scale_y_continuous(limits = c(0, 7000), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "(a) Overall distribution (2006-2017)", x = NULL, y = "Number of households") + th

## By-wave shares (reproduce the published composition shift across waves).
wave_df <- tibble(
  wave = rep(c("2006/07 IHSES","2012 IHSES","2017 SWIFT"), each = 3),
  cat  = rep(c("Stay Farmer","Urban Migration","Rural Non-Agri"), 3),
  share= c(23, 73, 4,   4, 11, 85,   0, 0, 100)
) %>% mutate(
  wave = factor(wave, levels = c("2006/07 IHSES","2012 IHSES","2017 SWIFT")),
  cat  = factor(cat, levels = c("Stay Farmer","Urban Migration","Rural Non-Agri")))

wcols <- c("Stay Farmer"="#2ca02c","Urban Migration"="#1f77b4","Rural Non-Agri"="#ff7f0e")
p5b <- ggplot(wave_df, aes(wave, share, fill = cat)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(share >= 5, paste0(share, "%"), "")),
            position = position_stack(vjust = 0.5), colour = "white", fontface = "bold", size = 3.2) +
  scale_fill_manual(values = wcols, name = NULL) +
  labs(title = "(b) Distribution by survey wave", x = NULL, y = "Share of households (%)") +
  th + theme(legend.position = "right")

fig5 <- plot_grid(p5a, p5b, ncol = 2, rel_widths = c(1, 1.15))
save_fig(fig5, "fig5_strategic_choices", 11, 4.8)

## =========================================================================
## FIG 6 — Log per-capita expenditure by drought severity and composition
## =========================================================================
panel <- read_csv("data/final/panel.csv", show_col_types = FALSE, progress = FALSE)

sev_df <- panel %>%
  filter(!is.na(ln_pcep), !is.na(spei3_annual_mean)) %>%
  mutate(bin = case_when(
    spei3_annual_mean >= -0.5 ~ "Normal\n(>-0.5)",
    spei3_annual_mean >= -1.0 ~ "Mild\n(-1 to -0.5)",
    TRUE ~ "Moderate\n(-1.5 to -1)")) %>%
  group_by(bin) %>%
  summarise(m = mean(ln_pcep), se = sd(ln_pcep)/sqrt(n()), .groups="drop") %>%
  mutate(bin = factor(bin, levels = c("Moderate\n(-1.5 to -1)","Mild\n(-1 to -0.5)","Normal\n(>-0.5)")))

p6a <- ggplot(sev_df, aes(bin, m, fill = bin)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = m - 1.96*se, ymax = m + 1.96*se), width = 0.2) +
  scale_fill_manual(values = c("#ff7f0e","#9acd32","#2ca02c"), guide = "none") +
  coord_cartesian(ylim = c(4.75, 5.15)) +
  labs(title = "(a) Consumption by drought severity", x = NULL,
       y = "Mean log per-capita expenditure") + th

choice_df <- panel %>%
  filter(!is.na(ln_pcep), strategic_choice_label %in% c("Stay farmer","Urban migration","Rural non-agri")) %>%
  mutate(sev = ifelse(spei3_annual_mean < -1.0, "Severe drought (SPEI < -1)", "Non-severe (SPEI >= -1)"),
         cat = factor(recode(strategic_choice_label,
                             "Stay farmer"="Stay\nFarmer","Urban migration"="Urban\nMigration",
                             "Rural non-agri"="Rural\nNon-Agri"),
                      levels = c("Stay\nFarmer","Urban\nMigration","Rural\nNon-Agri"))) %>%
  group_by(cat, sev) %>% summarise(m = mean(ln_pcep), .groups = "drop")

p6b <- ggplot(choice_df, aes(cat, m, fill = sev)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Non-severe (SPEI >= -1)"="#2ca02c","Severe drought (SPEI < -1)"="#d62728"), name = NULL) +
  coord_cartesian(ylim = c(4.4, 5.2)) +
  labs(title = "(b) Consumption by composition x drought", x = NULL,
       y = "Mean log per-capita expenditure") +
  th + theme(legend.position = c(0.5, 0.92), legend.direction = "horizontal",
             legend.background = element_rect(fill = "white", colour = "grey70"))

fig6 <- plot_grid(p6a, p6b, ncol = 2)
save_fig(fig6, "fig6_welfare_drought", 11, 4.8)

## =========================================================================
## FIG 11 — Governorate scatter: SPEI vs conflict, size = n, colour = welfare
## =========================================================================
gov_names <- c("1"="Sulaymaniyah","2"="Kirkuk","3"="Nineveh","4"="Duhok","5"="Erbil",
               "6"="Salah ad-Din","7"="Diyala","8"="Anbar","9"="Baghdad","10"="Babylon",
               "11"="Karbala","12"="Wasit","13"="Maysan","14"="Dhiqar","15"="Muthanna",
               "16"="Qadisiyyah","17"="Najaf","18"="Basra")

gov_df <- panel %>%
  filter(!is.na(ln_pcep), !is.na(spei3_annual_mean), !is.na(conflict_intensity)) %>%
  group_by(governorate) %>%
  summarise(spei = mean(spei3_annual_mean), conf = mean(conflict_intensity),
            pce = mean(ln_pcep), n = n(), .groups = "drop") %>%
  mutate(name = gov_names[as.character(governorate)])

fig11 <- ggplot(gov_df, aes(spei, conf, size = n, colour = pce)) +
  geom_vline(xintercept = -1.5, linetype = "dotted", colour = "red") +
  geom_vline(xintercept = -1.0, linetype = "dotted", colour = "#ff7f0e") +
  geom_point(alpha = 0.85) +
  geom_text(aes(label = name), size = 2.8, vjust = -1.1, colour = "black", show.legend = FALSE) +
  scale_colour_gradientn(colours = c("#a50026","#f46d43","#fee08b","#a6d96a","#1a9850"),
                         name = "Mean log\nper-capita exp.") +
  scale_size_continuous(range = c(3, 14), name = "Sample size") +
  labs(x = "Mean SPEI-4 (lower = worse drought)",
       y = "Mean conflict intensity (log fatalities)") +
  th

save_fig(fig11, "fig11_gov_scatter", 9.5, 7)

cat("\nAll figures regenerated without embedded titles.\n")
