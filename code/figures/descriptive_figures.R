# =============================================================================#
# Figures produced (both pdf and png in outputs/figures/):
#   fig1_spei_trend          drought (SPEI) over time by region group
#   fig2_conflict_fatalities conflict deaths over time
#   fig3_spectral_indices    satellite vegetation/temperature/rainfall series
#   fig5_strategic_choices   occupational composition, overall and by wave
#   fig6_welfare_drought     consumption by drought severity and composition
#   fig11_gov_scatter        governorate scatter of drought vs conflict
# =============================================================================

# dplyr/readr/tidyr for data, ggplot2 for plots, stringr for string helpers,
# cowplot to arrange several panels into one figure
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2)
  library(stringr); library(cowplot)
})

figdir <- "outputs/figures"

th <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# save_fig(): save one plot as both pdf and png with the given width/height
save_fig <- function(p, name, w, h) {
  ggsave(file.path(figdir, paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path(figdir, paste0(name, ".png")), p, width = w, height = h, dpi = 150)
  cat("wrote", name, "\n")
}

# -----------------------------------------------------------------------------
# Figure 1: average SPEI-4 (drought index) per year, for four governorate groups.
# -----------------------------------------------------------------------------
clim <- read_csv("data/processed/climate_extended.csv",
                 show_col_types = FALSE, progress = FALSE) %>%
  filter(year >= 2004, year <= 2017)

# flag which group each governorate belongs to (a governorate can be in more than
# one line below because "Iraq average" uses all of them)
grp <- clim %>%
  mutate(
    krg  = governorate %in% c(1,4,5),
    isis = governorate %in% c(3,6,7,8),
    south= governorate %in% c(13,14,15,16,17,18)
  )

# build one yearly SPEI series per group and stack them (bind_rows). Each block
# averages SPEI by year over the relevant governorates and tags it with a label.
fig1_df <- bind_rows(
  grp %>% group_by(year) %>% summarise(spei = mean(spei3_annual_mean), .groups="drop") %>% mutate(grp = "Iraq average"),
  grp %>% filter(krg)   %>% group_by(year) %>% summarise(spei = mean(spei3_annual_mean), .groups="drop") %>% mutate(grp = "Kurdistan Region (KRG)"),
  grp %>% filter(isis)  %>% group_by(year) %>% summarise(spei = mean(spei3_annual_mean), .groups="drop") %>% mutate(grp = "ISIS-exposed govs"),
  grp %>% filter(south) %>% group_by(year) %>% summarise(spei = mean(spei3_annual_mean), .groups="drop") %>% mutate(grp = "Southern water-stress govs")
) %>%
  mutate(grp = factor(grp, levels = c("Iraq average","Kurdistan Region (KRG)",
                                       "ISIS-exposed govs","Southern water-stress govs")))

# colours and line widths per group (the Iraq average is drawn thicker/black)
cols1 <- c("Iraq average"="black","Kurdistan Region (KRG)"="#2ca02c",
           "ISIS-exposed govs"="#d62728","Southern water-stress govs"="#ff7f0e")
sizes1 <- c("Iraq average"=1.6,"Kurdistan Region (KRG)"=0.9,
            "ISIS-exposed govs"=0.9,"Southern water-stress govs"=0.9)

# the plot: shaded ISIS window, a dotted line at 0 (normal) and a dashed red line
# at -1.5 (severe drought), the four series, and dotted lines at the survey years
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

# -----------------------------------------------------------------------------
# Figure 2: total and civilian conflict deaths per year (in thousands).
# We sum deaths by year, divide by 1000, then pivot_longer so "total" and "civ"
# become two categories we can plot as side-by-side bars.
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# Figure 3: four satellite-based series (2006-2017), one panel each.
# NDVI and EVI measure vegetation greenness, LST is land surface temperature,
# CHIRPS is rainfall. Each file has a monthly date, so we extract the year from
# the first 4 characters of the date and average by year.
# -----------------------------------------------------------------------------
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

# one small plot per indicator, then combine into a 2x2 grid
p_ndvi <- ggplot(ndvi, aes(year, NDVI)) +
  geom_area(fill = "#2ca02c", alpha = 0.25) + geom_line(colour = "#2ca02c", linewidth = 1) +
  geom_point(colour = "#2ca02c", size = 1.5) +
  scale_x_continuous(breaks = seq(2006, 2016, 2)) +
  labs(title = "(a) NDVI - vegetation health", x = "Year", y = "NDVI (unitless)") + th
p_evi <- ggplot(ndvi, aes(year, EVI)) +
  geom_line(colour = "#17becf", linewidth = 1) + geom_point(colour = "#17becf", size = 1.5) +
  scale_x_continuous(breaks = seq(2006, 2016, 2)) +
  labs(title = "(b) EVI - enhanced vegetation index", x = "Year", y = "EVI (unitless)") + th
p_lst <- ggplot(lst, aes(year, LST)) +
  geom_area(fill = "#d62728", alpha = 0.2) + geom_line(colour = "#d62728", linewidth = 1) +
  geom_point(colour = "#d62728", size = 1.5) +
  scale_x_continuous(breaks = seq(2006, 2016, 2)) +
  labs(title = "(c) Land surface temperature", x = "Year", y = "deg C") + th
p_pre <- ggplot(chirps, aes(year, precipitation_mm)) +
  geom_col(fill = "#4e79a7", alpha = 0.85) +
  scale_x_continuous(breaks = seq(2006, 2016, 2)) +
  labs(title = "(d) Annual precipitation - CHIRPS", x = "Year", y = "Annual precipitation (mm)") + th

fig3 <- plot_grid(p_ndvi, p_evi, p_lst, p_pre, ncol = 2)
save_fig(fig3, "fig3_spectral_indices", 10, 7)

# -----------------------------------------------------------------------------
# Figure 5: occupational composition (stay farmer / urban migration / rural
# non-agri), shown two ways: overall counts (panel a) and shares by wave (b).
#
# Composition sample: agricultural/rural households (occ_agri==1 or rural==1)
# that have a non-missing composition label. This matches the "non-missing
# composition" sample in the paper's descriptive table. Numbers are computed from
# the panel here.
# -----------------------------------------------------------------------------
# map the raw label to a short two-line label used on the plot
comp_lab <- c("Stay farmer" = "Stay\nFarmer",
              "Urban migration" = "Urban\nMigration",
              "Rural non-agri" = "Rural\nNon-Agri")
comp_panel <- read_csv("data/final/panel_household_gov.csv", show_col_types = FALSE, progress = FALSE) %>%
  filter((occ_agri == 1 | rural == 1),
         strategic_choice_label %in% names(comp_lab))

# panel a data: count households per category and turn counts into percentages
overall <- comp_panel %>%
  count(strategic_choice_label, name = "n") %>%
  mutate(cat = factor(comp_lab[strategic_choice_label],
                      levels = c("Stay\nFarmer","Urban\nMigration","Rural\nNon-Agri")),
         pct = round(100 * n / sum(n), 1)) %>%
  arrange(cat) %>%
  select(cat, n, pct)

# panel a plot: bars of counts, labelled with count and percentage
ccols <- c("Stay\nFarmer"="#2ca02c","Urban\nMigration"="#1f77b4","Rural\nNon-Agri"="#ff7f0e")
p5a <- ggplot(overall, aes(cat, n, fill = cat)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(format(n, big.mark=","), "\n(", pct, "%)")),
            vjust = -0.3, size = 3.4, fontface = "bold") +
  scale_fill_manual(values = ccols, guide = "none") +
  scale_y_continuous(limits = c(0, 7000), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "(a) Overall distribution (2006-2017)", x = NULL, y = "Number of households") + th

# panel b data: within each wave, the share of each category (adds to 100% per wave)
wave_lab <- c("1" = "2006/07 IHSES", "2" = "2012 IHSES", "3" = "2017 SWIFT")
wave_df <- comp_panel %>%
  filter(!is.na(wave)) %>%
  count(wave, strategic_choice_label, name = "n") %>%
  group_by(wave) %>%
  mutate(share = round(100 * n / sum(n))) %>%
  ungroup() %>%
  mutate(wave = factor(wave_lab[as.character(wave)],
                       levels = c("2006/07 IHSES","2012 IHSES","2017 SWIFT")),
         cat  = factor(recode(strategic_choice_label,
                              "Stay farmer" = "Stay Farmer",
                              "Urban migration" = "Urban Migration",
                              "Rural non-agri" = "Rural Non-Agri"),
                       levels = c("Stay Farmer","Urban Migration","Rural Non-Agri"))) %>%
  select(wave, cat, share)

# panel b plot: stacked shares by wave, labelling only slices of at least 5%
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

# -----------------------------------------------------------------------------
# Figure 6: consumption (log per-capita expenditure) two ways.
# Panel a: average consumption across three drought-severity bins.
# Panel b: average consumption by composition category, split by whether the year
# was a severe drought.
# -----------------------------------------------------------------------------
panel <- read_csv("data/final/panel_household_gov.csv", show_col_types = FALSE, progress = FALSE)

# bin each household-year by drought severity, then take the mean consumption and
# its standard error (for the error bars) within each bin
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

# average consumption by composition category and severe-vs-not drought
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

# -----------------------------------------------------------------------------
# Figure 11: one dot per governorate, positioned by average drought (x, SPEI) and
# average conflict (y). Dot size = number of households, dot colour = average
# consumption. This shows which governorates are dry, violent and poor at once.
# -----------------------------------------------------------------------------
gov_names <- c("1"="Sulaymaniyah","2"="Kirkuk","3"="Nineveh","4"="Duhok","5"="Erbil",
               "6"="Salah ad-Din","7"="Diyala","8"="Anbar","9"="Baghdad","10"="Babylon",
               "11"="Karbala","12"="Wasit","13"="Maysan","14"="Dhiqar","15"="Muthanna",
               "16"="Qadisiyyah","17"="Najaf","18"="Basra")

# conflict is the population-normalised measure (deaths per 100,000), joined from
# the per-capita conflict file so this figure uses the same conflict variable as
# the rest of the paper.
conf_pc <- read_csv("data/processed/conflict_gov_year_pc.csv",
                    show_col_types = FALSE, progress = FALSE) %>%
  select(governorate, year, conflict_int)

# average SPEI, conflict and consumption within each governorate, and count rows
gov_df <- panel %>%
  left_join(conf_pc, by = c("governorate", "year")) %>%
  filter(!is.na(ln_pcep), !is.na(spei3_annual_mean), !is.na(conflict_int)) %>%
  group_by(governorate) %>%
  summarise(spei = mean(spei3_annual_mean), conf = mean(conflict_int),
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
       y = "Mean conflict (deaths / 100,000)") +
  th

save_fig(fig11, "fig11_gov_scatter", 9.5, 7)

cat("\nAll figures regenerated\n")
