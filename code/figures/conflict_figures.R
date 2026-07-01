## 29_figures_pc.R
## New figures for the revised paper (no embedded titles -- LaTeX caption only):
##   fig19_fatalities_dist.{pdf,png}      distribution of conflict fatalities
##   fig20_dryness_conflict_ts.{pdf,png}  dryness & per-capita conflict over time
## Author: A. Gars

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2); library(cowplot)
})

fig_dir <- "outputs/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

cpc  <- read_csv("data/processed/conflict_gov_year_pc.csv", show_col_types = FALSE, progress = FALSE)
clim <- read_csv("data/processed/climate_extended.csv", show_col_types = FALSE, progress = FALSE)

base_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(6, 10, 6, 6))

## region groups used in the subsample narrative
grp <- function(g) ifelse(g %in% c(1,4,5), "Kurdistan (KRG)",
                   ifelse(g %in% c(3,6,7,8), "ISIS-exposed belt", "Rest of Iraq"))
grp_levels <- c("Kurdistan (KRG)", "ISIS-exposed belt", "Rest of Iraq")
grp_cols   <- c("Kurdistan (KRG)"="#1b9e77", "ISIS-exposed belt"="#d95f02", "Rest of Iraq"="#7570b3")

## ============================================================
## FIG 19: distribution of conflict fatalities
## ============================================================
d <- cpc %>% mutate(region = factor(grp(governorate), levels = grp_levels))

## panel A: histogram of raw fatalities (log10(1+x) axis to show the skew + zero mass)
pA <- ggplot(d, aes(x = conflict_fatalities + 1)) +
  geom_histogram(bins = 30, fill = "#9ecae1", colour = "white") +
  scale_x_log10(breaks = c(1, 11, 101, 1001, 8001),
                labels = c("0", "10", "100", "1,000", "8,000")) +
  labs(x = "Annual fatalities per governorate (log scale)", y = "Governorate-year count") +
  base_theme

## panel B: distribution of deaths per 100,000 (the normalized measure)
## dashed line = high-conflict threshold (p75 of survey gov-years)
thr_bin <- quantile(d$conflict_int[d$year %in% c(2007, 2012, 2017)], 0.75, na.rm = TRUE)
pB <- ggplot(d, aes(x = deaths_per_100k + 0.1)) +
  geom_histogram(bins = 30, fill = "#fc9272", colour = "white") +
  scale_x_log10(breaks = c(0.1, 1.1, 10.1, 100.1),
                labels = c("0", "1", "10", "100")) +
  geom_vline(xintercept = thr_bin + 0.1, linetype = "dashed", colour = "grey25") +
  labs(x = "Deaths per 100,000 inhabitants (log scale)",
       y = "Governorate-year count") +
  base_theme

fig19 <- plot_grid(pA, pB, ncol = 2, labels = c("A", "B"))
ggsave(file.path(fig_dir, "fig19_fatalities_dist.pdf"), fig19, width = 10, height = 4.2)
ggsave(file.path(fig_dir, "fig19_fatalities_dist.png"), fig19, width = 10, height = 4.2, dpi = 150)
cat("Wrote fig19_fatalities_dist\n")

## ============================================================
## FIG 20: dryness & per-capita conflict over time, by region group
## ============================================================
dry_ts <- clim %>%
  transmute(governorate, year, dryness = -spei3_annual_mean,
            region = factor(grp(governorate), levels = grp_levels)) %>%
  filter(year >= 2004, year <= 2017) %>%
  group_by(region, year) %>% summarise(val = mean(dryness, na.rm = TRUE), .groups = "drop")

conf_ts <- cpc %>%
  transmute(governorate, year, cpc = conflict_int,
            region = factor(grp(governorate), levels = grp_levels)) %>%
  filter(year >= 2004, year <= 2017) %>%
  group_by(region, year) %>% summarise(val = mean(cpc, na.rm = TRUE), .groups = "drop")

pDry <- ggplot(dry_ts, aes(year, val, colour = region)) +
  geom_hline(yintercept = 1.5, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
  scale_colour_manual(values = grp_cols, name = NULL) +
  scale_x_continuous(breaks = seq(2004, 2017, 2)) +
  labs(x = NULL, y = "Dryness (−SPEI-4)") +
  base_theme

pConf <- ggplot(conf_ts, aes(year, val, colour = region)) +
  annotate("rect", xmin = 2013.5, xmax = 2017.5, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "red") +
  geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
  scale_colour_manual(values = grp_cols, name = NULL) +
  scale_x_continuous(breaks = seq(2004, 2017, 2)) +
  labs(x = NULL, y = "Conflict (deaths / 100,000)") +
  base_theme

leg <- get_legend(pDry)
fig20 <- plot_grid(
  plot_grid(pDry + theme(legend.position = "none"),
            pConf + theme(legend.position = "none"),
            ncol = 2, labels = c("A", "B")),
  leg, ncol = 1, rel_heights = c(1, 0.08))
ggsave(file.path(fig_dir, "fig20_dryness_conflict_ts.pdf"), fig20, width = 10, height = 4.4)
ggsave(file.path(fig_dir, "fig20_dryness_conflict_ts.png"), fig20, width = 10, height = 4.4, dpi = 150)
cat("Wrote fig20_dryness_conflict_ts\n")
