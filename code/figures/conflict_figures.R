# =============================================================================
# conflict_figures.R
# -----------------------------------------------------------------------------
# Builds two figures about conflict for the paper. The plots carry no title
# inside the image, because the title is supplied by the LaTeX caption.
#   Figure 19: how conflict deaths are distributed across governorate-years.
#   Figure 20: dryness and conflict over time, split by region group.
#
# Inputs:
#   data/processed/conflict_gov_year_pc.csv  (conflict_int = deaths per 100,000)
#   data/processed/climate_extended.csv      (SPEI, turned into dryness)
# Outputs (both pdf and png) in outputs/figures/:
#   fig19_fatalities_dist, fig20_dryness_conflict_ts
# =============================================================================

# dplyr/readr/tidyr for data, ggplot2 for plotting, cowplot to combine panels
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2); library(cowplot)
})

fig_dir <- "outputs/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# read the conflict table and the climate table
cpc  <- read_csv("data/processed/conflict_gov_year_pc.csv", show_col_types = FALSE, progress = FALSE)
clim <- read_csv("data/processed/climate_extended.csv", show_col_types = FALSE, progress = FALSE)

# a shared minimal look for both figures (clean background, legend at bottom)
base_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(6, 10, 6, 6))

# grp(): map a governorate number to one of three regions used in the paper's
# subsample story. grp_levels fixes their order; grp_cols fixes their colours.
grp <- function(g) ifelse(g %in% c(1,4,5), "Kurdistan (KRG)",
                   ifelse(g %in% c(3,6,7,8), "ISIS-exposed belt", "Rest of Iraq"))
grp_levels <- c("Kurdistan (KRG)", "ISIS-exposed belt", "Rest of Iraq")
grp_cols   <- c("Kurdistan (KRG)"="#1b9e77", "ISIS-exposed belt"="#d95f02", "Rest of Iraq"="#7570b3")

# -----------------------------------------------------------------------------
# Figure 19: distribution of conflict deaths.
# Two side-by-side histograms. Both use a log x-axis because a few
# governorate-years have huge death counts while most have very few.
# -----------------------------------------------------------------------------
d <- cpc %>% mutate(region = factor(grp(governorate), levels = grp_levels))

# panel A: raw fatality counts. We add 1 before taking the log so that the many
# zero-death cells still appear on a log axis (log of 0 is undefined).
pA <- ggplot(d, aes(x = conflict_fatalities + 1)) +
  geom_histogram(bins = 30, fill = "#9ecae1", colour = "white") +
  scale_x_log10(breaks = c(1, 11, 101, 1001, 8001),
                labels = c("0", "10", "100", "1,000", "8,000")) +
  labs(x = "Annual fatalities per governorate (log scale)", y = "Governorate-year count") +
  base_theme

# panel B: the normalised measure (deaths per 100,000). The dashed vertical line
# marks the high-conflict threshold (the top-quartile cut-off of the survey
# years), computed here directly from the data with quantile(..., 0.75).
thr_bin <- quantile(d$conflict_int[d$year %in% c(2007, 2012, 2017)], 0.75, na.rm = TRUE)
pB <- ggplot(d, aes(x = deaths_per_100k + 0.1)) +
  geom_histogram(bins = 30, fill = "#fc9272", colour = "white") +
  scale_x_log10(breaks = c(0.1, 1.1, 10.1, 100.1),
                labels = c("0", "1", "10", "100")) +
  geom_vline(xintercept = thr_bin + 0.1, linetype = "dashed", colour = "grey25") +
  labs(x = "Deaths per 100,000 inhabitants (log scale)",
       y = "Governorate-year count") +
  base_theme

# put the two panels side by side (labelled A and B) and save
fig19 <- plot_grid(pA, pB, ncol = 2, labels = c("A", "B"))
ggsave(file.path(fig_dir, "fig19_fatalities_dist.pdf"), fig19, width = 10, height = 4.2)
ggsave(file.path(fig_dir, "fig19_fatalities_dist.png"), fig19, width = 10, height = 4.2, dpi = 150)
cat("Wrote fig19_fatalities_dist\n")

# -----------------------------------------------------------------------------
# Figure 20: dryness and conflict over time (2004-2017), one line per region.
# We first average each measure by region and year (transmute builds the region
# and the value, group_by/summarise takes the yearly regional mean).
# -----------------------------------------------------------------------------
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

# left panel: dryness over time. The dotted horizontal line marks a severe-dry
# reference level.
pDry <- ggplot(dry_ts, aes(year, val, colour = region)) +
  geom_hline(yintercept = 1.5, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
  scale_colour_manual(values = grp_cols, name = NULL) +
  scale_x_continuous(breaks = seq(2004, 2017, 2)) +
  labs(x = NULL, y = "Dryness (−SPEI-4)") +
  base_theme

# right panel: conflict over time. The shaded rectangle highlights the ISIS
# period (2014-2017).
pConf <- ggplot(conf_ts, aes(year, val, colour = region)) +
  annotate("rect", xmin = 2013.5, xmax = 2017.5, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "red") +
  geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
  scale_colour_manual(values = grp_cols, name = NULL) +
  scale_x_continuous(breaks = seq(2004, 2017, 2)) +
  labs(x = NULL, y = "Conflict (deaths / 100,000)") +
  base_theme

# share one legend under both panels: pull the legend out of pDry, then stack the
# two legend-less panels on top of that single legend, and save
leg <- get_legend(pDry)
fig20 <- plot_grid(
  plot_grid(pDry + theme(legend.position = "none"),
            pConf + theme(legend.position = "none"),
            ncol = 2, labels = c("A", "B")),
  leg, ncol = 1, rel_heights = c(1, 0.08))
ggsave(file.path(fig_dir, "fig20_dryness_conflict_ts.pdf"), fig20, width = 10, height = 4.4)
ggsave(file.path(fig_dir, "fig20_dryness_conflict_ts.png"), fig20, width = 10, height = 4.4, dpi = 150)
cat("Wrote fig20_dryness_conflict_ts\n")
