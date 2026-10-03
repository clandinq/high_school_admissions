#--------------------------------------------------------------------------------------------
# File name:         04_persistence.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - here("output", "panel_rates.rds")
#   - here("script", "00_helpers.R")
# Files created:
#   - here("output", "rank_persistence.csv")
#   - here("output", "persistence_suppression_sensitivity.csv")
#   - here("output", "persistence_coverage.csv")
#   - here("output", "figures", "per_4yr.pdf")
#   - here("output", "figures", "ret_4yr.pdf")
# Purpose:
#   - Rank schools on second-year persistence/retention and test sensitivity to top-coded suppression.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################
source(here("script", "00_helpers.R"))

#####################################
##  (1): Main persistence rankings. ##
#####################################
# (1.1): Load the panel and set the pooling window.
panel <- readRDS(here("output", "panel_rates.rds"))
first_cohort <- 2014
last_cohort  <- 2023

# (1.2): Rank each persistence outcome and save the stacked results.
rank_per_4yr <- rank_outcome(panel, "per_4yr", first_cohort, last_cohort)
rank_ret_4yr <- rank_outcome(panel, "ret_4yr", first_cohort, last_cohort)
rank_per_2yr <- rank_outcome(panel, "per_2yr", first_cohort, last_cohort)
rank_all <- bind_rows(rank_per_4yr, rank_ret_4yr, rank_per_2yr)
write_csv(rank_all, here("output", "rank_persistence.csv"))

# (1.3): Sanity check on rate levels and the primary ranking.
print(summary(rank_all$est))
print(rank_per_4yr %>%
        filter(group %in% c("gpa_hi", "all"), school_type == "comprehensive") %>%
        select(group, school_label, est, ci_lo, ci_hi, eb, rank_eb, indist_top, n_obs, n_supp) %>%
        arrange(group, rank_eb), n = 40)

####################################################
##  (2): Sensitivity to top-coded suppression.   ##
####################################################
# (2.1): Build panels where suppressed per_4yr cells are filled with the midpoint or the lower bound
#        of the suppression range (suppressed set to 0 in the copies).
panel_mid <- panel %>%
  mutate(rate       = if_else(outcome == "per_4yr" & suppressed == 1L, (red_lo + red_hi) / 2, rate),
         suppressed = if_else(outcome == "per_4yr", 0L, suppressed))
panel_low <- panel %>%
  mutate(rate       = if_else(outcome == "per_4yr" & suppressed == 1L, red_lo, rate),
         suppressed = if_else(outcome == "per_4yr", 0L, suppressed))

# (2.2): Rank under each assumption.
rank_per_4yr_mid <- rank_outcome(panel_mid, "per_4yr", first_cohort, last_cohort)
rank_per_4yr_low <- rank_outcome(panel_low, "per_4yr", first_cohort, last_cohort)

# (2.3): Compare estimates and ranks per school x group.
sens <- rank_per_4yr %>%
  select(group, district, school, school_label, school_type, n_supp, est_main = est, rank_eb_main = rank_eb) %>%
  left_join(rank_per_4yr_mid %>% select(group, school, est_midpoint = est, rank_eb_midpoint = rank_eb),
            by = c("group", "school")) %>%
  left_join(rank_per_4yr_low %>% select(group, school, est_lower = est, rank_eb_lower = rank_eb),
            by = c("group", "school")) %>%
  filter(school_type == "comprehensive") %>%
  arrange(group, rank_eb_main)
write_csv(sens, here("output", "persistence_suppression_sensitivity.csv"))

# (2.4): Spearman rank correlations (EB ranks) between the three versions, by group.
sens %>%
  group_by(group) %>%
  summarise(main_vs_midpoint  = cor(rank_eb_main, rank_eb_midpoint, method = "spearman", use = "complete.obs"),
            main_vs_lower     = cor(rank_eb_main, rank_eb_lower, method = "spearman", use = "complete.obs"),
            midpoint_vs_lower = cor(rank_eb_midpoint, rank_eb_lower, method = "spearman", use = "complete.obs"),
            max_abs_rank_move = max(abs(rank_eb_main - rank_eb_midpoint), na.rm = TRUE),
            max_est_shift     = max(est_midpoint - est_main, na.rm = TRUE)) %>%
  print()

########################################
##  (3): Coverage of per_4yr series.  ##
########################################
# (3.1): Expected cohort observations per series: 10 single cohorts, or 4 non-overlapping 3-cohort
#        windows (ending 2023, 2020, 2017, 2014). The span comes from any outcome of that school x group.
span_lookup <- panel %>%
  filter(school_type == "comprehensive", group %in% c("all", "gpa_hi")) %>%
  group_by(school, group) %>%
  summarise(cohort_span = max(cohort_span), .groups = "drop")
n_windows <- c(`1` = 10L, `3` = 4L)

# (3.2): Count present and suppressed observations in the window; absent = expected - present.
present <- panel %>%
  filter(outcome == "per_4yr", school_type == "comprehensive", group %in% c("all", "gpa_hi")) %>%
  select_cohorts(first_cohort, last_cohort) %>%
  group_by(school, group) %>%
  summarise(n_present = n(), n_suppressed = sum(suppressed), .groups = "drop")
coverage <- school_meta %>%
  filter(school_type == "comprehensive") %>%
  distinct(school, school_label) %>%
  crossing(group = c("all", "gpa_hi")) %>%
  left_join(span_lookup, by = c("school", "group")) %>%
  left_join(present, by = c("school", "group")) %>%
  mutate(cohort_span  = replace_na(cohort_span, 1L),
         n_expected   = n_windows[as.character(cohort_span)],
         n_present    = replace_na(n_present, 0L),
         n_suppressed = replace_na(n_suppressed, 0L),
         n_absent     = n_expected - n_present,
         n_usable     = n_present - n_suppressed) %>%
  arrange(group, school_label)
write_csv(coverage, here("output", "persistence_coverage.csv"))
print(coverage %>% select(group, school_label, cohort_span, n_expected, n_present, n_suppressed, n_absent), n = 40)

#######################
##  (4): Figures.   ##
#######################
# (4.1): Four-year persistence.
p_per <- plot_dotci(rank_per_4yr,
                    title = "Persistencia al segundo año (entre quienes empezaron en una universidad de 4 años)",
                    x_lab = "Porcentaje que sigue inscrito en el segundo año")
ggsave(here("output", "figures", "per_4yr.pdf"), p_per, width = 9, height = 5.5, device = cairo_pdf)

# (4.2): Four-year retention (same institution).
p_ret <- plot_dotci(rank_ret_4yr,
                    title = "Retención al segundo año (misma universidad de 4 años en la que empezaron)",
                    x_lab = "Porcentaje que sigue en la misma institución en el segundo año")
ggsave(here("output", "figures", "ret_4yr.pdf"), p_ret, width = 9, height = 5.5, device = cairo_pdf)
