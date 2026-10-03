#--------------------------------------------------------------------------------------------
# File name:         02_enrollment.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - here("output", "panel_rates.rds")
# Files created:
#   - here("output", "rank_enrollment.csv")
#   - here("output", "rank_enrollment_sensitivity.csv")
#   - here("output", "rank_enrollment_spearman.csv")
#   - here("output", "composition_share_gpa_hi.csv")
#   - here("output", "figures", "enr_4yr.pdf")
#   - here("output", "figures", "enr_any.pdf")
#   - here("output", "figures", "enr_2yr.pdf")
# Purpose:
#   - Rank schools on first-year college enrollment, run cohort-window sensitivity, and estimate GPA composition.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################
source(here("script", "00_helpers.R"))

######################################
##  (1): Main rankings, 2015-2024.  ##
######################################
# (1.1): Load the panel.
panel <- read_rds(here("output", "panel_rates.rds"))

# (1.2): Rank the three enrollment outcomes. 2-year enrollment is not a "higher is better"
#        outcome: it is ranked mechanically (higher_better = TRUE) but labelled neutral.
rank_enr <- map_dfr(c("enr_4yr", "enr_any", "enr_2yr"),
                    ~ rank_outcome(panel, .x, 2015, 2024, higher_better = TRUE)) %>%
  mutate(ranked_direction = if_else(outcome == "enr_2yr", "neutral", "higher_better"))
write_csv(rank_enr, here("output", "rank_enrollment.csv"))

# (1.3): Sanity checks: rates in [0, 1] and 15 comprehensive schools ranked per group.
print(summary(rank_enr[, c("est", "ci_lo", "ci_hi", "eb")]))
print(rank_enr %>% filter(school_type == "comprehensive", !is.na(rank_eb)) %>% count(outcome, group))

###############################################
##  (2): Sensitivity for 4-year enrollment.  ##
###############################################
# (2.1): All cohorts, 2005-2024.
rank_all_cohorts <- rank_outcome(panel, "enr_4yr", 2005, 2024)

# (2.2): Main window excluding the COVID cohorts 2020-2021. The panel is filtered before
#        ranking. Kent/Enumclaw GPA 3-cohort windows ending in 2021 cover 2019-2021, so those
#        windows are dropped as well. select_cohorts() keeps windows ending 2024, 2021, 2018,
#        2015, so only the 2021 window contains COVID cohorts (2020 and 2022 are never selected).
panel_nocovid <- panel %>%
  filter(!(cohort_span == 1 & cohort %in% 2020:2021),
         !(cohort_span == 3 & cohort == 2021))
rank_nocovid <- rank_outcome(panel_nocovid, "enr_4yr", 2015, 2024)

# (2.3): Spearman correlation of rank_eb between the main window and each sensitivity.
rank_main_4yr <- rank_enr %>% filter(outcome == "enr_4yr")
sens_long <- bind_rows(main = rank_main_4yr, all_cohorts = rank_all_cohorts, no_covid = rank_nocovid,
                       .id = "spec") %>%
  filter(school_type == "comprehensive") %>%
  select(spec, group, district, school_label, rank_eb)

sens_wide <- sens_long %>%
  pivot_wider(names_from = spec, values_from = rank_eb, names_prefix = "rank_eb_")

spearman <- sens_wide %>%
  filter(group %in% c("all", "gpa_hi")) %>%
  group_by(group) %>%
  summarise(rho_all_cohorts = cor(rank_eb_main, rank_eb_all_cohorts, method = "spearman", use = "complete.obs"),
            rho_no_covid    = cor(rank_eb_main, rank_eb_no_covid, method = "spearman", use = "complete.obs"),
            n_schools       = n(), .groups = "drop")
print(spearman)

# (2.4): Save the correlations and per-school ranks under each specification.
write_csv(spearman, here("output", "rank_enrollment_spearman.csv"))
write_csv(sens_wide %>% arrange(group, rank_eb_main), here("output", "rank_enrollment_sensitivity.csv"))

#################################################################
##  (3): Composition diagnostic: share of graduates GPA >= 3.  ##
#################################################################
# (3.1): Cohort-level inputs for 2015-2024. For the all-student series (single cohort), build
#        the 3-year trailing mean ending each cohort so it can be matched to the GPA windows of
#        Kent/Enumclaw. Cohorts are completed first so lags never skip a missing year.
comp_in <- panel %>%
  filter(school_type == "comprehensive", outcome %in% c("enr_4yr", "enr_none"), cohort >= 2013, cohort <= 2024)

all_series <- comp_in %>%
  filter(group == "all", cohort_span == 1) %>%
  select(school_label, district, outcome, cohort, all = rate) %>%
  complete(nesting(school_label, district, outcome), cohort = 2013:2024) %>%
  arrange(school_label, outcome, cohort) %>%
  group_by(school_label, outcome) %>%
  mutate(all_trail3 = (all + lag(all, 1) + lag(all, 2)) / 3) %>%
  ungroup()

# (3.2): Match the GPA series (span 1 or 3) to the right all-student series, then back out
#        w = (all - gpa_lo) / (gpa_hi - gpa_lo). Cells with |gpa_hi - gpa_lo| < 0.15 or any
#        missing input are skipped.
comp_cells <- comp_in %>%
  filter(group %in% c("gpa_hi", "gpa_lo"), cohort >= 2015) %>%
  select(school_label, district, outcome, cohort, cohort_span, group, rate) %>%
  pivot_wider(names_from = group, values_from = rate) %>%
  left_join(all_series, by = c("school_label", "district", "outcome", "cohort")) %>%
  mutate(all_use = if_else(cohort_span == 1, all, all_trail3),
         gap     = gpa_hi - gpa_lo,
         w       = (all_use - gpa_lo) / gap) %>%
  filter(!is.na(all_use), !is.na(gpa_hi), !is.na(gpa_lo), abs(gap) >= 0.15)

# (3.3): Median w per school and outcome, then average the enr_4yr and enr_none medians.
composition <- comp_cells %>%
  group_by(school_label, district, outcome) %>%
  summarise(w_med = median(w), n_cells = n(), .groups = "drop") %>%
  group_by(school_label, district) %>%
  summarise(share_gpa_hi = mean(w_med), n_cells = sum(n_cells), .groups = "drop") %>%
  arrange(share_gpa_hi)
print(composition, n = 20)
write_csv(composition, here("output", "composition_share_gpa_hi.csv"))

#####################
##  (4): Figures.  ##
#####################
# (4.1): 4-year enrollment.
p_4yr <- plot_dotci(rank_enr %>% filter(outcome == "enr_4yr"),
                    "Inscripción en universidad de 4 años (primer año tras graduarse)",
                    x_lab = "Porcentaje de egresados inscritos")
ggsave(here("output", "figures", "enr_4yr.pdf"), p_4yr, width = 9, height = 5.5, device = cairo_pdf)

# (4.2): Any enrollment.
p_any <- plot_dotci(rank_enr %>% filter(outcome == "enr_any"),
                    "Inscripción en cualquier institución postsecundaria (primer año tras graduarse)",
                    x_lab = "Porcentaje de egresados inscritos")
ggsave(here("output", "figures", "enr_any.pdf"), p_any, width = 9, height = 5.5, device = cairo_pdf)

# (4.3): 2-year / community and technical college enrollment (neutral, not "better" or "worse").
p_2yr <- plot_dotci(rank_enr %>% filter(outcome == "enr_2yr"),
                    "Inscripción en college de 2 años o técnico (primer año tras graduarse)",
                    x_lab = "Porcentaje de egresados inscritos")
ggsave(here("output", "figures", "enr_2yr.pdf"), p_2yr, width = 9, height = 5.5, device = cairo_pdf)
