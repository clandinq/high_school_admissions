#--------------------------------------------------------------------------------------------
# File name:         07_compare.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - here("output", "panel_rates.rds")
#   - here("output", "rank_enrollment.csv")
#   - here("output", "rank_selective.csv")
#   - here("output", "rank_persistence.csv")
#   - here("output", "rank_completion.csv")
# Files created:
#   - here("output", "rank_all_outcomes.csv")
#   - here("output", "rank_shift.csv")
#   - here("output", "rank_shift_spearman.csv")
#   - here("output", "summary_index.csv")
#   - here("output", "coverage_summary.csv")
#   - here("output", "district_consistency.csv")
# Purpose:
#   - Compare high-achiever vs all-student rankings and build an equal-weight standardized summary.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################
source(here("script", "00_helpers.R"))

################################
##  (1): Load the rankings.  ##
################################
# (1.1): Stack the outcome rankings (common columns only).
ranks <- c("rank_enrollment", "rank_selective", "rank_persistence", "rank_completion") %>%
  map(~ read_csv(here("output", paste0(.x, ".csv")))) %>%
  map(~ select(.x, outcome, group, district, school, school_label, school_type, cohorts, cohort_span,
               n_obs, n_supp, n_cohorts, est, se, ci_lo, ci_hi, eb, eb_lo, eb_hi, shrink_b,
               mu_regional, tau, rank_raw, rank_eb, indist_top)) %>%
  bind_rows()
print(count(ranks, outcome, group))
write_csv(ranks, here("output", "rank_all_outcomes.csv"))

###########################################################
##  (2): High-achiever vs all-student rank comparison.  ##
###########################################################
# (2.1): Gap (GPA >= 3.0 minus all students) and rank shift, per outcome and school.
shift_outcomes <- c("enr_4yr", "enr_any", "per_4yr", "comp_ba6", "comp_any6")
rank_shift <- ranks %>%
  filter(school_type == "comprehensive", outcome %in% shift_outcomes, group %in% c("gpa_hi", "all")) %>%
  select(outcome, school_label, district, group, eb, rank_eb) %>%
  pivot_wider(names_from = group, values_from = c(eb, rank_eb)) %>%
  mutate(gap        = eb_gpa_hi - eb_all,
         rank_shift = rank_eb_all - rank_eb_gpa_hi) %>%
  arrange(outcome, rank_eb_gpa_hi)
write_csv(rank_shift, here("output", "rank_shift.csv"))

# (2.2): Spearman correlation of the two rankings and mean absolute rank shift.
rank_spearman <- rank_shift %>%
  filter(!is.na(rank_eb_gpa_hi), !is.na(rank_eb_all)) %>%
  group_by(outcome) %>%
  summarise(n_schools      = n(),
            spearman       = cor(rank_eb_gpa_hi, rank_eb_all, method = "spearman"),
            mean_abs_shift = mean(abs(rank_shift)),
            max_abs_shift  = max(abs(rank_shift)),
            .groups = "drop")
print(rank_spearman)
write_csv(rank_spearman, here("output", "rank_shift_spearman.csv"))

###############################################
##  (3): Equal-weight standardized summary.  ##
###############################################
# (3.1): z-score each outcome's EB rate across the 15 comprehensive schools, then average
#        with equal weights (1/4 each) over 4-year enrollment, 4-year persistence,
#        bachelor's in 6 years and UW enrollment. The high-achiever version uses GPA >= 3.0
#        rates where available; UW enrollment has no GPA split, so it always uses all students.
#        Schools missing a component are averaged over the components they have.
summary_components <- tribble(
  ~outcome,   ~component_es,
  "enr_4yr",  "Inscripción 4 años",
  "per_4yr",  "Persistencia 4 años",
  "comp_ba6", "Licenciatura en 6 años",
  "sel_uw",   "Inscripción UW"
)
summary_index <- bind_rows(
  ranks %>% filter(outcome %in% c("enr_4yr", "per_4yr", "comp_ba6"), group == "gpa_hi") %>% mutate(view = "gpa_hi"),
  ranks %>% filter(outcome == "sel_uw", group == "all") %>% mutate(view = "gpa_hi"),
  ranks %>% filter(outcome %in% summary_components$outcome, group == "all") %>% mutate(view = "all")
) %>%
  filter(school_type == "comprehensive", !is.na(eb)) %>%
  group_by(view, outcome) %>%
  mutate(z = (eb - mean(eb)) / sd(eb)) %>%
  ungroup() %>%
  left_join(summary_components, by = "outcome") %>%
  select(view, school_label, district, component_es, z) %>%
  pivot_wider(names_from = component_es, values_from = z) %>%
  rowwise() %>%
  mutate(n_components = sum(!is.na(c_across(all_of(summary_components$component_es)))),
         z_mean       = mean(c_across(all_of(summary_components$component_es)), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(view) %>%
  mutate(rank_summary = as.integer(rank(-z_mean, ties.method = "min"))) %>%
  ungroup() %>%
  arrange(view, rank_summary)
print(summary_index, n = 40, width = 200)
write_csv(summary_index, here("output", "summary_index.csv"))

##################################
##  (4): Coverage and checks.  ##
##################################
# (4.1): Cohort observations used and suppressed, per school x outcome x group.
coverage <- ranks %>%
  filter(school_type %in% c("comprehensive", "choice")) %>%
  select(outcome, group, school_label, school_type, cohorts, cohort_span, n_obs, n_supp)
print(coverage %>% filter(n_supp > 0) %>% arrange(desc(n_supp)), n = 40)
write_csv(coverage, here("output", "coverage_summary.csv"))

# (4.2): District-wide pooled rates vs the simple mean of that district's comprehensive
#        schools (all students, 2015-2024). Without n these are not expected to match
#        exactly (district rows also include alternative programs), only to be close.
panel <- read_rds(here("output", "panel_rates.rds"))
district_check <- panel %>%
  filter(outcome %in% c("enr_4yr", "enr_any"), group == "all", cohort >= 2015, cohort <= 2024,
         school_type %in% c("district", "comprehensive")) %>%
  group_by(outcome, district, school_type) %>%
  summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = school_type, values_from = rate, names_prefix = "rate_") %>%
  mutate(diff = rate_district - rate_comprehensive)
print(district_check)
write_csv(district_check, here("output", "district_consistency.csv"))

# (4.3): EB estimates must lie between the raw estimate and the regional mean.
eb_check <- ranks %>%
  filter(!is.na(eb), !is.na(est)) %>%
  mutate(inside = eb >= pmin(est, mu_regional) - 1e-9 & eb <= pmax(est, mu_regional) + 1e-9)
print(sprintf("EB estimates between raw and regional mean: %d of %d", sum(eb_check$inside), nrow(eb_check)))
