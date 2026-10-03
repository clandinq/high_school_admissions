#--------------------------------------------------------------------------------------------
# File name:         05_completion.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - here("output", "panel_rates.rds")
# Files created:
#   - here("output", "rank_completion.csv")
#   - here("output", "rank_completion_sensitivity.csv")
#   - here("output", "figures", "comp_ba6.pdf")
#   - here("output", "figures", "comp_any6.pdf")
#   - here("output", "figures", "comp_aa2.pdf")
# Purpose:
#   - Rank high schools on postsecondary completion (bachelor's, any credential, early associate).
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################
source(here("script", "00_helpers.R"))

######################################
##  (1): Rank completion outcomes.  ##
######################################
# (1.1): Load the panel.
panel <- read_rds(here("output", "panel_rates.rds"))

# (1.2): Main rankings. Year-6 outcomes use cohorts 2009-2018; the year-2 associate outcome has
#        its own recent 10-cohort window (2013-2022) because those data run to 2022.
rank_ba6 <- rank_outcome(panel, "comp_ba6", 2009, 2018)
rank_any6 <- rank_outcome(panel, "comp_any6", 2009, 2018)
rank_aa2 <- rank_outcome(panel, "comp_aa2", 2013, 2022)
rank_comp <- bind_rows(rank_ba6, rank_any6, rank_aa2)
write_csv(rank_comp, here("output", "rank_completion.csv"))

# (1.3): Sensitivity: comp_ba6 over every available cohort (2005-2018), compared with the main window.
rank_ba6_all <- rank_outcome(panel, "comp_ba6", 2005, 2018)
sens <- rank_ba6 %>%
  filter(school_type == "comprehensive") %>%
  select(group, school_label, est_main = est, rank_eb_main = rank_eb) %>%
  left_join(rank_ba6_all %>% select(group, school_label, est_all = est, rank_eb_all = rank_eb),
            by = c("group", "school_label"))
write_csv(sens, here("output", "rank_completion_sensitivity.csv"))
print(sens %>%
        group_by(group) %>%
        summarise(spearman_rank_eb = cor(rank_eb_main, rank_eb_all, method = "spearman", use = "complete.obs"),
                  n_schools = sum(!is.na(rank_eb_main) & !is.na(rank_eb_all))))

###############################
##  (2): Validation checks.  ##
###############################
# (2.1): Bachelor's + no degree should not exceed 1 (the remainder is associate / apprenticeship).
#        Single-cohort series only; 0.02 allows for rounding.
sum_chk <- panel %>%
  filter(outcome %in% c("comp_ba6", "comp_none6"), cohort_span == 1, !is.na(rate)) %>%
  select(outcome, district, school, cohort, group, rate) %>%
  pivot_wider(names_from = outcome, values_from = rate) %>%
  filter(!is.na(comp_ba6), !is.na(comp_none6)) %>%
  mutate(total = comp_ba6 + comp_none6)
print(sprintf("comp_ba6 + comp_none6 > 1.02: %d of %d", sum(sum_chk$total > 1.02), nrow(sum_chk)))

# (2.2): Plausibility of bachelor's levels (school-level pooled rates, comprehensive schools).
print(rank_ba6 %>%
        filter(school_type == "comprehensive") %>%
        group_by(group) %>%
        summarise(min_est = min(est, na.rm = TRUE), mean_est = mean(est, na.rm = TRUE), max_est = max(est, na.rm = TRUE)))

# (2.3): Bachelor's ranking tables for the report.
print(rank_ba6 %>%
        filter(school_type == "comprehensive", group %in% c("gpa_hi", "all")) %>%
        select(group, school_label, est, ci_lo, ci_hi, eb, rank_eb, indist_top, cohorts, n_obs) %>%
        arrange(group, rank_eb), n = 40)
print(rank_aa2 %>%
        filter(school_type == "comprehensive", group == "all") %>%
        select(school_label, est, ci_lo, ci_hi, eb, rank_eb, indist_top, cohorts, n_obs), n = 20)

##########################
##  (3): Save figures.  ##
##########################
# (3.1): Dot-and-CI plots, one per outcome.
p_ba6 <- plot_dotci(rank_ba6, "Licenciatura (bachelor's) en 6 años tras la secundaria (cohortes 2009-2018)",
                    "Porcentaje de egresados con licenciatura o más a los 6 años")
ggsave(here("output", "figures", "comp_ba6.pdf"), p_ba6, width = 9, height = 5.5, device = cairo_pdf)

p_any6 <- plot_dotci(rank_any6, "Cualquier título postsecundario en 6 años (cohortes 2009-2018)",
                     "Porcentaje de egresados con algún título a los 6 años")
ggsave(here("output", "figures", "comp_any6.pdf"), p_any6, width = 9, height = 5.5, device = cairo_pdf)

p_aa2 <- plot_dotci(rank_aa2, "Título asociado/certificado a los 2 años (indicador de Running Start, cohortes 2013-2022)",
                    "Porcentaje de egresados con título asociado/certificado a los 2 años")
ggsave(here("output", "figures", "comp_aa2.pdf"), p_aa2, width = 9, height = 5.5, device = cairo_pdf)
