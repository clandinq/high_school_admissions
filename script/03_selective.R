#--------------------------------------------------------------------------------------------
# File name:         03_selective.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - here("output", "panel_institutions.rds")
#   - here("output", "panel_rates.rds")
# Files created:
#   - here("output", "rank_selective.csv")
#   - here("output", "panel_selective.rds")
#   - here("output", "selective_institutions_seen.csv")
#   - here("output", "figures", "sel_uw.pdf")
#   - here("output", "figures", "sel_any.pdf")
# Purpose:
#   - Rank schools on enrollment at the UW and at selective institutions, per graduate.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################
source(here("script", "00_helpers.R"))

##############################################
##  (1): Selective-institution enrollment.  ##
##############################################
# (1.1): Load the institution panel (shares of college enrollees) and the any-enrollment rate.
inst <- readRDS(here("output", "panel_institutions.rds")) %>%
  filter(school_type != "excluded", cohort >= 2005, cohort <= 2024) %>%
  left_join(selective_institutions, by = "inst_key")
enr_any <- readRDS(here("output", "panel_rates.rds")) %>%
  filter(outcome == "enr_any", group == "all") %>%
  select(district, school, cohort, enr_rate = rate)

# (1.2): Validation: summed listed shares per school-cohort (comprehensive schools only).
share_sums <- inst %>%
  filter(school_type == "comprehensive") %>%
  group_by(district, school, cohort) %>%
  summarise(share_sum = sum(share, na.rm = TRUE), n_supp = sum(suppressed), .groups = "drop")
print(summary(share_sums$share_sum))
print(quantile(share_sums$share_sum, c(0.01, 0.05, 0.5, 0.95, 0.99)))

# (1.3): Per school-cohort shares of enrollees. UW is unknown (NA) when it is not listed that
#        year (it may sit in "Other 4 Year") or when its cell is suppressed. sel_share is a
#        LOWER BOUND: selective institutions outside the listed top ~8 are invisible, and
#        suppressed cells count as zero. It is set to NA whenever UW is NA.
shares <- inst %>%
  group_by(district, school, school_label, school_type, cohort) %>%
  summarise(uw_listed   = as.integer(any(inst_key == "UW")),
            uw_share    = if (any(inst_key == "UW" & suppressed == 0)) sum(share[inst_key == "UW"], na.rm = TRUE) else NA_real_,
            sel_share   = sum(share[!is.na(in_state)], na.rm = TRUE),
            sel_oos_sh  = sum(share[!is.na(in_state) & in_state == 0], na.rm = TRUE),
            .groups = "drop") %>%
  mutate(sel_share  = if_else(is.na(uw_share), NA_real_, sel_share),
         sel_oos_sh = if_else(is.na(uw_share), NA_real_, sel_oos_sh)) %>%
  left_join(enr_any, by = c("district", "school", "cohort")) %>%
  mutate(sel_uw  = uw_share * enr_rate,
         sel_any = sel_share * enr_rate,
         sel_oos = sel_oos_sh * enr_rate)
print(sprintf("School-cohorts: %d; UW not listed: %d; UW NA (not listed or suppressed): %d; enr_any NA: %d",
              nrow(shares), sum(shares$uw_listed == 0), sum(is.na(shares$uw_share)), sum(is.na(shares$enr_rate))))
print(count(filter(shares, is.na(uw_share), cohort >= 2015), school_type))

# (1.4): Reshape into the long panel format used by rank_outcome().
panel_sel <- shares %>%
  pivot_longer(c(sel_uw, sel_any, sel_oos), names_to = "outcome", values_to = "rate") %>%
  mutate(group       = "all",
         cohort_span = 1L,
         suppressed  = as.integer(is.na(rate))) %>%
  select(district, school, school_label, school_type, outcome, group, cohort, cohort_span,
         rate, suppressed)
write_rds(panel_sel, here("output", "panel_selective.rds"))

##########################################
##  (2): Rank schools, 2015-2024.       ##
##########################################
# (2.1): Rank each outcome and save.
rank_sel <- map_dfr(c("sel_uw", "sel_any", "sel_oos"),
                    ~ rank_outcome(panel_sel, .x, 2015, 2024))
write_csv(rank_sel, here("output", "rank_selective.csv"))

# (2.2): Print the comprehensive-school rankings for the main outcomes.
rank_sel %>%
  filter(outcome %in% c("sel_uw", "sel_any"), school_type == "comprehensive") %>%
  select(outcome, school_label, est, ci_lo, ci_hi, eb, rank_raw, rank_eb, indist_top, n_obs, n_supp) %>%
  arrange(outcome, rank_eb) %>%
  print(n = Inf)

# (2.3): Sanity check: UW per-graduate rates should be roughly 3-20%.
print(summary(filter(rank_sel, outcome == "sel_uw", school_type == "comprehensive")$est))

####################################
##  (3): Institutions observed.   ##
####################################
# (3.1): Selective institutions listed at least once in 2015-2024, by school.
seen <- inst %>%
  filter(school_type %in% c("comprehensive", "choice"), cohort >= 2015, !is.na(inst_label), suppressed == 0) %>%
  group_by(school_label, school_type, inst_label) %>%
  summarise(n_cohorts_listed = n_distinct(cohort), mean_share = mean(share), .groups = "drop") %>%
  arrange(school_label, desc(n_cohorts_listed))
write_csv(seen, here("output", "selective_institutions_seen.csv"))

# (3.2): How often does any non-UW selective institution appear? (rare => top-N truncation)
non_uw <- seen %>% filter(inst_label != "University of Washington (todos los campus)")
print(sprintf("Non-UW selective school-institution pairs: %d; school-cohort rows: %d of %d",
              nrow(non_uw), sum(non_uw$n_cohorts_listed), nrow(filter(shares, school_type %in% c("comprehensive", "choice"), cohort >= 2015))))
print(non_uw, n = Inf)

######################
##  (4): Figures.   ##
######################
# (4.1): UW enrollment per graduate.
p_uw <- plot_dotci(filter(rank_sel, outcome == "sel_uw"),
                   title = "Inscripción en la Universidad de Washington (% de graduados)",
                   x_lab = "% de graduados inscritos en la UW (primer año)", groups = "all")
ggsave(here("output", "figures", "sel_uw.pdf"), p_uw, width = 9, height = 5.5, device = cairo_pdf)

# (4.2): Selective institutions per graduate (lower bound).
p_any <- plot_dotci(filter(rank_sel, outcome == "sel_any"),
                    title = "Inscripción en universidades selectivas (% de graduados, cota inferior)",
                    x_lab = "% de graduados inscritos en universidades selectivas (primer año)", groups = "all")
ggsave(here("output", "figures", "sel_any.pdf"), p_any, width = 9, height = 5.5, device = cairo_pdf)
