#--------------------------------------------------------------------------------------------
# File name:         01_clean.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - here("data", "High_School_Graduate_Outcomes_-_First_Year_Enrollment_20261002.csv")
#   - here("data", "High_School_Graduate_Outcomes_-_Persistence___Retention_20261002.csv")
#   - here("data", "High_School_Graduate_Outcomes_-_Post_Secondary_Completion_20261002.csv")
#   - here("data", "High_School_Graduate_Outcomes_-_Enrollment_by_Institution_20261002.csv")
#   - here("data", "High_School_Graduate_Outcomes_-_Earnings_20261002.csv")
# Files created:
#   - here("output", "panel_rates.rds")
#   - here("output", "panel_female.rds")
#   - here("output", "panel_institutions.rds")
#   - here("output", "earnings.rds")
#   - here("output", "school_classification.csv")
# Purpose:
#   - Filter ERDC outcome files to the Auburn-area districts and build tidy cohort-level panels.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################
source(here("script", "00_helpers.R"))

#########################################
##  (1): Import and filter raw files.  ##
#########################################
# (1.1): Shared cleaning for the three rate files. All columns are read as text: cohort years
#        carry thousands commas ("2,015"), rates are shares in [0, 1], and suppressed cells
#        have a blank Pct plus a percent range in RedactedPct (e.g. "80-100").
read_erdc <- function(file) {
  read_csv(here("data", file), col_types = cols(.default = col_character()), na = character()) %>%
    filter(DistrictType == "School Dist", DistrictTTL %in% c(districts_in_scope, "Statewide"),
           DemographicGroup %in% c("All Students", "GPA", "Gender"))
}
tidy_rates <- function(df) {
  df %>%
    mutate(cohort      = parse_number(CohortYearTTL),
           cohort_span = if_else(cohortType == "3yr", 3L, 1L),
           group       = case_when(DemographicValue == "All"   ~ "all",
                                   DemographicValue == ">=3.0" ~ "gpa_hi",
                                   DemographicValue == "<3.0"  ~ "gpa_lo",
                                   DemographicValue == "Female" ~ "female"),
           suppressed  = as.integer(Pct == ""),
           rate        = if_else(Pct == "", NA_real_, suppressWarnings(as.numeric(Pct))),
           red_lo      = if_else(suppressed == 1L, as.numeric(str_extract(RedactedPct, "^\\d+")) / 100, NA_real_),
           red_hi      = if_else(suppressed == 1L, as.numeric(str_extract(RedactedPct, "\\d+$")) / 100, NA_real_)) %>%
    select(district = DistrictTTL, school = SchoolTTL, cohort, cohort_span, group, outcome,
           rate, suppressed, red_lo, red_hi)
}

# (1.2): First-year enrollment. Keep FallFirstYearFlg == "N" (enrolled any time in the first
#        year); 2025 exists only as fall-only ("Y") and is not comparable, so it drops here.
enroll_raw <- read_erdc("High_School_Graduate_Outcomes_-_First_Year_Enrollment_20261002.csv")
print(count(enroll_raw, FallFirstYearFlg, CohortYearTTL == "2,025"))
enroll <- enroll_raw %>%
  filter(FallFirstYearFlg == "N") %>%
  mutate(outcome = recode(PSEnrollLevel, "4 Year" = "enr_4yr", "2 Year / CTC" = "enr_2yr",
                          "Not Enrolled" = "enr_none")) %>%
  tidy_rates()
print(sprintf("First-year enrollment rows: raw filtered %s -> tidy %s", comma(nrow(enroll_raw)), comma(nrow(enroll))))

# (1.3): Any enrollment = 1 - not enrolled (suppression ranges flip accordingly).
enroll_any <- enroll %>%
  filter(outcome == "enr_none") %>%
  mutate(outcome = "enr_any", rate = 1 - rate, lo = 1 - red_hi, hi = 1 - red_lo,
         red_lo = lo, red_hi = hi) %>%
  select(-lo, -hi)

# (1.4): Persistence (still enrolled anywhere in year 2) and retention (same institution),
#        both as shares of that sector's first-year enrollees.
persist_raw <- read_erdc("High_School_Graduate_Outcomes_-_Persistence___Retention_20261002.csv")
persist <- persist_raw %>%
  mutate(outcome = paste0(if_else(PersistRetain == "Persisted", "per_", "ret_"),
                          if_else(PSEnrollLevel == "4 Year", "4yr", "2yr"))) %>%
  tidy_rates()
print(sprintf("Persistence rows: %s", comma(nrow(persist))))

# (1.5): Completion (highest credential, mutually exclusive categories): bachelor's+ by year 6,
#        any credential by year 6 (= 1 - No Degree), and associate/certificate by year 2
#        (mostly Running Start associate degrees earned during high school).
complete_raw <- read_erdc("High_School_Graduate_Outcomes_-_Post_Secondary_Completion_20261002.csv")
complete <- complete_raw %>%
  mutate(outcome = case_when(DegreeType == "Bachelor's or Higher"  & YearAfterHSGrad == "6" ~ "comp_ba6",
                             DegreeType == "No Degree"             & YearAfterHSGrad == "6" ~ "comp_none6",
                             DegreeType == "Associate/Certificate" & YearAfterHSGrad == "2" ~ "comp_aa2")) %>%
  filter(!is.na(outcome)) %>%
  tidy_rates()
complete_any <- complete %>%
  filter(outcome == "comp_none6") %>%
  mutate(outcome = "comp_any6", rate = 1 - rate, lo = 1 - red_hi, hi = 1 - red_lo,
         red_lo = lo, red_hi = hi) %>%
  select(-lo, -hi)
print(sprintf("Completion rows: raw filtered %s -> kept %s", comma(nrow(complete_raw)), comma(nrow(complete))))

# (1.6): Enrollment by institution (share of first-year enrollees; top ~8 named + "Other").
inst <- read_csv(here("data", "High_School_Graduate_Outcomes_-_Enrollment_by_Institution_20261002.csv"),
                 col_types = cols(.default = col_character()), na = character()) %>%
  filter(DistType == "School Dist", DistrictTTL %in% c(districts_in_scope, "Statewide"),
         FallFirstYearFlg == "N") %>%
  transmute(district = DistrictTTL, school = SchoolTTL, cohort = parse_number(CohortYearTTL),
            institution = PSOrganizationTTL, inst_key = str_to_upper(PSOrganizationTTL),
            level = PSEnrollLevel, suppressed = as.integer(Pct == ""),
            share = if_else(Pct == "", NA_real_, as.numeric(Pct)),
            red_lo = if_else(suppressed == 1L, as.numeric(str_extract(RedactedPct, "^\\d+")) / 100, NA_real_),
            red_hi = if_else(suppressed == 1L, as.numeric(str_extract(RedactedPct, "\\d+$")) / 100, NA_real_))
print(sprintf("Institution rows: %s", comma(nrow(inst))))

# (1.7): Earnings (pooled over cohorts by ERDC; medians by highest credential and year).
#        The state rows in this file are labelled "StatewideComp" rather than "Statewide".
earn <- read_csv(here("data", "High_School_Graduate_Outcomes_-_Earnings_20261002.csv"),
                 col_types = cols(.default = col_character()), na = character()) %>%
  filter(DistrictType == "School Dist", DistrictTTL %in% c(districts_in_scope, "StatewideComp"),
         DemographicGrouping %in% c("All Students", "GPA")) %>%
  transmute(district = if_else(DistrictTTL == "StatewideComp", "Statewide", DistrictTTL), school = SchoolTTL,
            group = case_when(DemographicValue == "All" ~ "all", DemographicValue == ">=3.0" ~ "gpa_hi",
                              DemographicValue == "<3.0" ~ "gpa_lo"),
            credential = HighestAchievement, years_after = parse_number(YearAfterGrad),
            median_earnings = parse_number(MedianEarnings), median_hours = parse_number(MedianHours),
            n_workers = parse_number(NumRecords))
print(sprintf("Earnings rows: %s", comma(nrow(earn))))

####################################################
##  (2): Classify schools and assemble the panel.  ##
####################################################
# (2.1): TAFA's predecessor name (2014-17) is the same program; merge the two series.
#        Then tag every school as comprehensive / choice / excluded / district / state.
classify <- function(df) {
  df %>%
    mutate(school = if_else(school == "Technology Access Foundation Academy", "TAFA at Saghalie", school)) %>%
    left_join(distinct(school_meta, school, school_label, school_type), by = "school") %>%
    mutate(school_type  = case_when(district == "Statewide"   ~ "state",
                                    school == "District Wide" ~ "district",
                                    is.na(school_type)        ~ "excluded",
                                    TRUE                      ~ school_type),
           school_label = case_when(school_type == "state"    ~ "Washington (estatal)",
                                    school_type == "district" ~ paste0("Distrito ", district),
                                    is.na(school_label)       ~ school,
                                    TRUE                      ~ school_label))
}
#        Female graduates (Gender subgroup, other gender values dropped) are kept in a separate
#        panel used only as a benchmark for the all-girls private school; the ranking panel
#        keeps the all-student and GPA groups only.
panel_all    <- bind_rows(enroll, enroll_any, persist, complete, complete_any) %>%
  filter(!is.na(group)) %>%
  classify()
panel        <- panel_all %>% filter(group != "female")
panel_female <- panel_all %>% filter(group == "female")
print(count(panel_female, outcome, cohort_span))
inst  <- classify(inst)
earn  <- classify(earn)

# (2.2): Classification table for the report.
school_class <- panel %>%
  filter(school_type != "state") %>%
  group_by(district, school, school_label, school_type) %>%
  summarise(first_cohort = min(cohort), last_cohort = max(cohort), .groups = "drop") %>%
  arrange(school_type, district, school)
print(school_class, n = 50)

################################
##  (3): Validation checks.  ##
################################
# (3.1): Unique key.
dups <- panel %>% count(outcome, group, district, school, cohort, cohort_span) %>% filter(n > 1)
print(sprintf("Duplicate panel keys: %d", nrow(dups)))
stopifnot(nrow(dups) == 0)

# (3.2): 2-year + 4-year + not enrolled should sum to 1 (2-decimal rounding -> +/- 0.02).
share_sum <- panel %>%
  filter(outcome %in% c("enr_4yr", "enr_2yr", "enr_none")) %>%
  group_by(district, school, cohort, cohort_span, group) %>%
  filter(n() == 3, all(suppressed == 0)) %>%
  summarise(total = sum(rate), .groups = "drop")
print(summary(share_sum$total))
print(sprintf("Share sums outside [0.97, 1.03]: %d of %d", sum(abs(share_sum$total - 1) > 0.03), nrow(share_sum)))

# (3.3): The all-student rate should lie between the two GPA-band rates.
between_chk <- panel %>%
  filter(cohort_span == 1, !is.na(rate)) %>%
  select(outcome, district, school, cohort, group, rate) %>%
  pivot_wider(names_from = group, values_from = rate) %>%
  filter(!is.na(all), !is.na(gpa_hi), !is.na(gpa_lo)) %>%
  mutate(inside = all >= pmin(gpa_hi, gpa_lo) - 0.01 & all <= pmax(gpa_hi, gpa_lo) + 0.01)
print(sprintf("All-student rate within GPA-band rates (+/- 0.01): %d of %d", sum(between_chk$inside), nrow(between_chk)))

# (3.4): Persistence should be >= retention.
pr_chk <- panel %>%
  filter(outcome %in% c("per_4yr", "ret_4yr"), !is.na(rate)) %>%
  select(outcome, district, school, cohort, cohort_span, group, rate) %>%
  pivot_wider(names_from = outcome, values_from = rate) %>%
  filter(!is.na(per_4yr), !is.na(ret_4yr))
print(sprintf("4-year persistence >= retention: %d of %d", sum(pr_chk$per_4yr >= pr_chk$ret_4yr), nrow(pr_chk)))

# (3.5): Coverage of the high-achiever series among comprehensive schools.
coverage <- panel %>%
  filter(school_type == "comprehensive", group == "gpa_hi", outcome %in% c("enr_4yr", "per_4yr", "comp_ba6")) %>%
  group_by(school_label, outcome) %>%
  summarise(span = max(cohort_span), first = min(cohort), last = max(cohort), supp = sum(suppressed), .groups = "drop") %>%
  pivot_wider(names_from = outcome, values_from = c(span, first, last, supp))
print(coverage, width = 200)

#############################
##  (4): Save the panels.  ##
#############################
# (4.1): Save the panels and classification table.
write_rds(panel, here("output", "panel_rates.rds"))
write_rds(panel_female, here("output", "panel_female.rds"))
write_rds(inst, here("output", "panel_institutions.rds"))
write_rds(earn, here("output", "earnings.rds"))
write_csv(school_class, here("output", "school_classification.csv"))
