#--------------------------------------------------------------------------------------------
# File name:         09_coursework_sat.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - here("data", "ospi", "report_card_sqss_dual_credit_2024-25.csv")
#   - here("data", "ospi", "report_card_sqss_dual_credit_2023-24.csv")
#   - here("data", "crdc", "crdc_2023_24_{ap, calculus, physics, computer_science, ib, dual_enrollment, enrollment}_wa.csv")
#   - here("output", "public_profiles", "extraction_sat.csv")   (hand transcriptions; see private_extraction_protocol.md)
#   - here("output", "private", "extraction_*.csv")
#   - here("output", "private", "qualitative_*.csv")
#   - here("output", "rank_all_outcomes.csv")
# Files created:
#   - here("output", "coursework_public.csv")
#   - here("output", "coursework_private.csv")
#   - here("output", "sat_table.csv")
#   - here("output", "figures", "coursework_vs_enr4yr.pdf")
#   - here("output", "figures", "coursework_vs_enr4yr.rds")
# Purpose:
#   - Descriptive (unranked) advanced-coursework and SAT figures for the 15 public and 3 private schools.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse, scales)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################
source(here("script", "00_helpers.R"))

comprehensive <- school_meta %>% filter(school_type == "comprehensive")

################################################################
##  (1): Dual-credit participation (OSPI Report Card, SQSS).  ##
################################################################
# (1.1): All students, all grades (9-12), school rows for the 15 schools plus the state row. Rates are
#        shares of enrolled students who completed at least one course of each type in the school
#        year; a student can count in several types, so the type rates do not add up to the total.
ospi_raw <- bind_rows(
  read_csv(here("data", "ospi", "report_card_sqss_dual_credit_2024-25.csv"), col_types = cols(.default = "c")),
  read_csv(here("data", "ospi", "report_card_sqss_dual_credit_2023-24.csv"), col_types = cols(.default = "c"))
)
ospi <- ospi_raw %>%
  filter(studentgroup == "All Students", gradelevel == "All Grades",
         organizationlevel == "State" | (organizationlevel == "School" & schoolname %in% comprehensive$school)) %>%
  transmute(school_year = schoolyear,
            school      = if_else(organizationlevel == "State", "State Total", schoolname),
            n_students  = as.integer(denominator),
            dc_any      = as.numeric(percent),
            dc_ap       = as.numeric(apcoursepercent),
            dc_ib       = as.numeric(ibcoursepercent),
            dc_cihs     = as.numeric(cihscoursepercent),
            dc_rs       = as.numeric(runningstartcoursepercent),
            dc_cte      = as.numeric(ctecoursepercent),
            dat_reason)

# (1.2): Checks: one row per school x year, all 15 schools in both years, nothing suppressed, rates in [0, 1].
stopifnot(!any(duplicated(ospi[c("school_year", "school")])),
          all(table(ospi$school[ospi$school != "State Total"]) == 2),
          n_distinct(ospi$school) == 16,
          all(ospi$dat_reason == "None"),
          all(between(as.matrix(select(ospi, starts_with("dc_"))), 0, 1)))

# (1.3): Latest year is the headline; 2023-24 is kept to show year-to-year stability.
ospi_wide <- ospi %>%
  mutate(yr = if_else(school_year == "2024-25", "", "_prev")) %>%
  select(-school_year, -dat_reason) %>%
  pivot_wider(names_from = yr, values_from = c(n_students, starts_with("dc_")), names_glue = "{.value}{yr}")
print(ospi_wide %>% select(school, dc_any, dc_ap, dc_ib, dc_cihs, dc_rs, dc_ap_prev, dc_ib_prev), n = 20)
print(paste0("Spearman AP+IB 2024-25 vs 2023-24 (15 schools): ",
             with(filter(ospi_wide, school != "State Total"), round(cor(dc_ap + dc_ib, dc_ap_prev + dc_ib_prev, method = "spearman"), 2))))

###########################################################
##  (2): Course offerings (Civil Rights Data Collection).  ##
###########################################################
# (2.1): CRDC 2023-24 school files, the six districts only. Codes: "Yes"/"No" indicators; -9 = not
#        applicable (e.g. no AP courses, so the AP-subject questions are skipped) and is read as 0;
#        the _X (non-binary) count columns are always -10 (not reported), so totals are M + F.
district_leaids <- c("5300300", "5303960", "5302820", "5308760", "5300001", "5308610")
read_crdc <- function(topic) {
  read_csv(here("data", "crdc", paste0("crdc_2023_24_", topic, "_wa.csv")), col_types = cols(.default = "c")) %>%
    filter(LEAID %in% district_leaids) %>%
    select(-c(LEA_STATE, LEA_STATE_NAME, LEAID, LEA_NAME, SCHID, SCH_JUST))
}
crdc_raw <- c("ap", "calculus", "physics", "computer_science", "ib", "dual_enrollment", "enrollment") %>%
  map(read_crdc) %>%
  reduce(left_join, by = c("COMBOKEY", "SCH_NAME")) %>%
  filter(SCH_NAME %in% toupper(comprehensive$school))
stopifnot(nrow(crdc_raw) == 15, !any(duplicated(crdc_raw$SCH_NAME)))

# (2.2): Counts and 0/1 indicators. "Offered" = at least one class (calculus, physics, CS) or the
#        school's own Yes (AP subject, IB, dual enrollment).
yes01 <- function(x) as.integer(x == "Yes")
mf    <- function(m, f) as.integer(m) + as.integer(f)
crdc <- crdc_raw %>%
  transmute(school         = comprehensive$school[match(SCH_NAME, toupper(comprehensive$school))],
            nces_id        = COMBOKEY,
            enrollment     = mf(TOT_ENR_M, TOT_ENR_F),
            ap_courses     = if_else(SCH_APENR_IND == "Yes", as.integer(SCH_APCOURSES), 0L),
            ap_students    = if_else(SCH_APENR_IND == "Yes", mf(TOT_APENR_M, TOT_APENR_F), 0L),
            ap_math        = yes01(SCH_APMATHENR_IND),
            ap_science     = yes01(SCH_APSCIENR_IND),
            ap_cs          = yes01(SCH_APCOMPENR_IND),
            ib             = yes01(SCH_IBENR_IND),
            dual_enrollment = yes01(SCH_DUAL_IND),
            calc_classes   = pmax(as.integer(SCH_MATHCLASSES_CALC), 0L),
            phys_classes   = pmax(as.integer(SCH_SCICLASSES_PHYS), 0L),
            cs_classes     = pmax(as.integer(SCH_COMPCLASSES_CSCI), 0L),
            calculus       = as.integer(calc_classes > 0),
            physics        = as.integer(phys_classes > 0),
            comp_sci       = as.integer(cs_classes > 0 | ap_cs == 1L))
stopifnot(all(!is.na(crdc$school)),
          all(map_lgl(select(crdc, ap_math:dual_enrollment, calculus:comp_sci), is.integer)),
          all(as.matrix(select(crdc, ap_math:dual_enrollment, calculus:comp_sci)) %in% 0:1))
print(crdc, n = 15, width = 200)

# (2.3): Cross-source check: CRDC 2023-24 AP students vs OSPI 2023-24 AP course completers should be
#        of the same order (different definitions: enrolled vs completed; same school year).
xcheck <- crdc %>%
  select(school, ap_students) %>%
  left_join(ospi %>% filter(school_year == "2023-24") %>%
              transmute(school, ospi_ap = round(dc_ap * n_students)), by = "school") %>%
  mutate(ratio = ap_students / ospi_ap)
print(xcheck, n = 15)

# (2.4): Public table: one row per school.
coursework_public <- comprehensive %>%
  distinct(school, school_label) %>%
  left_join(ospi_wide, by = "school") %>%
  left_join(crdc, by = "school") %>%
  left_join(read_csv(here("output", "school_classification.csv")) %>% distinct(school, district), by = "school") %>%
  relocate(district, .after = school_label) %>%
  # Substantive IB program: at least 5 % of students completed an IB course (CRDC flags Kentwood for 1 student).
  mutate(ib_program = as.integer(dc_ib >= 0.05))
stopifnot(nrow(coursework_public) == 15, !anyNA(coursework_public$dc_any), !anyNA(coursework_public$ap_courses))
state_dc <- ospi_wide %>% filter(school == "State Total")
write_csv(bind_rows(coursework_public, state_dc %>% mutate(school_label = "Estado de Washington")),
          here("output", "coursework_public.csv"))

##################################################
##  (3): SAT and AP exams (OSPI / College Board).  ##
##################################################
# (3.1): Hand transcriptions of the OSPI "Summary by school" files (SAT 2023, SAT 2024, AP 2023, AP 2024)
#        and the 2016 file (old SAT scale; kept in the transcription but not used here).
cb <- read_csv(here("output", "public_profiles", "extraction_college_board.csv"), col_types = cols(.default = "c")) %>%
  filter(cohort %in% c("2023", "2024")) %>%
  mutate(school = if_else(school_name == "Enumclaw High School", "Enumclaw Sr High School", school_name),
         metric = recode(metric_raw, "SAT test-takers" = "sat_n", "Mean SAT total" = "sat_mean",
                         "Mean SAT ERW" = "sat_erw", "Mean SAT Math" = "sat_math", "AP exam takers" = "ap_takers",
                         "AP exams taken" = "ap_exams", "AP exams scored 3, 4 or 5" = "ap_exams_3plus"),
         value = as.numeric(numerator)) %>%
  select(school, cohort, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)
stopifnot(all(cb$school %in% comprehensive$school), nrow(cb) == 30)

# (3.2): Who takes the SAT. OSPI does not print a participation rate, so test-takers are set against
#        grade-12 enrollment in the same school year (OSPI SQSS 2023-24 denominator; there is no 2022-23
#        file, so 2023 has no ratio). The ratio exceeds 1 at several Auburn / Federal Way schools, so the
#        count must include juniors (school-day SAT); it is therefore only used to split schools into
#        broad testing (ratio >= 0.6; observed values are >= 0.88) and self-selected testing (observed
#        <= 0.23), never shown as a rate.
grade12 <- ospi_raw %>%
  filter(organizationlevel == "School", studentgroup == "All Students", gradelevel == "12",
         schoolname %in% comprehensive$school) %>%
  transmute(school = schoolname, cohort = paste0("20", str_sub(schoolyear, 6, 7)), grade12_n = as.integer(denominator))
sat_table <- cb %>%
  left_join(grade12, by = c("school", "cohort")) %>%
  mutate(sat_ratio_g12 = sat_n / grade12_n,
         sat_broad     = as.integer(sat_ratio_g12 >= 0.6),
         ap_pass_rate  = ap_exams_3plus / ap_exams) %>%
  left_join(comprehensive %>% distinct(school, school_label), by = "school")
stopifnot(sum(is.na(sat_table$grade12_n[sat_table$cohort == "2024"])) == 0,
          all(sat_table$ap_exams_3plus <= sat_table$ap_exams), all(sat_table$ap_takers <= sat_table$ap_exams),
          with(filter(sat_table, cohort == "2024"), all(sat_ratio_g12 >= 0.85 | sat_ratio_g12 <= 0.25)))
print(sat_table %>% filter(cohort == "2024") %>% select(school_label, sat_n, grade12_n, sat_ratio_g12, sat_mean, ap_exams, ap_pass_rate) %>% arrange(desc(sat_mean)), n = 15)
print(sat_table %>% filter(cohort == "2024") %>% group_by(sat_broad) %>%
        summarise(n_schools = n(), min_mean = min(sat_mean), max_mean = max(sat_mean)))
write_csv(sat_table, here("output", "sat_table.csv"))

########################################################
##  (4): Private schools: their own published figures.  ##
########################################################
# (4.1): Coursework and SAT figures as printed by each school (not comparable with OSPI / CRDC).
private_figs <- list.files(here("output", "private"), pattern = "^extraction_", full.names = TRUE) %>%
  map(read_csv, col_types = cols(.default = "c")) %>%
  bind_rows() %>%
  filter(str_detect(metric_raw, regex("SAT|AP|Advanced Placement|Dual Credit", ignore_case = FALSE)),
         measure_kind == "descriptive") %>%
  transmute(school_id, school_name, field = "figure", text = quote, doc_file, cohort, notes)
private_courses <- list.files(here("output", "private"), pattern = "^qualitative_", full.names = TRUE) %>%
  map(read_csv, col_types = cols(.default = "c")) %>%
  bind_rows() %>%
  filter(field == "advanced_courses") %>%
  transmute(school_id, field, text = value, doc_file, cohort = NA_character_, notes = NA_character_)
coursework_private <- bind_rows(private_courses, private_figs) %>%
  group_by(school_id) %>% fill(school_name, .direction = "updown") %>% ungroup() %>%
  arrange(school_id, field)
print(coursework_private %>% select(school_id, field, text), width = 200)
write_csv(coursework_private, here("output", "coursework_private.csv"))

########################
##  (5): Figure.       ##
########################
# (5.1): Share of students taking AP or IB (2024-25) vs 4-year enrollment of all graduates (EB rate,
#        cohorts 2015-2024). AP and IB shares are summed; few students take both, so this is close to
#        (and at most slightly above) the share taking either. Descriptive only: both reflect who attends.
fig_d <- coursework_public %>%
  mutate(ap_ib = dc_ap + dc_ib, prog = if_else(ib_program == 1L, "Programa IB", "Sin programa IB")) %>%
  left_join(read_csv(here("output", "rank_all_outcomes.csv")) %>%
              filter(outcome == "enr_4yr", group == "all") %>% select(school, eb), by = "school")
stopifnot(!anyNA(fig_d$eb))
p <- ggplot(fig_d, aes(x = ap_ib, y = eb)) +
  geom_point(aes(colour = prog), size = 2.6) +
  geom_text(aes(label = school_label), size = 3, vjust = -0.9, colour = "grey25", check_overlap = TRUE) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_colour_manual(values = c("Programa IB" = "#e08214", "Sin programa IB" = "#1b6ca8"), name = NULL) +
  labs(x = "Estudiantes que completaron un curso AP o IB en 2024-25 (% de 9.º a 12.º)",
       y = "Inscripción en universidad de 4 años\n(todos los graduados, tasa ajustada)",
       caption = "Fuentes: OSPI Report Card (doble crédito, 2024-25) y ERDC (cohortes 2015–2024). Descriptivo: ambas cifras reflejan quién asiste a cada escuela.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, colour = "grey40", size = 8))
ggsave(here("output", "figures", "coursework_vs_enr4yr.pdf"), p, width = 8, height = 5.5, device = cairo_pdf)
write_rds(p, here("output", "figures", "coursework_vs_enr4yr.rds"))
print(paste0("Spearman AP+IB share vs EB 4-year enrollment: ", round(cor(fig_d$ap_ib, fig_d$eb, method = "spearman"), 2)))
