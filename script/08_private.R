#--------------------------------------------------------------------------------------------
# File name:         08_private.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - here("data", "private")                           (folder listing only)
#   - here("output", "private", "extraction_*.csv")     (hand transcriptions; see private_extraction_protocol.md)
#   - here("output", "private", "qualitative_*.csv")
#   - here("output", "rank_all_outcomes.csv")
#   - here("output", "panel_rates.rds")
#   - here("output", "panel_selective.rds")
#   - here("output", "panel_female.rds")
#   - here("output", "summary_index.csv")
# Files created:
#   - here("output", "private_status.csv")
#   - here("output", "private_comparison.csv")
#   - here("output", "private_not_comparable.csv")
#   - here("output", "private_qualitative.csv")
#   - here("output", "private_public_benchmark.csv")
#   - here("output", "figures", "private_vs_public.pdf")
#   - here("output", "figures", "private_vs_public.rds")
# Purpose:
#   - Place documented private-school outcomes next to the public-school distribution (never ranked).
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse, scales)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################
source(here("script", "00_helpers.R"))

##########################################
##  (1): Load documents and extractions.  ##
##########################################
# (1.1): The three private schools and their document folders (matched by name; a missing or
#        empty folder means "no data provided").
private_schools <- tribble(
  ~school_id,   ~school_name,                              ~folder_pattern,
  "bellarmine", "Bellarmine Preparatory School",           "bellarmine",
  "holy_names", "Holy Names Academy",                      "holy",
  "cristo_rey", "Cristo Rey Jesuit Seattle High School",   "cristo"
)
private_dirs <- list.dirs(here("data", "private"), recursive = FALSE)
private_schools <- private_schools %>%
  mutate(folder   = map_chr(folder_pattern, ~ first(private_dirs[str_detect(str_to_lower(basename(private_dirs)), .x)])),
         n_docs   = map_int(folder, ~ if (is.na(.x)) 0L else length(list.files(.x, recursive = TRUE))),
         has_docs = as.integer(n_docs > 0))

# (1.2): Hand transcriptions (all text on read; empty schema when no file exists yet).
extract_cols <- c("school_id", "school_name", "scope", "doc_file", "doc_type", "page", "quote",
                  "metric_raw", "metric_std", "institution", "sector", "value", "numerator",
                  "denominator", "denominator_desc", "cohort", "timing", "measure_kind",
                  "self_reported", "notes")
qual_cols <- c("school_id", "field", "value", "doc_file", "page", "quote")
read_private <- function(pattern, cols) {
  list.files(here("output", "private"), pattern, full.names = TRUE) %>%
    map(~ read_csv(.x, col_types = cols(.default = col_character()), na = c(""))) %>%
    bind_rows(tibble(!!!set_names(rep(list(character()), length(cols)), cols))) %>%
    select(all_of(cols))
}
extraction <- read_private("^extraction_.*\\.csv$", extract_cols) %>%
  mutate(across(c(value, numerator, denominator), as.numeric),
         self_reported = as.integer(self_reported),
         row_id        = row_number())
qualitative <- read_private("^qualitative_.*\\.csv$", qual_cols)

# (1.3): Status per school.
private_status <- private_schools %>%
  left_join(count(extraction, school_id, name = "n_rows"), by = "school_id") %>%
  left_join(count(qualitative, school_id, name = "n_qual"), by = "school_id") %>%
  mutate(across(c(n_rows, n_qual), ~ replace_na(.x, 0L)),
         folder = if_else(is.na(folder), NA_character_, basename(folder))) %>%
  select(school_id, school_name, folder, n_docs, has_docs, n_rows, n_qual)
print(private_status)
print(count(extraction, school_id, doc_type, measure_kind))
write_csv(private_status, here("output", "private_status.csv"))

##################################
##  (2): Validation checks.     ##
##################################
# (2.1): Known schools and controlled vocabularies.
stopifnot(all(extraction$school_id %in% private_schools$school_id),
          all(extraction$scope %in% c("school", "network")),
          all(extraction$measure_kind %in% c("acceptance", "enrollment", "matriculation", "persistence", "completion", "descriptive")),
          all(extraction$metric_std %in% c("enr_4yr", "enr_any", "per_4yr", "per_other", "comp_ba6", "comp_any6",
                                           "class_size", "matric_count", "matric_complete", "other")),
          all(extraction$self_reported %in% c(0L, 1L)),
          all(qualitative$school_id %in% private_schools$school_id))

# (2.2): Shares lie in [0, 1], and agree with numerator / denominator when both are printed.
share_rows <- extraction %>% filter(!is.na(value), metric_std != "matric_complete")
stopifnot(all(share_rows$value >= 0 & share_rows$value <= 1))
ratio_bad <- extraction %>%
  filter(!is.na(value), !is.na(numerator), !is.na(denominator)) %>%
  filter(abs(value - numerator / denominator) > 0.006)
print(sprintf("Rows where value != numerator / denominator: %d", nrow(ratio_bad)))
stopifnot(nrow(ratio_bad) == 0)

# (2.3): Cristo Rey has no graduates before 2028, so it can have no school-level outcome rows.
cristo_school_outcomes <- extraction %>%
  filter(school_id == "cristo_rey", scope == "school",
         measure_kind %in% c("acceptance", "enrollment", "matriculation", "persistence", "completion"))
stopifnot(nrow(cristo_school_outcomes) == 0)

# (2.4): At most one value per school x scope x metric x cohort x timing within a document type
#        (the same figure repeated on several pages is fine; conflicting values are not).
dup_metrics <- extraction %>%
  filter(metric_std %in% c("enr_4yr", "enr_any", "per_4yr", "comp_ba6", "comp_any6", "class_size")) %>%
  group_by(school_id, scope, doc_type, metric_std, cohort, timing) %>%
  summarise(n_values = n_distinct(coalesce(value, numerator)), .groups = "drop") %>%
  filter(n_values > 1)
print(dup_metrics)
stopifnot(nrow(dup_metrics) == 0)

##############################################
##  (3): Build the private-school metrics.  ##
##############################################
# (3.1): Spanish labels and definitions (public side = ERDC definitions in the main analysis).
outcome_info <- tribble(
  ~outcome,    ~outcome_es,                                ~erdc_def_es,                                                                    ~first, ~last,
  "enr_4yr",   "Inscripción en universidad de 4 años",     "ERDC: % de graduados inscritos en una universidad de 4 años en algún momento del primer año", 2015, 2024,
  "enr_any",   "Inscripción en cualquier institución",     "ERDC: % de graduados inscritos en cualquier institución en algún momento del primer año",    2015, 2024,
  "sel_uw",    "Inscripción en UW (todos los campus)",     "ERDC: % de graduados inscritos en UW (Seattle, Bothell o Tacoma) en el primer año",          2015, 2024,
  "per_4yr",   "Persistencia al 2.º año (4 años)",         "ERDC: % de quienes empezaron en una universidad de 4 años que siguen inscritos en el 2.º año", 2014, 2023,
  "comp_ba6",  "Licenciatura en 6 años",                   "ERDC: % de graduados con licenciatura o más a los 6 años",                                    2009, 2018,
  "comp_any6", "Cualquier título en 6 años",               "ERDC: % de graduados con algún título postsecundario a los 6 años",                           2009, 2018
)
source_label <- function(doc_file, page) paste0(doc_file, if_else(is.na(page), "", paste0(", p. ", page)))
type_label   <- function(self_reported) if_else(self_reported == 0L, "NSC (externo)", "Publicado por la escuela")

# (3.2): Shares printed directly in the documents (never acceptances).
direct <- extraction %>%
  filter(metric_std %in% outcome_info$outcome, measure_kind != "acceptance") %>%
  transmute(row_id, school_id, scope, outcome = metric_std,
            value = coalesce(value, numerator / denominator),
            cohort, timing, denominator_desc, metric_raw,
            source = source_label(doc_file, page), source_type = type_label(self_reported),
            measure_kind, derivation = "directo", notes) %>%
  group_by(school_id, scope, outcome, cohort, timing, value) %>%
  summarise(across(everything(), first), source = paste(unique(source), collapse = "; "), .groups = "drop")

# (3.3): Matriculation lists: match institutions to UW and to the selective list. UW-Madison
#        is matched before UW so that "UW-Madison" is not read as the University of Washington.
match_institution <- function(x) {
  x <- str_to_lower(str_squish(x))
  case_when(str_detect(x, "madison")                                                   ~ "UNIVERSITY OF WISCONSIN - MADISON",
            str_detect(x, "^(the )?(university of washington|u of washington|uw)([ ,(\\-–]|$)") ~ "UW",
            str_detect(x, "whitman college")                                           ~ "WHITMAN COLLEGE",
            str_detect(x, "stanford")                                                  ~ "STANFORD UNIVERSITY",
            str_detect(x, "university of pennsylvania|^upenn|^penn$")                  ~ "UNIVERSITY OF PENNSYLVANIA",
            str_detect(x, "carnegie mellon")                                           ~ "CARNEGIE MELLON UNIVERSITY",
            str_detect(x, "uc berkeley|university of california.*berkeley")            ~ "UNIVERSITY OF CALIFORNIA - BERKELEY",
            str_detect(x, "ucla|university of california.*los angeles")                ~ "UNIVERSITY OF CALIFORNIA-LOS ANGELES",
            str_detect(x, "ucsd|uc san diego|university of california.*san diego")     ~ "UNIVERSITY OF CALIFORNIA-SAN DIEGO",
            str_detect(x, "ucsb|uc santa barbara|university of california.*santa barbara") ~ "UNIVERSITY OF CALIFORNIA-SANTA BARBARA",
            str_detect(x, "uc irvine|university of california.*irvine")                ~ "UNIVERSITY OF CALIFORNIA - IRVINE",
            str_detect(x, "^(the )?university of michigan")                            ~ "UNIVERSITY OF MICHIGAN",
            str_detect(x, "university of southern california|^usc$")                   ~ "UNIVERSITY OF SOUTHERN CALIFORNIA",
            str_detect(x, "new york university|^nyu$")                                 ~ "NEW YORK UNIVERSITY",
            str_detect(x, "boston university")                                         ~ "BOSTON UNIVERSITY",
            str_detect(x, "^northeastern university")                                  ~ "NORTHEASTERN UNIVERSITY",
            str_detect(x, "tulane")                                                    ~ "TULANE UNIVERSITY")
}
matric <- extraction %>%
  filter(metric_std == "matric_count") %>%
  mutate(inst_key = match_institution(institution))
print(matric %>% filter(!is.na(inst_key)) %>% count(school_id, cohort, institution, inst_key), n = 60)

# (3.4): Per school x cohort: class size, list coverage, UW and selective-list counts. A list
#        counts as complete if the document says so or its counts cover >= 90% of the class.
class_sizes <- extraction %>%
  filter(metric_std == "class_size") %>%
  group_by(school_id, scope, cohort) %>%
  summarise(class_size = first(numerator), class_source = first(source_label(doc_file, page)), .groups = "drop")
matric_flag <- extraction %>%
  filter(metric_std == "matric_complete") %>%
  group_by(school_id, scope, cohort) %>%
  summarise(stated_complete = as.integer(max(value)), .groups = "drop")
matric_sum <- matric %>%
  group_by(school_id, scope, cohort) %>%
  summarise(n_listed   = sum(numerator, na.rm = TRUE),
            n_uw       = sum(numerator[inst_key %in% "UW"], na.rm = TRUE),
            n_sel      = sum(numerator[!is.na(inst_key)], na.rm = TRUE),
            n_4yr      = sum(numerator[sector %in% "4yr"], na.rm = TRUE),
            n_no_count = sum(is.na(numerator)),
            source     = paste(unique(source_label(doc_file, page)), collapse = "; "),
            self_reported = max(self_reported),
            .groups = "drop") %>%
  left_join(class_sizes, by = c("school_id", "scope", "cohort")) %>%
  left_join(matric_flag, by = c("school_id", "scope", "cohort")) %>%
  mutate(coverage = n_listed / class_size,
         complete = as.integer(n_no_count == 0 & !is.na(class_size) &
                                 (replace_na(stated_complete, 0L) == 1L | coverage >= 0.9)))
print(matric_sum, width = 200)

# (3.5): Derived shares. Only complete lists give comparable rates; partial lists are lower
#        bounds and go to the not-comparable table. The full selective-list share is never
#        comparable (the public counterpart only sees ERDC's top ~8 institutions).
derived <- matric_sum %>%
  filter(!is.na(class_size)) %>%
  transmute(school_id, scope, cohort, complete, coverage, source = paste0(source, "; tamaño de clase: ", class_source),
            source_type = type_label(self_reported),
            sel_uw = n_uw / class_size, enr_4yr = n_4yr / class_size, sel_list = n_sel / class_size,
            denominator_desc = paste0("graduados de la clase ", cohort, " (", class_size, ")")) %>%
  pivot_longer(c(sel_uw, enr_4yr, sel_list), names_to = "outcome", values_to = "value") %>%
  mutate(timing = NA_character_, metric_raw = "lista de matriculación", measure_kind = "matriculation",
         derivation = "calculado de la lista de matriculación", row_id = NA_integer_,
         notes = sprintf("la lista cubre %.0f %% de la clase", 100 * coverage))

# (3.6): Comparable metrics vs not-comparable figures.
comparison_rows <- bind_rows(direct, derived %>% filter(complete == 1L, outcome != "sel_list")) %>%
  left_join(outcome_info %>% select(outcome, outcome_es, erdc_def_es), by = "outcome") %>%
  left_join(private_schools %>% select(school_id, school_name), by = "school_id") %>%
  mutate(private_id = row_number())
not_comparable <- bind_rows(
  extraction %>%
    filter(measure_kind == "acceptance" | metric_std %in% c("other", "per_other")) %>%
    transmute(school_id, scope, metric_raw, value, numerator, cohort,
              source = source_label(doc_file, page), source_type = type_label(self_reported),
              reason_es = case_when(measure_kind == "acceptance"   ~ "Admisiones (aceptaciones), no inscripciones",
                                    metric_std == "per_other"      ~ "Persistencia con un denominador distinto al de ERDC",
                                    measure_kind == "matriculation" ~ "Lista sin conteos o sin tamaño de clase",
                                    TRUE                           ~ "Sin equivalente en ERDC"),
              notes),
  derived %>%
    filter(complete == 0L | outcome == "sel_list") %>%
    transmute(school_id, scope, metric_raw = paste0(outcome, " (", metric_raw, ")"), value, cohort, source, source_type,
              reason_es = if_else(outcome == "sel_list",
                                  "Lista completa de universidades selectivas: en las públicas solo se ven las ~8 instituciones más comunes",
                                  "Lista de matriculación incompleta: cota inferior"),
              notes)
) %>%
  left_join(private_schools %>% select(school_id, school_name), by = "school_id")
stopifnot(!any(comparison_rows$measure_kind == "acceptance"))
write_csv(not_comparable, here("output", "private_not_comparable.csv"))

#############################################
##  (4): Public-school benchmark positions.  ##
#############################################
# (4.1): Pooled public distribution (observed pooled rate, 15 comprehensive schools) for all
#        students and GPA >= 3.0, plus female graduates (same cohort windows) for the
#        all-girls school.
ranks <- read_csv(here("output", "rank_all_outcomes.csv"))
bench_pooled <- ranks %>%
  filter(school_type == "comprehensive", outcome %in% outcome_info$outcome, group %in% c("all", "gpa_hi"), !is.na(est)) %>%
  select(outcome, group, school_label, rate = est)
panel_female <- read_rds(here("output", "panel_female.rds"))
bench_female <- outcome_info %>%
  filter(outcome != "sel_uw") %>%
  pmap_dfr(function(outcome, first, last, ...) {
    panel_female %>%
      filter(outcome == !!outcome, school_type == "comprehensive") %>%
      select_cohorts(first, last) %>%
      pool_rates(outcome, group, school_label)
  }) %>%
  filter(!is.na(est)) %>%
  select(outcome, group, school_label, rate = est)
benchmark <- bind_rows(bench_pooled, bench_female) %>% mutate(basis = "pooled", cohort = NA_character_)

# (4.2): Same-cohort public distribution (single-year cohorts; all students and female only,
#        since the GPA series for Kent and Enumclaw exist only as 3-year windows).
private_cohorts <- comparison_rows %>% filter(str_detect(cohort, "^\\d{4}$")) %>% distinct(outcome, cohort)
panel_same <- bind_rows(read_rds(here("output", "panel_rates.rds")) %>% filter(group == "all"),
                        read_rds(here("output", "panel_selective.rds")),
                        panel_female) %>%
  filter(school_type == "comprehensive", cohort_span == 1, !is.na(rate)) %>%
  mutate(cohort = as.character(cohort)) %>%
  semi_join(private_cohorts, by = c("outcome", "cohort")) %>%
  transmute(outcome, group, school_label, rate, basis = "same_cohort", cohort)
benchmark <- bind_rows(benchmark, panel_same)
write_csv(benchmark, here("output", "private_public_benchmark.csv"))

# (4.3): Position of each private figure in each distribution (count of public schools below,
#        neighbours, quartiles). This is a position, not a rank. The female benchmark is
#        reported for the all-girls school only.
positions <- comparison_rows %>%
  select(private_id, school_id, outcome, value, cohort) %>%
  inner_join(benchmark %>% rename(bench_cohort = cohort), by = "outcome", relationship = "many-to-many") %>%
  filter(basis == "pooled" | bench_cohort == cohort,
         group != "female" | school_id == "holy_names") %>%
  group_by(private_id, basis, group) %>%
  summarise(n_public   = n(),
            n_below    = sum(rate < first(value)),
            pub_min    = min(rate), pub_p25 = quantile(rate, 0.25), pub_median = median(rate),
            pub_p75    = quantile(rate, 0.75), pub_max = max(rate),
            next_below = if (any(rate < first(value))) school_label[rate < first(value)][which.max(rate[rate < first(value)])] else NA_character_,
            next_above = if (any(rate >= first(value))) school_label[rate >= first(value)][which.min(rate[rate >= first(value)])] else NA_character_,
            .groups = "drop")
private_comparison <- comparison_rows %>%
  left_join(positions, by = "private_id", relationship = "one-to-many") %>%
  select(private_id, school_id, school_name, scope, outcome, outcome_es, value, cohort, timing,
         denominator_desc, metric_raw, derivation, source, source_type, erdc_def_es, basis, group,
         n_public, n_below, pub_min, pub_p25, pub_median, pub_p75, pub_max, next_below, next_above, notes)
print(private_comparison, width = 200)
write_csv(private_comparison, here("output", "private_comparison.csv"))

################################################
##  (5): Qualitative table and final checks.  ##
################################################
# (5.1): School x field table; undocumented fields stay blank, schools without documents
#        are marked "sin datos proporcionados" (no data provided).
qual_fields <- c("location", "commute", "enrollment_size", "coed", "advanced_courses", "tuition", "financial_aid", "model")
private_qual <- private_schools %>%
  select(school_id, school_name, has_docs) %>%
  crossing(field = qual_fields) %>%
  left_join(qualitative %>%
              group_by(school_id, field) %>%
              summarise(value  = paste(value, collapse = "; "),
                        source = paste(unique(source_label(doc_file, page)), collapse = "; "),
                        .groups = "drop"),
            by = c("school_id", "field")) %>%
  mutate(value = if_else(has_docs == 0L, "sin datos proporcionados", value),
         field = factor(field, levels = qual_fields)) %>%
  arrange(school_id, field)
print(private_qual, n = 30)
write_csv(private_qual, here("output", "private_qualitative.csv"))

# (5.2): Private schools must never appear in the public rankings or summary index.
private_pattern <- "Bellarmine|Holy Names|Cristo Rey"
stopifnot(!any(str_detect(ranks$school_label, private_pattern)),
          !any(str_detect(read_csv(here("output", "summary_index.csv"))$school_label, private_pattern)))

###########################
##  (6): Figure.         ##
###########################
# (6.1): Public schools (hollow grey dots) vs each private figure (diamond), pooled basis,
#        one panel per outcome and one row per public group.
bench_labels <- c(group_labels[c("gpa_hi", "all")], female = "Mujeres (todas)")
plot_bench <- benchmark %>%
  filter(basis == "pooled", outcome %in% comparison_rows$outcome) %>%
  filter(group != "female" | "holy_names" %in% comparison_rows$school_id) %>%
  left_join(outcome_info %>% select(outcome, outcome_es), by = "outcome") %>%
  mutate(group_lab = factor(bench_labels[group], levels = rev(bench_labels)))
plot_private <- comparison_rows %>%
  crossing(group = unique(plot_bench$group)) %>%
  filter(group != "female" | school_id == "holy_names") %>%
  mutate(group_lab = factor(bench_labels[group], levels = rev(bench_labels)),
         rate      = value,
         label     = paste0(if_else(scope == "network", "Red Cristo Rey (no es la escuela de Seattle)", school_name),
                            ", cohorte ", coalesce(cohort, "no indicada"),
                            " (", if_else(scope == "network", "publicado por la red", str_to_lower(source_type)), ")"))
p <- ggplot(plot_bench, aes(x = rate, y = group_lab)) +
  geom_point(shape = 21, colour = "grey55", fill = "white", size = 2.2,
             position = position_jitter(height = 0.12, width = 0, seed = 1)) +
  geom_point(data = plot_private, aes(colour = label), shape = 18, size = 4) +
  facet_wrap(~ outcome_es, ncol = 1, scales = "free_x") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  labs(title = "Escuelas privadas frente a las 15 secundarias públicas (no es un ranking)",
       x = NULL, y = NULL,
       caption = paste0("Círculos: escuelas públicas (tasa observada, cohortes agrupadas). Rombos: cifra documentada por cada escuela privada.\n",
                        "Las definiciones, cohortes y denominadores difieren; ver la tabla de cada escuela.")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.direction = "vertical", panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, colour = "grey40", size = 8), plot.title.position = "plot")
if (nrow(comparison_rows) > 0) ggsave(here("output", "figures", "private_vs_public.pdf"), p, width = 9, height = 2 + 2 * n_distinct(plot_bench$outcome), device = cairo_pdf)
if (nrow(comparison_rows) > 0) write_rds(p, here("output", "figures", "private_vs_public.rds"))
