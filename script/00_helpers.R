#--------------------------------------------------------------------------------------------
# File name:         00_helpers.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - (none; sourced by the other scripts)
# Files created:
#   - (none)
# Purpose:
#   - Shared school lists, selective-institution list, pooling / EB-shrinkage / plotting helpers.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse, scales)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################

#############################################
##  (1): School and institution metadata.  ##
#############################################
# (1.1): Districts in scope (ERDC names; Sumner-Bonney Lake appears as "Sumner").
districts_in_scope <- c("Auburn", "Kent", "Federal Way", "Tahoma", "Enumclaw", "Sumner")

# (1.2): School classification. Anything in scope but not listed here is tagged "excluded"
#        (alternative, reengagement, online, transition programs; see 01_clean.R output).
school_meta <- tribble(
  ~school,                                 ~school_label,         ~school_type,
  "Auburn Senior High School",             "Auburn HS",           "comprehensive",
  "Auburn Mountainview High School",       "Auburn Mountainview", "comprehensive",
  "Auburn Riverside High School",          "Auburn Riverside",    "comprehensive",
  "Kent-Meridian High School",             "Kent-Meridian",       "comprehensive",
  "Kentlake High School",                  "Kentlake",            "comprehensive",
  "Kentridge High School",                 "Kentridge",           "comprehensive",
  "Kentwood High School",                  "Kentwood",            "comprehensive",
  "Decatur High School",                   "Decatur",             "comprehensive",
  "Federal Way High School",               "Federal Way HS",      "comprehensive",
  "Thomas Jefferson High School",          "Thomas Jefferson",    "comprehensive",
  "Todd Beamer High School",               "Todd Beamer",         "comprehensive",
  "Tahoma Senior High School",             "Tahoma",              "comprehensive",
  "Enumclaw Sr High School",               "Enumclaw",            "comprehensive",
  "Sumner High School",                    "Sumner",              "comprehensive",
  "Bonney Lake High School",               "Bonney Lake",         "comprehensive",
  "TAFA at Saghalie",                      "TAFA at Saghalie",    "choice",
  "Technology Access Foundation Academy",  "TAFA at Saghalie",    "choice",
  "Kent Laboratory Academy",               "Kent Laboratory Academy", "choice"
)

# (1.3): Selective institutions, matched on upper-cased ERDC institution names.
#        Rule: very low admit rates (roughly <=35% in recent cycles) or a flagship with
#        selective admission. UW pools Seattle/Bothell/Tacoma in ERDC data.
selective_institutions <- tribble(
  ~inst_key,                                   ~inst_label,             ~in_state,
  "UW",                                        "University of Washington (todos los campus)", 1L,
  "WHITMAN COLLEGE",                           "Whitman College",       1L,
  "STANFORD UNIVERSITY",                       "Stanford",              0L,
  "UNIVERSITY OF PENNSYLVANIA",                "University of Pennsylvania", 0L,
  "CARNEGIE MELLON UNIVERSITY",                "Carnegie Mellon",       0L,
  "UNIVERSITY OF CALIFORNIA - BERKELEY",       "UC Berkeley",           0L,
  "UNIVERSITY OF CALIFORNIA-LOS ANGELES",      "UCLA",                  0L,
  "UNIVERSITY OF CALIFORNIA-SAN DIEGO",        "UC San Diego",          0L,
  "UNIVERSITY OF CALIFORNIA-SANTA BARBARA",    "UC Santa Barbara",      0L,
  "UNIVERSITY OF CALIFORNIA - IRVINE",         "UC Irvine",             0L,
  "UNIVERSITY OF MICHIGAN",                    "University of Michigan", 0L,
  "UNIVERSITY OF SOUTHERN CALIFORNIA",         "USC",                   0L,
  "NEW YORK UNIVERSITY",                       "NYU",                   0L,
  "BOSTON UNIVERSITY",                         "Boston University",     0L,
  "NORTHEASTERN UNIVERSITY",                   "Northeastern",          0L,
  "TULANE UNIVERSITY",                         "Tulane",                0L,
  "UNIVERSITY OF WISCONSIN - MADISON",         "UW-Madison",            0L
)

# (1.4): Spanish display labels.
group_labels <- c(gpa_hi = "GPA ≥ 3.0 (alto rendimiento)",
                  all    = "Todos los estudiantes",
                  gpa_lo = "GPA < 3.0")

#########################################################
##  (2): Cohort selection, pooling and EB shrinkage.  ##
#########################################################
# (2.1): Keep cohorts in [first, last]. Single-cohort series (span 1) keep every year;
#        3-cohort trailing windows (span 3, Kent/Enumclaw GPA) keep non-overlapping
#        windows ending at last, last-3, ... down to first.
select_cohorts <- function(df, first, last) {
  df %>%
    filter(cohort >= first, cohort <= last,
           cohort_span == 1 | (last - cohort) %% 3 == 0)
}

# (2.2): Pool cohort-level rates for each school x group: equal-weight mean, between-cohort
#        SE = sd / sqrt(T), t-based 95% CI. Suppressed cohorts are missing (not zero).
pool_rates <- function(df, ...) {
  df %>%
    group_by(..., .add = FALSE) %>%
    summarise(cohorts     = if (any(!is.na(rate))) paste0(min(cohort[!is.na(rate)] - cohort_span[!is.na(rate)] + 1L), "–", max(cohort[!is.na(rate)])) else NA_character_,
              cohort_span = max(cohort_span),
              n_obs       = sum(!is.na(rate)),
              n_supp      = sum(suppressed),
              est         = if (n_obs > 0) mean(rate, na.rm = TRUE) else NA_real_,
              se          = if (n_obs > 1) sd(rate, na.rm = TRUE) / sqrt(n_obs) else NA_real_,
              .groups = "drop") %>%
    mutate(n_cohorts = n_obs * cohort_span,
           tcrit     = if_else(n_obs > 1, qt(0.975, pmax(n_obs - 1, 1)), NA_real_),
           ci_lo     = pmax(est - tcrit * se, 0),
           ci_hi     = pmin(est + tcrit * se, 1)) %>%
    select(-tcrit)
}

# (2.3): Empirical-Bayes (normal-normal) shrinkage. The prior (mu, tau2) is estimated by
#        DerSimonian-Laird on the rows flagged in_fit (the comprehensive schools); every row
#        with an SE is then shrunk toward mu with B = se^2 / (se^2 + tau2).
eb_shrink <- function(est, se, in_fit) {
  ok  <- in_fit & !is.na(est) & !is.na(se) & se > 0
  w   <- 1 / se[ok]^2
  k   <- sum(ok)
  mu_fe <- sum(w * est[ok]) / sum(w)
  q     <- sum(w * (est[ok] - mu_fe)^2)
  tau2  <- max(0, (q - (k - 1)) / (sum(w) - sum(w^2) / sum(w)))
  w_re  <- 1 / (se[ok]^2 + tau2)
  mu    <- sum(w_re * est[ok]) / sum(w_re)
  b     <- se^2 / (se^2 + tau2)
  eb    <- b * mu + (1 - b) * est
  eb_sd <- sqrt((1 - b) * se^2)
  tibble(eb = eb, eb_lo = pmax(eb - 1.96 * eb_sd, 0), eb_hi = pmin(eb + 1.96 * eb_sd, 1),
         shrink_b = b, mu_regional = mu, tau = sqrt(tau2))
}

# (2.4): Wilson interval, used only if graduate counts by school-year become available.
wilson_ci <- function(p, n, z = 1.96) {
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  tibble(ci_lo = centre - half, ci_hi = centre + half)
}

# (2.5): Full ranking pipeline for one outcome: select cohorts, pool, shrink (separately by
#        group), rank comprehensive schools, and flag schools statistically
#        indistinguishable from the top school (raw CI overlaps the top school's raw CI).
#        higher_better = FALSE ranks ascending (e.g. 2-year enrollment is shown unranked).
rank_outcome <- function(panel, outcome_id, first, last, higher_better = TRUE) {
  pooled <- panel %>%
    filter(outcome == outcome_id, school_type %in% c("comprehensive", "choice", "state")) %>%
    select_cohorts(first, last) %>%
    pool_rates(outcome, group, district, school, school_label, school_type)
  pooled %>%
    group_by(group) %>%
    group_modify(~ bind_cols(.x, eb_shrink(.x$est, .x$se, .x$school_type == "comprehensive"))) %>%
    mutate(sign      = if_else(higher_better, -1, 1),
           is_ranked = school_type == "comprehensive" & !is.na(est) & !is.na(eb),
           rank_raw  = if_else(is_ranked, as.integer(rank(if_else(is_ranked, sign * est, Inf), ties.method = "min")), NA_integer_),
           rank_eb   = if_else(is_ranked, as.integer(rank(if_else(is_ranked, sign * eb, Inf), ties.method = "min")), NA_integer_),
           top_lo    = ci_lo[which(rank_raw == 1)[1]],
           top_hi    = ci_hi[which(rank_raw == 1)[1]],
           indist_top = as.integer(is_ranked & ci_hi >= top_lo & ci_lo <= top_hi)) %>%
    ungroup() %>%
    select(-sign, -is_ranked, -top_lo, -top_hi) %>%
    mutate(first_cohort = first, last_cohort = last) %>%
    arrange(group, rank_eb)
}

##########################
##  (3): Plot helpers.  ##
##########################
# (3.1): Dot-and-CI plot. Raw pooled rate with 95% CI (line + hollow dot) and EB-shrunk rate
#        (filled dot), comprehensive schools only, panels by group. Schools are ordered by
#        the EB rate within the first panel. The dashed line is the regional mean.
plot_dotci <- function(rank_df, title, x_lab = "Tasa", groups = c("gpa_hi", "all"),
                       pct = TRUE) {
  d <- rank_df %>%
    filter(school_type == "comprehensive", group %in% groups, !is.na(est)) %>%
    mutate(group_lab = factor(group_labels[group], levels = group_labels[groups]))
  ord <- d %>% filter(group == groups[1]) %>% arrange(eb) %>% pull(school_label)
  ord <- c(setdiff(unique(d$school_label), ord), ord)
  d <- d %>% mutate(school_label = factor(school_label, levels = ord),
                    indist = if_else(indist_top == 1, "Indistinguible de la mejor", "Por debajo de la mejor"))
  ggplot(d, aes(y = school_label)) +
    geom_vline(aes(xintercept = mu_regional), linetype = "dashed", colour = "grey55") +
    geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi, colour = indist), width = 0, linewidth = 0.7, orientation = "y") +
    geom_point(aes(x = est, colour = indist), shape = 21, fill = "white", size = 2.3) +
    geom_point(aes(x = eb), shape = 16, size = 1.6, colour = "black") +
    facet_wrap(~ group_lab, nrow = 1) +
    scale_colour_manual(values = c("Indistinguible de la mejor" = "#1b6ca8",
                                   "Por debajo de la mejor"     = "#b0b0b0"), name = NULL) +
    scale_x_continuous(labels = if (pct) label_percent(accuracy = 1) else label_comma()) +
    labs(title = title, x = x_lab, y = NULL,
         caption = "Círculo hueco y línea: tasa observada (promedio de cohortes) con IC 95%. Punto negro: tasa ajustada (bayes empírico).\nLínea discontinua: media regional.") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.caption = element_text(hjust = 0, colour = "grey40", size = 8),
          plot.title.position = "plot")
}
