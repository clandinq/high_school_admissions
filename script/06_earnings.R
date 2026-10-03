#--------------------------------------------------------------------------------------------
# File name:         06_earnings.R
# Creation date:     2026-10-02
# Author:            César Landín
# Files used:
#   - here("output", "earnings.rds")
# Files created:
#   - here("output", "earnings_table.csv")
#   - here("output", "figures", "earnings.pdf")
# Purpose:
#   - Describe (not rank) median earnings by school for comprehensive schools versus the state.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
pacman::p_load(here, tidyverse, scales)
options(readr.show_col_types = FALSE,
        scipen = 999)
#########################################################
source(here("script", "00_helpers.R"))

######################################
##  (1): Build the earnings table.  ##
######################################
# (1.1): Load school and state earnings (state rows come from ERDC's "StatewideComp" label).
earn <- read_rds(here("output", "earnings.rds")) %>%
  filter(school_type %in% c("comprehensive", "state"))

# (1.2): Keep year 10 for bachelor's and HS diploma, plus year 5 for HS diploma only.
earn_all <- earn %>%
  filter(group %in% c("all", "gpa_hi"),
         (credential %in% c("Bachelor's or Higher", "HS Diploma") & years_after == 10) |
           (credential == "HS Diploma" & years_after == 5)) %>%
  select(school_label, district, school_type, group, credential, years_after, median_earnings, n_workers)

# (1.3): Ratio to the state median for the same credential / group / year.
state_val <- earn_all %>%
  filter(school_type == "state") %>%
  select(group, credential, years_after, state_earnings = median_earnings)
earn_table <- earn_all %>%
  left_join(state_val, by = c("group", "credential", "years_after")) %>%
  mutate(ratio_to_state = median_earnings / state_earnings) %>%
  select(-state_earnings) %>%
  arrange(credential, years_after, group, desc(median_earnings))
stopifnot(!anyNA(earn_table$ratio_to_state))
write_csv(earn_table, here("output", "earnings_table.csv"))

# (1.4): Wide version for inspection.
earn_wide <- earn_table %>%
  mutate(col = paste(group, if_else(credential == "HS Diploma", "hs", "ba"), years_after, sep = "_")) %>%
  select(school_label, col, median_earnings) %>%
  pivot_wider(names_from = col, values_from = median_earnings) %>%
  arrange(desc(gpa_hi_ba_10))
print(earn_wide, n = 20)

# (1.5): Missing cells (school x group x credential x year combinations that are absent).
expected <- earn_table %>%
  distinct(school_label) %>%
  crossing(distinct(earn_table, group, credential, years_after))
missing_cells <- expected %>%
  anti_join(earn_table, by = c("school_label", "group", "credential", "years_after")) %>%
  arrange(school_label, credential, years_after, group)
print(sprintf("Missing school x group x credential x year cells: %d of %d", nrow(missing_cells), nrow(expected)))
print(missing_cells, n = 50)

# (1.6): Plausibility check on the school medians.
print(summary(earn_table$median_earnings[earn_table$school_type == "comprehensive"]))
print(summary(earn_table$n_workers[earn_table$school_type == "comprehensive"]))

#############################
##  (2): Earnings figure.  ##
#############################
# (2.1): Year-10 dot plot. Schools ordered by the bachelor's, GPA >= 3.0 value; vertical line = state.
plot_df <- earn_table %>%
  filter(years_after == 10)
ord <- plot_df %>%
  filter(school_type == "comprehensive", credential == "Bachelor's or Higher", group == "gpa_hi") %>%
  arrange(median_earnings) %>%
  pull(school_label)
ord <- c(setdiff(unique(plot_df$school_label[plot_df$school_type == "comprehensive"]), ord), ord)
cred_labels <- c("Bachelor's or Higher" = "Título universitario o superior", "HS Diploma" = "Solo diploma de preparatoria")
plot_df <- plot_df %>%
  mutate(group_lab = factor(group_labels[group], levels = group_labels[c("gpa_hi", "all")]),
         cred_lab  = factor(cred_labels[credential], levels = cred_labels))
p <- ggplot(plot_df %>% filter(school_type == "comprehensive") %>% mutate(school_label = factor(school_label, levels = ord)),
            aes(x = median_earnings, y = school_label, colour = group_lab)) +
  geom_vline(data = plot_df %>% filter(school_type == "state"),
             aes(xintercept = median_earnings, colour = group_lab), linetype = "dashed", show.legend = FALSE) +
  geom_point(size = 2.3, alpha = 0.9) +
  facet_wrap(~ cred_lab, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = c("#1b6ca8", "#d9822b"), name = NULL) +
  scale_x_continuous(labels = label_dollar(scale = 1 / 1000, suffix = "k", accuracy = 1)) +
  labs(title = "Ingresos (descriptivo, no es un ranking)",
       x = "Mediana de ingresos anuales, 10 años después de graduarse (USD)", y = NULL,
       caption = paste0("Línea discontinua: mediana estatal (mismo grupo). Cifras condicionadas al nivel educativo más alto alcanzado, ",
                        "solo empleo en WA\n(quienes se van del estado no aparecen) y con todas las cohortes agrupadas; no hay intervalos de confianza.")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, colour = "grey40", size = 8),
        plot.title.position = "plot")
ggsave(here("output", "figures", "earnings.pdf"), p, width = 9, height = 5.5, device = cairo_pdf)
