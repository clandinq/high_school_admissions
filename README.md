# Auburn-area high school graduate outcomes (ERDC)

Descriptive rankings of the 15 comprehensive public high schools in the Auburn, Kent, Federal Way,
Tahoma, Enumclaw and Sumner-Bonney Lake districts (Washington) on postsecondary outcomes, for
high-GPA graduates (HS GPA >= 3.0) and for all graduates. The report is in Spanish and is published
from `docs/index.html` via GitHub Pages.

## Data
Five CSV exports from the ERDC *High School Graduate Outcomes* dashboard
(https://erdc.wa.gov/data-dashboards/high-school-graduate-outcomes), downloaded 2026-10-02, saved in
`data/` with their original names (`High_School_Graduate_Outcomes_-_<Topic>_20261002.csv`; topics:
Earnings, Enrollment_by_Institution, First_Year_Enrollment, Persistence___Retention,
Post_Secondary_Completion). `data/` is not tracked (the files exceed GitHub's size limit).

## Pipeline
- `script/00_helpers.R`: school lists, selective-institution list, pooling / empirical-Bayes / plot helpers
- `script/01_clean.R`: filter and tidy the raw files into `output/*.rds`
- `script/02_enrollment.R` ... `06_earnings.R`: one outcome family each -> `output/rank_*.csv`, `output/figures/*.pdf`
- `script/07_compare.R`: high-GPA vs all-student rank shifts, equal-weight standardized summary, checks
- `script/08_private.R`: separate (unranked) comparison of three private schools against the public distribution.
  Inputs are hand transcriptions in `output/private/` of documents saved in `data/private/<school>/`
  (official school / Cristo Rey Network pages retrieved 2026-10-02; URLs in each `sources.csv`);
  see `script/private_extraction_protocol.md`. These transcriptions are curated inputs, not regenerated.
- `report/hs_outcomes_report.qmd`: Spanish report

Run everything and rebuild the site with `./render.sh` (needs R with tidyverse, here, gt, scales, and
the Quarto CLI).
