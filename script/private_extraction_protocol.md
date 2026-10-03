# Private-school extraction protocol

This is step 0 of `08_private.R`. The source documents in `data/private/<school folder>/` are PDFs, images and spreadsheets, and they are transcribed by hand into CSVs in `output/private/`. `render.sh` does not regenerate these CSVs, so treat them as curated inputs.

**Rule: transcribe, never infer.** Every row must quote the document. If a number is not printed, leave the cell blank. Never fill a gap from memory or from outside sources.

## School ids
| school_id | School | Folder matched by (case-insensitive) |
|---|---|---|
| `bellarmine` | Bellarmine Preparatory School | `bellarmine` |
| `holy_names` | Holy Names Academy | `holy` |
| `cristo_rey` | Cristo Rey Jesuit Seattle High School | `cristo` |

## `output/private/extraction_<school_id>.csv`: one row per printed figure
| Column | Content |
|---|---|
| `school_id`, `school_name` | |
| `scope` | `school`, or `network` (Cristo Rey Network-wide figures) |
| `doc_file` | file name within the school folder |
| `doc_type` | `profile` / `matriculation_list` / `nsc_studenttracker` / `network_report` / `other` |
| `page` | page number (or sheet name) |
| `quote` | the verbatim text as printed (short) |
| `metric_raw` | the document's own label |
| `metric_std` | see the table below |
| `institution` | for `matric_count` rows: the institution name as printed |
| `sector` | for `matric_count` rows: `4yr` / `2yr` / `other` / blank if unknown |
| `value` | a share as a fraction in [0, 1] (e.g. 87% becomes 0.87); blank for counts |
| `numerator`, `denominator` | counts, if printed |
| `denominator_desc` | what the denominator is, in the document's words (e.g. "all graduates of the class of 2022") |
| `cohort` | graduating class year (e.g. `2022`); a range like `2019-2021` if pooled |
| `timing` | `fall` (fall immediately after HS) / `first_year` / `other` / blank |
| `measure_kind` | `acceptance` / `enrollment` / `matriculation` / `persistence` / `completion` / `descriptive` |
| `self_reported` | integer: 1 if published by the school, 0 if NSC-produced |
| `notes` | anything that affects comparability |

### `metric_std` values
| Value | Use for |
|---|---|
| `enr_4yr` | share of graduates enrolled at a 4-year institution |
| `enr_any` | share of graduates enrolled at any postsecondary institution |
| `per_4yr` | among 4-year enrollees, share still enrolled anywhere in year 2. Use this only if the denominator is 4-year enrollees; otherwise use `per_other` and describe the denominator |
| `comp_ba6` | share of graduates with a bachelor's within 6 years |
| `comp_any6` | share of graduates with any degree within 6 years |
| `class_size` | number of graduates in the class; goes in `numerator` |
| `matric_count` | number of students from the class enrolling at `institution`; goes in `numerator` |
| `matric_complete` | 1 in `value` if the document states the list covers the whole class; 0 if it says it is partial or a "selected" list |
| `other` | anything else, including all acceptance figures, "% attend college" claims, average SAT/GPA, and named lists without counts |

Acceptance figures always get `measure_kind = acceptance` and `metric_std = other`, even when the document calls them "college placement".

## Public schools: `output/public_profiles/extraction_college_board.csv`
The same columns and the same rule apply. One row per printed SAT or AP figure from the OSPI "Summary by School" PDFs saved in `data/public_profiles/`. The score or count goes in `numerator`, `value` stays blank, `metric_std = other` and `measure_kind = descriptive`. `metric_raw` is one of `SAT test-takers`, `Mean SAT total`, `Mean SAT ERW`, `Mean SAT Math`, `AP exam takers`, `AP exams taken` or `AP exams scored 3, 4 or 5`; the 2016 file uses `Mean SAT Critical Reading`, `Mean SAT Math` and `Mean SAT Writing`. `quote` holds the full OSPI row. `script/09_coursework_sat.R` reads this file.

## `output/private/qualitative_<school_id>.csv`
Columns: `school_id, field, value, doc_file, page, quote`. There is one row per field found. Omit fields that are not documented.

| field | Content |
|---|---|
| `location` | address or neighborhood/city |
| `commute` | distance or commute time from Auburn, only if a document states it |
| `enrollment_size` | total enrollment and/or class size |
| `coed` | coeducational or single-sex |
| `advanced_courses` | AP, IB, dual-credit or college-in-the-high-school offerings |
| `tuition` | published tuition |
| `financial_aid` | aid share, average award, need-based aid |
| `model` | Cristo Rey only: the corporate work-study program and the income-eligibility requirement, as stated |
