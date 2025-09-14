# Current workflow requires manual export of REDCap data

# Load packages -----------------------------------------------------------

library(dplyr)

# Get raw data from RedCAP ------------------------------------------------

# Step 1: export data from RedCAP website (output of this step: 2 csv files)
## TODO: extract data using use API key
## Step 1A: click on link to export data (this is located in a panel on the left hand side of the screen)
## Step 1B: click link to export "raw data" as csv file. Don't need to export "data labels"
## Step 1C: click link to export "data labels" as csv file

# Step 2: move csv files into package directory
## Drag the 2 csv files from steps 1B-1C into VIP/data-raw folder
## csv file that contains DATA LABELS in name: change file name to redcap_labels.csv
## other csv file that contains DATA in name: change file name to redcap_data.csv

# Step 3: upload data to R
core <- utils::read.csv(system.file("data-raw", "redcap_data.csv", package = "VIP"))
labels <- utils::read.csv(system.file("data-raw", "redcap_labels.csv", package = "VIP"), check.names = FALSE)

# Rename variables --------------------------------------------------------

# Create lookup table to track original variable names in RedCAP, variable names for excel files that will be uploaded to Google Drive, and variable definitions
new <- original <- names(core)

# Clean variable names to facilitate data wrangling

## Bizarre "1s" in variable name
new <- gsub("(outcome|comparator|vax)1", "\\1", new, perl = TRUE)

## Ensure consistency between use of abbreviations and full names
new <- gsub("adverse_events_", "ae_", new, fixed = TRUE)
new <- gsub("vaccine_effectiveness_", "ve_", new, fixed = TRUE)
new <- gsub("coadministration_", "coadmin_", new, fixed = TRUE)
new <- gsub("epidemiology_", "epi_", new, fixed = TRUE)
new <- gsub("ve_oc", "ve_outcome", new, fixed = TRUE)
new <- gsub("ve_ic", "ve_immunocomp", new, fixed = TRUE)
new <- gsub("influenza", "flu", new, fixed = TRUE)
new <- gsub("txt", "text", new, fixed = TRUE)
new <- gsub("include_", "incl_", new, fixed = TRUE)

## Convert "product_#" to "v#"
new <- gsub("product_([0-9]+)", "v\\1", new, perl = TRUE)

## Move name of virus to end
new <- gsub("(covid|rsv|flu)_complete", "complete_\\1", new, perl = TRUE)

## Ensure consistent use of y/yes and n/total
new <- gsub("vax_yes", "vax_y", new)
new <- gsub("vax_y$", "vax_with_outcome", new)
new <- gsub("vax_y_", "vax_with_outcome_", new)
new <- gsub("vax_n$", "vax_total", new)
new <- gsub("vax_n_", "vax_total_", new)

## Ensure consistent use of numbers
new <- gsub("two", "2", new, fixed = TRUE)
new <- gsub("three", "3", new, fixed = TRUE)
new <- gsub("four", "4", new, fixed = TRUE)
new <- gsub("five", "5", new, fixed = TRUE)
new <- gsub("six", "6", new, fixed = TRUE)
new <- gsub("seven", "7", new, fixed = TRUE)
new <- gsub("eight", "8", new, fixed = TRUE)
new <- gsub("nine", "9", new, fixed = TRUE)

# Other
new <- gsub("desrcibe", "describe", new, fixed = TRUE)
new <- gsub("em_type", "em_stat", new, fixed = TRUE)
new <- gsub("longterm_yn", "has_longterm_data", new, fixed = TRUE)
new <- gsub("otherinfo_yn", "has_other_data", new, fixed = TRUE)
new <- gsub("ae_incidence_yn", "ae_outcome_incidence_calculable", new, fixed = TRUE)
new <- gsub("ae_comparator", "ae_vax_comparator", new, fixed = TRUE)
new <- gsub("ae_outcome_num", "ae_vax_n_outcomes_reported", new, fixed = TRUE)
new <- gsub("ve_outcome_prime", "ve_outcome_1", new, fixed = TRUE)
new <- gsub("ve_outcome_num", "ve_outcome_n_reported", new, fixed = TRUE)
new <- gsub("ve_outcome_([0-9]+)_(text|other)", "ve_outcome_\\2_\\1", new, perl = TRUE)
new <- gsub("ve_outcome_text_([0-9]+)_([0-9]+)", "ve_outcome_text_\\1_v\\2", new, perl = TRUE)
new <- gsub("ve_(v[0-9]+)_complete", "ve_complete_\\1", new, perl = TRUE)
new <- gsub("vax([0-9]+)_type", "vax_type_\\1", new, perl = TRUE)
#new <- gsub("coadmin_sero_([0-9]+)", "coadmin_is_vs_solo_\\1", new, perl = TRUE)
#new[new == "coadmin_sero"] <- "coadmin_is_vs_solo"

# Create lookup table to rename variables
dict <- c(
  record_id = "id_redcap",
  study_label = "id_covidence",
  author = "author",
  pubyear = "pubyear",
  title = "title",
  virus___1 = "covid",
  virus___2 = "flu",
  virus___3 = "rsv",
  domain___1 = "ae",
  domain___2 = "ve",
  domain___3 = "coadmin",
  domain___4 = "epi",

  # AE vaccines
  domain_ae_include_vax___1 = "ae_bnt162b2",
  domain_ae_include_vax___2 = "ae_bnt162b2_xbb",
  domain_ae_include_vax___3 = "ae_mrna1273",
  domain_ae_include_vax___4 = "ae_mrna1273_xbb",
  domain_ae_include_vax___5 = "ae_cov2601",
  domain_ae_include_vax___6 = "ae_ad26",
  domain_ae_include_vax___7 = "ae_covid_vax_other",
  domain_ae_include_vax___8 = "ae_arexvy",
  domain_ae_include_vax___9 = "ae_abrysvo",
  domain_ae_include_vax___10 = "ae_mrna1345",
  domain_ae_include_vax___11 = "ae_nirsevimab",
  domain_ae_include_vax___12 = "ae_palivizumab",
  domain_ae_include_vax___13 = "ae_clesroviamab",
  domain_ae_include_vax___14 = "ae_iiv",
  domain_ae_include_vax___15 = "ae_rsv_vax_other",
  domain_ae_include_vax___16 = "ae_laiv",
  domain_ae_include_vax___17 = "ae_flu_vax_other",

  # AE outcomes
  domain_ae_include_yn = "ae_is_of_interest",
  domain_ae_include_list = "ae_not_of_interest",
  domain_ae_inc_other = "ae_other_ae_incl",
  domain_ae_inc_other_txt = "ae_other",

  # Number of vaccines
  domain_ae_num = "ae_n_vaccines_studied",
  domain_ve_num = "ve_n_vaccines_studied",

  # Study info
  setting = "study_setting",
  design = "study_design",
  design_other = "study_design_other",
  date_start_month = "date_start_month",
  date_start_year = "date_start_year",
  date_end_month = "date_end_month",
  date_end_year = "date_end_year",
  date_explain = "date_notes",

  # Population
  population___1 = "infant",
  population___2 = "child",
  population___3 = "adult",
  population___4 = "elder",
  population___5 = "preg",
  population___6 = "immunocomp",
  population___7 = "population_other_incl",
  population_premature = "perc_premature",
  immunocomp_def___1 = "sot",
  immunocomp_def___2 = "heme_malig",
  immunocomp_def___3 = "hiv_controlled",
  immunocomp_def___4 = "hiv_uncontrolled",
  immunocomp_def___5 = "solid_tumor",
  immunocomp_def___6 = "autoimmune",
  immunocomp_def___7 = "incl_immunocomp_other",
  immunocomp_def___8 = "undefined_immunocomp",
  immunocomp_def_other = "immunocomp_other",
  immunocomp_describe = "immunocomp_def",
  general_article_information_complete = "gen_info_status_complete",
  coadmin_vax_num = "coadmin_n_vax_approaches",
  coadmin_vax_prime = "coadmin_vax_product"
)

# Check to make sure all names(dict) are included in original
all(names(dict) %in% original)

# Create final lookup table
dict <- data.frame(
  original = original,
  new = new,
  description = names(labels)
) |>
  left_join(data.frame(original = names(dict), from_dict = unname(dict)), by = "original") |>
  mutate(new = ifelse(is.na(from_dict), new, from_dict)) |>
  select(-from_dict)

# Make sure all new names are unique
if (!identical(length(unique(dict$new)), length(dict$new))) {
  stop("dict$new contains duplicates")
}

# Rename columns
names(core) <- dict$new

remove(original, new, labels)

# Lookup tables for variable mapping --------------------------------------

# Reviewer
reviewer_lookup <- c(
  "1" = "Eric Meyerowitz",
  "2" = "Jake Scott",
  "3" = "Nicole McCann",
  "4" = "Caitlin Dugdale",
  "5" = "Aaron Richterman",
  "6" = "Jana Jarolimova",
  "7" = "Harleen Marwah",
  "8" = "Ethan Borre",
  "9" = "Katherine Rich",
  "10" = "Linh Le",
  "11" = "Christopher Alba",
  "12" = "Joseph Ladines-Lim",
  "13" = "Corey Watts",
  "14" = "Maria Sundaram",
  "15" = "Nicole Basta",
  "16" = "Elise Holmes",
  "17" = "Clare Stoddart",
  "18" = "Leah Moat",
  "19" = "Cory Anderson",
  "20" = "Angela Ulrich",
  "21" = "Derek Fleming",
  "22" = "Emily Smith",
  "23" = "Meredith Arpey",
  "24" = "Sydney Redepenning",
  "25" = "Anje Mehr",
  "26" = "Mike Abers"
)

# Study design
design_lookup <- c(
  "1" = "RCT",
  "2" = "Non-randomized trial",
  "3" = "Observational",
  "4" = "Case-control",
  "5" = "Cross-sectional",
  "6" = "Case series",
  "7" = "Other"
)

# Vaccine product
vax_lookup <- c(
  "1" = "COVID - BNT162b2",
  "2" = "COVID - BNT162b2_XBB.1.5",
  "3" = "COVID - mRNA-1273",
  "4" = "COVID - mRNA-1273_XBB.1.5",
  "5" = "COVID - NVX-CoV2373",
  "6" = "COVID - Ad26.COV2.S",
  "7" = "COVID - other",
  "8" = "RSV - Arexvy",
  "9" = "RSV - Abrysvo",
  "10" = "RSV - mRNA-1345",
  "11" = "RSV - Nirsevimab",
  "12" = "RSV - Palivizumab",
  "13" = "RSV - Clesroviamab",
  "14" = "Influenza - IIV",
  "15" = "RSV - other",
  "16" = "Influenza - LAIV",
  "17" = "Influenza - other",
  "18" = "Multiple",
  "19" = NA_character_
)

# AE comparator
ae_comparator_lookup <- c(
  "1" = "Unvaccinated",
  "2" = "Placebo",
  "3" = "Other",
  "4" = "None"
)

# VE comparator
ve_comparator_lookup <- c(
  "1" = "Unvaccinated",
  "2" = "Other active comparator",
  "3" = "None"
)

# AE outcome
ae_outcome_lookup <- c(
  "1" = "Influenza - GBS",
  "2" = "Influenza - Stroke",
  "3" = "Influenza - MI",
  "4" = "Influenza - School absenteeism",
  "5" = "RSV - GBS",
  "6" = "RSV - Stroke",
  "7" = "RSV - MI",
  "8" = "RSV - School absenteeism",
  "9" = "COVID - Myocarditis",
  "10" = "COVID - GBS",
  "11" = "COVID - ITP",
  "12" = "COVID - Stroke",
  "13" = "COVID - CVST",
  "14" = "COVID - School absenteeism",
  "15" = "Pregnant - Pre-eclampsia/eclampsia",
  "16" = "Pregnant - Gestational HTN",
  "17" = "Pregnant - Prematurity",
  "18" = "Pregnant - Placental abruption",
  "19" = "Pregnant - SGA",
  "20" = "Pregnant - Congenital defect",
  "21" = "Pregnant - Stillbirth",
  "22" = "Pregnant - Miscarriage"
)

# VE outcome
ve_outcome_lookup <- c(
  "1" = "Hospitalization",
  "2" = "Death",
  #"3" = "Hospitalization and/or death",
  "3" = "Hospitalization",
  "4" = "ICU admission",
  "5" = "Medically-attended infection",
  "6" = "School absenteeism",
  "7" = "Work absenteeism",
  "8" = "Symptomatic infection",
  "9" = "Other"
)

# AE EM type
ae_em_stat_lookup <- c(
  "1" = "IRR",
  "2" = "RR",
  "3" = "OR",
  "4" = "HR",
  "5" = "Other",
  "6" = "Not reported",
  "7" = "Prevalence ratio"
)

# VE EM type
ve_em_stat_lookup <- c(
  "1" = "(1 - OR)*100",
  "2" = "IRR",
  "3" = "RR",
  "4" = "OR",
  "5" = "HR",
  "6" = "Other",
  "7" = "Not reported",
  "8" = "Prevalence ratio"
)

# Maternal vax
maternalvax_lookup <- c(
  "1" = "Yes",
  "2" = "No",
  "3" = "Not applicable",
  "4" = "Not sure/not reported"
)

# Co-admin outcome
coadmin_outcome_lookup <- c(
  "1" = "Serologic outcome",
  #"2" = "Safety/adverse events",
  "2" = "Adverse events",
  "3" = "Other",
  "4" = "None"
)

# Co-admin comparison type
coadmin_comparison_lookup <- c(
  "1" = "Superior",
  #"2" = "Same/noninferior",
  "2" = "Noninferior",
  "3" = "Inferior",
  "4" = "Other",
  "5" = "Not reported"
)

# Clean data --------------------------------------------------------------

lookup_fn <- function(lookup) {
  names(lookup) <- NULL
  lookup_length <- length(lookup)
  function(x) {
    if (!is.integer(x)) return(x)
    if (all(is.na(x))) return(NA_character_)
    if (max(x, na.rm = TRUE) > lookup_length) return(x)
    lookup[x]
  }
}

core <- core |>
  filter(is.na(exclude) | exclude == 0) |>
  mutate(
    across(contains("complete"), function(x) {
      if (!is.integer(x) || max(x, na.rm = TRUE) > 2) return(x)
      case_when(
        x == 0L ~ "Incomplete",
        x == 1L ~ "Unverified",
        x == 2L ~ "Complete",
        .default = NA_character_
      )
    }),
    across(c(date_start_month, date_end_month), function(x) ifelse(x == 99, NA_integer_, x)),
    across(where(is.character), function(x) {
      x <- trimws(x)
      x[x %in% c("", "9999", "NA")] <- NA_character_
      x
    }),
    across(where(is.integer), function(x) ifelse(x == 9999, NA_integer_, x)),
    across(where(is.numeric), function(x) ifelse(x == 9999, NA_real_, x)),
    across(matches("vax_type$|vax_type_v?[0-9]|coadmin_vax_product"), lookup_fn(vax_lookup)),
    across(matches("^ae_vax_comparator$|ae_vax_comparator_v?[0-9]"), lookup_fn(ae_comparator_lookup)),
    across(matches("^ve_comparator$|ve_comparator_v?[0-9]"), lookup_fn(ve_comparator_lookup)),
    across(matches("^ae_outcome$|ae_outcome_v?[0-9]"), lookup_fn(ae_outcome_lookup)),
    across(matches("^ve_outcome$|ve_outcome_v?[0-9]"), lookup_fn(ve_outcome_lookup)),
    across(contains("ae_outcome_em_stat"), lookup_fn(ae_em_stat_lookup)),
    across(contains("ve_em_stat"), lookup_fn(ve_em_stat_lookup)),
    across(contains("maternalvax"), lookup_fn(maternalvax_lookup)),
    across(contains("coadmin_sero"), lookup_fn(coadmin_comparison_lookup)),
    across(contains("coadmin_outcome"), lookup_fn(coadmin_outcome_lookup)),
    reviewer = lookup_fn(reviewer_lookup)(reviewer),
    study_design = lookup_fn(design_lookup)(study_design),
    study_design = ifelse(study_design == "Other" & !is.na(study_design_other), study_design_other, study_design),
    study_design_other = ifelse(study_design == study_design_other, NA_character_, study_design_other),
    perc_premature = gsub("%", "", perc_premature),
    perc_premature = case_when(
      perc_premature == "Overall incidence: 64.2/1,000 births (95CI:62.0-66.4)" ~ "6.42",
      perc_premature == "-1" ~ NA_character_,
      .default = perc_premature
    ),
    population_other = ifelse(population_other_incl == 1 & !is.na(population_other), population_other, NA_character_),
    author = gsub(" [0-9]{4}", "", author)
  ) |>
  select(-population_other_incl)

# Check for columns containing 999
#core[, vars_which(core, function(x) any(x == 999, na.rm = TRUE))] %>%  filter(if_any(everything(), function(x) x == 999))

# Check whether perc_premature column can be converted to numeric
#sort(unique(core$perc_premature))
if (!any(grepl("[A-z]", core$perc_premature), na.rm = TRUE)) {
  core$perc_premature <- as.numeric(core$perc_premature)
}

# Remove columns which contain exclusively missing values or identical values in all rows
#names(core[!vapply(core, function(x) all(is.na(x)) || length(unique(x)) == 1L, logical(1), USE.NAMES = FALSE)])
core <- core[!vapply(core, function(x) all(is.na(x)) || length(unique(x)) == 1L, logical(1), USE.NAMES = FALSE)]

# Clean up workspace
remove(ae_comparator_lookup, ae_em_stat_lookup, ae_outcome_lookup, coadmin_comparison_lookup, coadmin_outcome_lookup, design_lookup, lookup_fn, maternalvax_lookup, reviewer_lookup, vax_lookup, ve_comparator_lookup, ve_em_stat_lookup, ve_outcome_lookup)

# Add comment columns -----------------------------------------------------

# Click link to export comment log as csv file (link below export data link on left hand side of screen)
# Move csv file to data-raw directory
# Rename csv file containing comments log to redcap_comments.csv

comments <- utils::read.csv(system.file("data-raw", "redcap_comments.csv", package = "VIP"), check.names = FALSE) |>
  select(
    id_redcap = Record,
    variable = Field,
    comment = Comment
  )

# Only include studies present in core
comments <- comments[comments$id_redcap %in% core$id_redcap, , drop = FALSE]
length(intersect(c("ae_vax_product", "n_vaccines_studied"), comments$variable)) == 0L
length(intersect(c("ae_vax_product", "n_vaccines_studied"), dict$new)) == 0L
comments <- comments |>
  left_join(select(dict, variable = original, new), by = "variable") |>
  mutate(
    variable = case_when(
      variable == "domain_ae_include_vax" ~ "ae_vax_product",
      variable == "ve_n_vaccines_studied" ~ "n_vaccines_studied",
      !is.na(new) ~ new,
      .default = variable
    )
  ) |>
  select(-new)

# Clean data
comments <- comments |>
  mutate(
    comment = gsub("\n", ". ", comment, fixed = TRUE),
    comment = gsub("\"", "'", comment),
    variable = case_when(
      id_redcap == 512 & variable == "ve_n_vaccines_studied" ~ "study_design",
      .default = variable
    )
  )

# Remove entries without useful info
comments <- comments |>
  filter(
    !(comment %in% c(
      "Updated",
      "Agree",
      "Eric had checked AE and VE but I think the paper only covers VE.",
      "'Safety and infectious outcomes in pediatric kidney transplant  recipients' - Described as safety, though also looks at VE for this population (not within our VE date window, however)",
      "VE on long COVID",
      "VE is out of range",
      "Also has VE but out of date range",
      "Not formally a co-administration study but analyses are stratified by whether patient received influenza vaccine (see raw data in supplement)",
      "This article presents data for 3 different studies- results are separated by study",
      "Cannot calculate overall incidence from a case-control study design.",
      "Cannot calculate overall incidence from case-control design.",
      "Confirmed calculation",
      "COVID vaccines are included too but not disaggregated.",
      "COVID vaccines not disaggregated. Only Moderna and Pfizer allowed",
      "Data for COVID-19 vaccine are not disaggregated by vaccine type",
      "fixed",
      "I changed this from 1 to 3",
      "I don't think this number is available in the data provided",
      "I don't think we have this number for booster cohort",
      "I don't think we have this number for the booster cohort",
      "I switched this designation from 'Other' to 'Observational' based on 8/5 meeting discussion",
      "Info on presentation",
      "ITP reported, but not by vaccine brand (study includes non-U.S. products)",
      "Not sure why they give a RR for preterm birth but not weight",
      "This meets our protocol criteria, in my opinion.",
      "This paper collects COVID vax data, but we are excluding it because it does not disaggregate by product.",
      "Note - article in COVIDENCE is wrong. Need to pull it up either in Zotero or online from CID",
      "Vaccine in study: SK's Inactivated Quadrivalent Seasonal . Influenza Vaccine (Fluarix Tetra)",
      "no negative controls and only comparing to non-US licensed vaccines for AEs not of special interest. May consider excluding. Leaving for second reviewer opinion."
    )
    ),
    !(variable == "date_notes" & comment %in% c("Interim safety and immunogenicity 29 days after vaccination are reported.", "Interim 15- and 29-day analysis results are reported.")),
    !(variable == "ve_child_vax_total_7" & comment == "Pediatric data not reported for this study")
  )

# Check for duplicate comments for same variable
#comments |>
#  group_by(id_redcap, variable) |>
#  summarize(
#    n = n(),
#    comment = paste(comment, collapse = "*****"),
#    .groups = "drop"
#  ) |>
#  filter(n > 1L)

# Collapse duplicates
comments <- comments |>
  group_by(id_redcap, variable) |>
  summarize(
    comment = paste(comment, collapse = ". "),
    .groups = "drop"
  )

# Create wide version
comments <- comments |>
  pivot_wider(
    names_from = variable,
    values_from = comment
  )

if (length(comments$comments[!is.na(comments$comments)]) == 1L && comments$comments[!is.na(comments$comments)] == "Counts and other AEs reported in Table S2 (added file to Covidence). Ns suggest denominator is 108. Also reports fatigue (n = 15, 13.9%), headache (4, 3.7%), chills and shivers (2, 1.9%), increased body temp (2, 1.9%), diarrhea (1, 0.9%), N/V (1, 0.9%), and arthralgias/back pain (1, 0.9%).") {
  comments$comments <- NULL
}

# Determine which columns in comments are article specific (and can therefore be added to core)
intersect(names(comments), names(core))
setdiff(names(comments), names(core))
sort(grepv("^id_redcap|^ae_|^ve_|^epi_|^coadmin|_v?[0-9]+$", names(comments), invert = TRUE))
z <- c("id_redcap", "date_end_month", "date_end_year", "date_start_month", "date_start_year", "immunocomp_def", "perc_premature", "population", "population_disagg", "population_other", "study_design", "study_setting", "virus")

z <- comments[intersect(z, names(comments))]|>
  remove_rows_all_na(-id_redcap)

new_core_cols <- names(z)
new_colnames <- paste0(names(z)[names(z) != "id_redcap"], "_notes")
length(intersect(new_colnames, names(core))) == 0L
!any(grepl("notes_notes", new_colnames))
names(z)[names(z) != "id_redcap"] <- new_colnames

# Add article specific columns to core
core <- left_join(core, z, by = "id_redcap")

# Remove article specific columns from comments
comments <- comments[c("id_redcap", setdiff(names(comments), new_core_cols))]
remove(z, new_colnames, new_core_cols)

# Remove rows with missing values for all variables (except id_redcap)
comments <- comments |>
  remove_rows_all_na(-id_redcap)

# Export data -------------------------------------------------------------

usethis::use_data(core, overwrite = TRUE)
usethis::use_data(dict, overwrite = TRUE)
usethis::use_data(comments, overwrite = TRUE)
