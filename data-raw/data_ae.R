# Make sure VIP::core is updated (if unsure, run data_core.R file in data-raw folder before running script below)

# Load packages -----------------------------------------------------------

library(tidyr)
library(dplyr)

# Note from Eric on 8/14/25 for RedCap 335. Add this info to "Comments / data issues" column in google doc
## Important myocarditis study. Its also extremely complicated — I'm happy to talk through it with whoever is looking at it if anything is confusing and can also input data differently depending on what you think is best. A few general points:
# They have so much data - we combined myocarditis and pericarditis (since that made sense to me clinically) for raw numbers but left aHR as myocarditis from the tables
# We left ischemic and hemorrhagic strokes separated but those may need to be condensed
# The HRs are adjusted for 100K person/years, but event numbers are also there. I wasn't sure if the "no vaccine group" was helpful — because it is adjusted, so without the person year calculation it may not be useful, but we left for now
# For "booster" - we chose supplementary tables 23 and 24 which are mRNA-1273 or BNT162b2 booster after any primary series (primary series included the Astrazeneca vaccine), but I think that is ok because we are looking at all boosters and this was the best way to capture the total number of events

# Create AE-specific data frame -------------------------------------------

# Remove irrelevant vars
df_ae <- VIP::core |>
  filter(ae == 1) |>
  select(-starts_with(c("ve_", "epi_", "coadmin_")), -c(ve, coadmin, epi))

# Limit to AE of interest
#df_ae <- df_ae |> filter(ae_is_of_interest == 1) |> select(-c(ae_other_ae_incl, ae_other))

#df_ae <- remove_constant_cols(df_ae)
df_ae <- df_ae[!vapply(df_ae, function(x) all(is.na(x)) || length(unique(x)) == 1L, logical(1), USE.NAMES = FALSE)]

# Create long version of data ---------------------------------------------

# All "ae_outcome" variable names should have a "_#_v#" suffix indicating AE outcome and vaccine
add_vax_outcome_id <- function(x, idx = grepl("ae_outcome", x, fixed = TRUE)) {
  # Add vax id
  needs_vax_id <- idx & !grepl("_v[0-9]", x)
  x[needs_vax_id] <- paste0(x[needs_vax_id], "_v1")
  # Add outcome id
  needs_outcome_id <- idx & !grepl("_[0-9]+_v[0-9]", x)
  x[needs_outcome_id] <- gsub("(_v[0-9])", "_1\\1", x[needs_outcome_id], perl = TRUE)
  x
}
names(df_ae) <- add_vax_outcome_id(names(df_ae))
remove(add_vax_outcome_id)

# All "ae_vax" variable names should have a "v#" suffix indicating vaccine type
add_vax_id <- function(x, idx = grepl("ae_vax", x, fixed = TRUE)) {
  # Add vax id
  needs_vax_id <- idx & !grepl("_v[0-9]", x)
  x[needs_vax_id] <- paste0(x[needs_vax_id], "_v1")
  x
}
names(df_ae) <- add_vax_id(names(df_ae))
remove(add_vax_id)

# Study-specific variables
df_main_cols <- df_ae[grepv("_[0-9]|_v[0-9]", names(df_ae), invert = TRUE)]

# Create a data frame for each vaccine
suffix <- regmatches(names(df_ae), gregexpr("_[0-9]+_v[0-9]+$", names(df_ae)))
suffix <- vapply(suffix, function(x) if (length(x) == 0L) NA_character_ else x, character(1), USE.NAMES = FALSE)
#suffix_vax <- gsub("_[0-9]+_", "", suffix)

#z <- grepv("ae_vax|ae_outcome", names(df_ae))
z <- names(df_ae)
has_vax_id <- grepl("v[0-9]+$", z)
has_outcome_id <- grepl("_[0-9]+_v[0-9]+$", z)
is_vax_only <- has_vax_id & !has_outcome_id
# Next line needed because when chopping up data in subsequent steps, a single column could be numeric for one subset and character for another. Need to join them together later using bind_rows
df_char <- mutate(df_ae, across(everything(), as.character))
create_rows <- function(x) {
  suffix_vax <- gsub("_[0-9]+_", "", x)
  # Vaccine-specific columns
  vax_colnames <- z[endsWith(z, suffix_vax) & is_vax_only]
  # Outcome-specific columns
  outcome_colnames <- z[endsWith(z, x)]
  out <- cbind(
    df_main_cols,
    df_char[vax_colnames],
    df_char[outcome_colnames]
  )
  names(out) <- gsub(x, "", names(out))
  names(out) <- gsub(suffix_vax, "", names(out))
  names(out) <- gsub("ae_outcome_", "", names(out))
  names(out) <- gsub("^ae_", "", names(out))
  names(out) <- gsub("^[_]+|[_]+$", "", names(out))
  out
}
ae <- lapply(unique(suffix[!is.na(suffix)]), create_rows)
ae <- bind_rows(ae)

remove(df_char, df_main_cols, has_outcome_id, has_vax_id, is_vax_only, suffix, z, create_rows)

# Clean output ------------------------------------------------------------

ae <- ae |>
  mutate(
    vax_type_other = case_when(
      tolower(vax_type_other) %in% c("comirnarty or spikevax", "pfizer or moderna vaccine") ~ "COVID - mRNA vaccines",
      tolower(vax_type) %in% c("unspecified inactive vaccines") ~ "Influenza - IIV",
      tolower(vax_type) == "bnt162b2 ba.4.5 vaccine" ~ "COVID - BNT162b2_BA4.5",
      tolower(vax_type_other) %in% c("nondescript influenza vaccines", "seasonal influenza vaccines", "seasonal influenza vaccination") ~ "Influenza - other",
      tolower(vax_type_other) == "mrna-1083 vaccine (mrna influenza vacine)" ~ "Influenza - mRNA-1083",
      vax_type == "Influenza - other" & tolower(vax_type_other) == "mrna-1010" ~ "Influenza - mRNA-1010",
      vax_type_other == "Influenza - mixed" ~ "Influenza - other",
      .default = vax_type_other
    ),
    vax_type = ifelse(is.na(vax_type), vax_type_other, vax_type),
    vax_type = case_when(
      is.na(vax_type) ~ vax_type_other,
      vax_type == "COVID - other" & vax_type_other %in% c("COVID - mRNA vaccine") ~ vax_type_other,
      vax_type == "Influenza - other" & vax_type_other %in% c("Influenza - IIV") ~ vax_type_other,
      vax_type == "Influenza - other" & vax_type_other %in% c("Influenza - mRNA-1083", "Influenza - mRNA-1010") ~ "Influenza - mRNA vaccines",
      vax_type_other == "COVID - BNT162b2_BA4.5" ~ "COVID - BNT162b2",
      vax_type == "Multiple" & covid == 0 & rsv == 0 & flu == 1 ~ "Influenza - other",
      .default = vax_type
    ),
    vax_type_notes = ifelse(vax_type_other == vax_type, NA_character_, vax_type_other),
    vax = vax_type,
    outcome_original = outcome
  ) |>
  separate_wider_delim(
    cols = vax,
    delim = " - ",
    names = c("virus", "vax_product"),
    too_few = "align_start"
  ) |>
  separate_wider_delim(
    cols = outcome,
    delim = " - ",
    names = c("outcome_virus", "outcome"),
    too_few = "align_start"
  ) |>
  select(-vax_type_other) |>
  mutate(
    vax_product = ifelse(is.na(vax_product) & !is.na(virus), virus, vax_product),
    outcome = ifelse(is.na(outcome) & !is.na(outcome_virus), outcome_virus, outcome),
    notes = case_when(
      outcome_virus == "Pregnant" & preg == 0 ~ "Pregnancy not selected under population options, but outcome is pregnancy-related",
      .default = NA_character_
    ),
    preg = ifelse(outcome_virus == "Pregnant", 1L, preg),
    outcome_virus = ifelse(outcome_virus == "Pregnant", virus, outcome_virus),
    study_period = case_when(
      is.na(date_start_year) ~ as.character(date_end_year),
      is.na(date_end_year) ~ as.character(date_start_year),
      .default = paste(date_start_year, date_end_year, sep = "-")
    ),
    article = paste(author, pubyear),
    virus = case_when(
      virus == "Multiple" & !is.na(outcome_virus) ~ outcome_virus,
      virus == "Multiple" & covid == 1 & rsv == 0 & flu == 0 ~ "COVID",
      virus == "Multiple" & covid == 0 & rsv == 1 & flu == 0 ~ "RSV",
      virus == "Multiple" & covid == 0 & rsv == 0 & flu == 1 ~ "Influenza",
      .default = virus
    )
  )

# Remove excess columns and rows
ae <- ae |>
  filter(vax_type != "Influenza - mRNA vaccines")

# Sanity check for covid, rsv, flu columns. Output should be 1 for all
ae |>
  filter(virus == "COVID") |>
  pull(covid) |>
  unique()

ae |>
  filter(virus == "RSV") |>
  pull(rsv) |>
  unique()

ae |>
  filter(virus == "Influenza") |>
  pull(flu) |>
  unique()

# Convert columns to their proper type
ae <- utils::type.convert(ae, as.is = TRUE)

# Add column that lists all populations in study
ae <- ae |>
  rowwise() |>
  mutate(
    pops_in_study = paste("infant"[infant], "child"[child], "adult"[adult], "elder"[elder], "pregnant"[preg], "immunocomp"[immunocomp], "", sep = "/")
  ) |>
  ungroup()

ae$pops_in_study <- gsub("NA", "", ae$pops_in_study)
ae$pops_in_study <- gsub("[/]+", "/", ae$pops_in_study)
ae$pops_in_study <- gsub("^/|/$", "", ae$pops_in_study)
ae$pops_in_study[ae$pops_in_study == ""] <- NA_character_

ae <- ae |>
  mutate(
    is_pop_other = ifelse(!is.na(population_other), 1L, 0L),
    #pops_in_study = trimws(gsub("^, |, $", "", pops_in_study)),
    #pops_in_study = ifelse(!grepl("[A-z]", pops_in_study), NA_character_, pops_in_study),
    population = ifelse(is_pop_other & !is.na(population_other), population_other, pops_in_study),
    population_original = population,
    population = case_when(
      outcome %in% c("Placental abruption", "Pre-eclampsia/eclampsia", "Gestational HTN", "Stillbirth", "Congenital defect", "Prematurity", "SGA") ~ "pregnant",
      tolower(population) %in% c("children 0-17", "infants birth through 12 months") ~ "infant/child",
      tolower(population) %in% c("neonates/newborns") ~ "infant",
      tolower(population) %in% c("adults 60 years and older", "adults 60+") ~ "elder",
      tolower(population) %in% c("50-87 years", "all adults 18+y", "all adults 18+", "adults 18+") ~ "adult/elder",
      population %in% c("12y-64y") ~ "child/adult",
      population %in% c("Patients 12y+", ">=16 years") ~ "child/adult/elder",
      .default = population
    ),
    across(where(is.character), function(x) ifelse(x == "other", "Other", x))
  )

# Make sure "outcome_virus" and "virus" are identical
identical(ae$virus, ae$outcome_virus)
ae |>
  filter(is.na(virus) | is.na(outcome_virus) | virus != outcome_virus) |>
  distinct(virus, outcome_virus, vax_type, vax_type_notes)|>
  arrange(virus, vax_type, outcome_virus)
ae$outcome_virus <- NULL

substring(ae$population, 1L, 1L) <- toupper(substring(ae$population, 1L, 1L))

ae <- ae |>
  mutate(
    outcome_vax_product = paste(outcome, vax_product, sep = " - "),
    outcome_vax_product = gsub("^NA - ", "", outcome_vax_product),
    virus = case_when(
      virus == "Multiple" & covid == 1 & rsv == 0 & flu == 0 ~ "COVID",
      .default = virus
    ),
    vax_product = case_when(
      vax_product == "Other" & virus == "Influenza" ~ "Influenza - other",
      .default = vax_product
    )
  )

# Edits from Caitlin
ae <- ae |>
  mutate(
    population = case_when(
      preg == 1 & outcome == "MI" ~ "Pregnant",
      outcome %in% c("SGA", "Prematurity") ~ "Pregnant",
      virus == "RSV" & preg == 1 & vax_product != "Nirsevimab" ~ "Pregnant",
      virus == "RSV" & population == "Pregnant" & vax_product == "Nirsevimab" ~ "Infant",
      .default = population
    )
  )

# Added 8/15/25 per e-mail from Caitlin. Remove rows with Arexvy for peds
ae <- ae |>
  filter(
    !(population %in% c("Infant", "Child", "Infant/child", "Pregnant") & vax_product == "Arexvy")
  )

ae$study_design[ae$study_design == "self-controlled case series"] <- "Self-controlled study"

# Reorder columns ---------------------------------------------------------

ae <- ae |>
  select(
    # Key study (article) identifiers
    id_redcap, id_covidence, reviewer, article, study_period,
    # Highly relevant columns
    virus,
    outcome_vax_product,
    vax_product, vax_type, vax_type_notes,
    population,
    study_design, study_setting,
    outcome, outcome_definition = describe,
    comparator_vax = vax_comparator, comparator_vax_other = vax_comparator_other,
    incidence_calculable,
    # Raw data
    n_vaccinated_total = vax_total, n_vaccinated_with_outcome = vax_with_outcome, n_unvaccinated_total = unvax_total, n_unvaccinated_with_outcome = unvax_with_outcome,
    em = em_value, em_ci, em_stat,
    disagg, disagg_summary = disagg_text,
    has_longterm_data, has_other_data,
    # Other columns
    is_pop_other,
    ## Study specific
    author, pubyear, title, date_start_month, date_start_year, date_end_month, date_end_year, date_notes,
    ## Status
    exclude, gen_info_status_complete, comments, comments_complete, second_review_yn, second_review_comment,
    ## Population specific
    pops_in_study,
    infant, child, adult, elder, preg, immunocomp, population_other, perc_premature,
    sot, heme_malig, hiv_controlled, solid_tumor, autoimmune, incl_immunocomp_other, undefined_immunocomp, immunocomp_other, immunocomp_def,
    population_disagg, population_comment,
    ## Other
    everything(),
    -c(n_vaccines_studied, vax_n_outcomes_reported)
  )

# Data for AE not of interest
df_ae_not_of_interest <- select(df_ae, id_redcap, id_covidence, reviewer, author, pubyear, study_design, study_setting, infant, child, adult, elder, preg, immunocomp, population_other, population_comment, ae_is_of_interest, ae_not_of_interest, as_other_incl = ae_other_ae_incl, ae_other)

# Clean up workspace
remove(df_ae)

# Export data -------------------------------------------------------------

# Store local version of cleaned AE data (including version in long format with 1 row per vaccine/AE outcome)
#usethis::use_data(df_ae, overwrite = TRUE)
usethis::use_data(ae, overwrite = TRUE)
