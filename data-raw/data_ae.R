# Make sure VIP::core is updated (if unsure, run data_core.R file in data-raw folder before running script below)

# Load packages -----------------------------------------------------------

library(tidyr)
library(dplyr)

# Functions ---------------------------------------------------------------

# Add columns from comments data frame to df_ae data frame
transfer_if_any <- function(.comments, .x, .transform_new_colnames = function(x) paste0(x, "NOTES"), .verbose = FALSE) {
  x_names <- names(.x)
  comments_names <- names(.comments)
  cols <- intersect(comments_names, x_names)
  cols <- cols[cols != "id_redcap"]
  if (length(cols) == 0L) {
    return(list(x = .x, comments = .comments, new_cols = NULL))
  }
  new_colnames <- .transform_new_colnames(cols)
  if (length(z <- intersect(new_colnames, x_names)) != 0L) {
    stop(sprintf("The following column names are already present in df_ae: %s", paste(z, collapse = ", ")))
  }
  if (any(z <- grepl("notes_notes", new_colnames))) {
    warning(sprintf("New column name(s) %s will be generated", paste(new_colnames[z], collapse = ", ")))
  }
  z <- .comments[c("id_redcap", cols)]
  names(z)[-1L] <- new_colnames
  x_new <- left_join(.x, z, by = "id_redcap")
  if (.verbose) {
    message(sprintf("Adding the following variables to df_ae: %s", paste(new_colnames, collapse = ", ")))
  }
  comments_new <- remove_rows_all_na(.comments[setdiff(comments_names, cols)], -id_redcap)
  list(
    x = x_new,
    comments = comments_new,
    new_cols = new_colnames
  )
}

# Create AE-specific data frame -------------------------------------------

# Remove irrelevant vars
df_ae <- VIP::core |>
  filter(ae == 1) |>
  select(-starts_with(c("ve_", "epi_", "coadmin_")), -c(ve, coadmin, epi))
names(df_ae)[names(df_ae) == "ae_cov2601"] <- "ae_novavax2601"

df_ae <- df_ae[!vapply(df_ae, function(x) all(is.na(x)) || length(unique(x)) == 1L, logical(1), USE.NAMES = FALSE)]

# Remove status columns
df_ae <- df_ae[grepv("^ae_v[0-9]+_complete$", names(df_ae), invert = TRUE)]

# Prepare comments data
comments <- VIP::comments |>
  select(-starts_with(c("ve_", "epi_", "coadmin_", "rob_")), -any_of(c("ae_outcome_em_value_7_v2"))) |>
  filter(id_redcap %in% .env$df_ae$id_redcap) |>
  remove_rows_all_na(-id_redcap) |>
  remove_cols_all_na()

# Transfer "vax_product_used_in_study_notes" column from comments to df_ae
z <- comments |>
  select(id_redcap, vax_product_used_in_study_notes = ae_vax_product) |>
  filter(!is.na(vax_product_used_in_study_notes))
df_ae <- left_join(df_ae, z, by = "id_redcap")
comments <- comments |>
  select(-ae_vax_product) |>
  remove_rows_all_na(-id_redcap)
remove(z)

# Check for columns present in comments but not df_ae
#setdiff(names(comments), names(df_ae))
length(setdiff(names(comments), names(df_ae))) == 0L

# Create long version of data ---------------------------------------------

# All outcome-specific variables ("ae_outcome" variables) should have a column name that ends with "_#_v#" suffix indicating AE outcome and vaccine

add_vax_outcome_id <- function(x, idx = grepl("ae_outcome", x, fixed = TRUE)) {
  # Add vax id
  needs_vax_id <- idx & !grepl("_v[0-9]+$", x)
  x[needs_vax_id] <- paste0(x[needs_vax_id], "_v1")
  # Add outcome id
  needs_outcome_id <- idx & !grepl("_[0-9]+_v[0-9]+$", x)
  x[needs_outcome_id] <- gsub("(_v[0-9]+)$", "_1\\1", x[needs_outcome_id], perl = TRUE)
  x
}
names(df_ae) <- add_vax_outcome_id(names(df_ae))
names(comments) <- add_vax_outcome_id(names(comments))
remove(add_vax_outcome_id)

# All vaccine-specific variables ("ae_vax" variables) should have a column name that ends with "v#" suffix indicating vaccine type
add_vax_id <- function(x, idx = grepl("ae_vax", x, fixed = TRUE)) {
  # Add vax id
  needs_vax_id <- idx & !grepl("_v[0-9]+$", x)
  x[needs_vax_id] <- paste0(x[needs_vax_id], "_v1")
  x
}
names(df_ae) <- add_vax_id(names(df_ae))
names(comments) <- add_vax_id(names(comments))
remove(add_vax_id)

# df_ae has 3 variable categories:

## 1. Variables that are independent of specific vaccine/AE outcome. These are article (study) specific variables
df_main_cols <- df_ae[grepv("_[0-9]|_v[0-9]", names(df_ae), invert = TRUE)]
if (length(intersect(names(df_main_cols), names(comments))) > 1L) {
  df_main_cols <- transfer_if_any(.comments = comments, .x = df_main_cols)
  if (length(df_main_cols$new_cols) > 0L && any(df_main_cols$new_cols %in% names(df_ae))) {
    stop(sprintf("The following columns are already present in df_ae: %", paste(df_main_cols$new_cols, collapse = ", ")))
  }
  comments <- df_main_cols$comments
  df_main_cols <- df_main_cols$x
}

# NEW starts here
# Next line needed because when chopping up data in subsequent steps, a single column could be numeric for one subset and character for another. Need to join them together later using bind_rows
z <- mutate(df_ae, across(-id_redcap, as.character))

## 2. Variables that are vaccine-specific but independent of AE outcome
#j <- grepv("_[0-9]+_v[0-9]+$", grepv("v[0-9]+$", setdiff(names(df_ae), names(df_main_cols))), invert = TRUE)
#paste(sort(unique(gsub("_v[0-9]+$", "", j))), collapse = "|")
df_vax <- z[grepl("ae_vax_comparator|ae_vax_comparator_other|ae_vax_n_outcomes_reported|ae_vax_type|ae_vax_type_other", names(z))]
df_vax$id_redcap <- z$id_redcap
if (length(intersect(names(df_vax), names(comments))) > 1L) {
  df_vax <- transfer_if_any(.comments = comments, .x = df_vax, .transform_new_colnames = function(x) gsub("(ae_vax_comparator_other|ae_vax_comparator|ae_vax_n_outcomes_reported|ae_vax_type_other|ae_vax_type)", "\\1NOTES", x, perl = TRUE))
  if (length(df_vax$new_cols) > 0L && any(df_vax$new_cols %in% names(df_ae))) {
    stop(sprintf("The following columns are already present in df_ae: %", paste(df_vax$new_cols, collapse = ", ")))
  }
  comments <- df_vax$comments
  df_vax <- df_vax$x
  #df_vax$id_redcap <- NULL
}

## 3. Variables that are both vaccine-specific and AE outcome-specific
df_vax_outcome <- z[setdiff(names(z), c(names(df_vax), names(df_main_cols)))]
if (length(grepv("_[0-9]+_v[0-9]+?", names(df_vax_outcome), invert = TRUE) != 0L)) {
  stop(sprintf("The following columns in df_vax_outcome do not contain _#_v# suffix: %s", paste(grepv("_[0-9]+_v[0-9]+?", names(df_vax_outcome), invert = TRUE), collapse = ", ")))
}
df_vax_outcome$id_redcap <- z$id_redcap

if (length(intersect(names(df_vax_outcome), names(comments))) > 1L) {
  df_vax_outcome <- transfer_if_any(.comments = comments, .x = df_vax_outcome, .transform_new_colnames = function(x) sub("(.*?)(?=(_v[0-9]|_[0-9]))", "\\1NOTES", x, perl = TRUE))
  if (length(df_vax_outcome$new_cols) > 0L && any(df_vax_outcome$new_cols %in% names(df_ae))) {
    stop(sprintf("The following columns are already present in df_ae: %", paste(df_vax_outcome$new_cols, collapse = ", ")))
  }
  if (any(idx <- !grepl("NOTES_v?[0-9]", df_vax_outcome$new_cols))) {
    stop(sprintf("The following columns do not contain 'NOTES' followed by _# or _v# pattern", paste(df_vax_outcome$new_cols[idx])))
  }
  comments <- df_vax_outcome$comments
  df_vax_outcome <- df_vax_outcome$x
  #df_vax_outcome$id_redcap <- NULL
}

# Check that row order is identical in df_ae, df_main_cols, df_vax, and df_vax_outcome
if (length(unique(list(df_ae$id_redcap, df_main_cols$id_redcap, df_vax$id_redcap, df_vax_outcome$id_redcap))) != 1L) {
  stop("Rows are not identifcal for df_ae, df_main_cols, df_vax, and df_vax_outcome")
} else {
  # Remove id_redcap column from df_vax and df_vax_outcome
  df_vax$id_redcap <- df_vax_outcome$id_redcap <- NULL
}

# Review df_vax_outcome to make sure all column names end in v#
#grepv("_[0-9]|_v[0-9]", names(df_vax_outcome), invert = TRUE)
length(grepv("v[0-9]", names(df_vax_outcome), invert = TRUE)) == 0L

# Check if comments contains any additional columns (other than id_redcap)
length(setdiff(names(comments), "id_redcap")) == 0L

# Remove "ae_" prefix from columns in df_main_cols and df_vax to facilitate future pivoting
names(df_main_cols) <- gsub("^ae_", "", names(df_main_cols))
#names(df_vax) <- gsub("ae_complete", "vax_complete", names(df_vax))
names(df_vax) <- gsub("^ae_", "", names(df_vax))

# Create ae data frame
## Loop through vax/outcome endings
vax_outcomes <- unique(unlist(regmatches(names(df_vax_outcome), gregexpr("_[0-9]+_v[0-9]+$", names(df_vax_outcome))), use.names = FALSE))
ae <- mapply(
  function(x, y) {
    out <- cbind(
      df_main_cols,
      df_vax[endsWith(names(df_vax), y)],
      df_vax_outcome[endsWith(names(df_vax_outcome), x)]
    )
    names(out) <- gsub(x, "", names(out))
    names(out) <- gsub(y, "", names(out))
    names(out) <- gsub("ae_outcome_", "", names(out))
    names(out) <- gsub("^ae_", "", names(out))
    names(out) <- gsub("^[_]+|[_]+$", "", names(out))
    out
  },
  # vax/outcome endings
  x = vax_outcomes,
  # vax endings
  y = vapply(regmatches(vax_outcomes, gregexpr("_v[0-9]+$", vax_outcomes)), function(z) z[1L], character(1), USE.NAMES = FALSE),
  SIMPLIFY = FALSE
)
ae <- bind_rows(ae)

# Rename columns
#names(ae) <- gsub("EMstatOther", "em_stat_other", names(ae), fixed = TRUE)
#names(ae) <- gsub("EMstat", "em_stat", names(ae), fixed = TRUE)
#names(ae) <- gsub("EMadj", "em_adj", names(ae), fixed = TRUE)
#names(ae) <- gsub("EMCI", "em_ci", names(ae), fixed = TRUE)
#names(ae) <- gsub("EM", "em", names(ae), fixed = TRUE)
#names(ae) <- gsub("UnvaxTotal", "n_unvaccinated_total", names(ae), fixed = TRUE)
#names(ae) <- gsub("UnvaxWithOutcome", "n_unvaccinated_with_outcome", names(ae), fixed = TRUE)
#names(ae) <- gsub("VaxTotal", "n_vaccinated_total", names(ae), fixed = TRUE)
#names(ae) <- gsub("VaxWithOutcome", "n_vaccinated_with_outcome", names(ae), fixed = TRUE)

# Clean data --------------------------------------------------------------

# Convert columns to their proper type (must run this line before subsequent steps)
ae <- utils::type.convert(ae, as.is = TRUE)

# Convert "NOTES" in column names to "_notes"
z <- gsub("NOTES", "_notes", names(ae), fixed = TRUE)
z <- gsub("[_]+", "_", z)

if (anyDuplicated(z)) {
  stop(sprintf("The following columns in ae already exist: %s", paste(z[duplicated(z)], collapse = ", ")))
}
names(ae) <- z

# Place "notes" columns immediately after their parent column

remove(comments, df_main_cols, df_vax, df_vax_outcome, idx, transfer_if_any, vax_outcomes)

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
      vax_type == "COVID - other" & vax_type_other %in% c("COVID - mRNA vaccine", "COVID - mRNA vaccines", "Pfizer and Moderna Combined", "Co-admin of bivalent COVID-19 mRNA vaccines and seasonal inactivated influenza vaccines") ~ "COVID - mRNA vaccines",
      covid == 1 & flu == 0 & rsv == 0 & vax_type %in% c("COVID - other", "Multiple") & vax_type_notes %in% c("Extracting the pooled pfizer/moderna data due to lack incidence rate data", "mRNA vaccines for SARS-CoV-2, such as BNT162b2 (BioNTech-Pfizer) and. mRNA-1273 (Moderna)") ~ "COVID - mRNA vaccines",
      vax_type == "Influenza - other" & vax_type_other %in% c("Influenza - IIV") ~ vax_type_other,
      vax_type == "Influenza - other" & vax_type_other %in% c("Influenza - mRNA-1083", "Influenza - mRNA-1010") ~ "Influenza - mRNA vaccines",
      vax_type_other == "COVID - BNT162b2_BA4.5" ~ "COVID - BNT162b2",
      vax_type == "COVID - other" & vax_type_other %in% c("BNT162b2 BA.4.5 vaccine", "BNT162b2 bivalent booster") ~ "COVID - BNT162b2",
      vax_type == "COVID - other" & vax_type_other %in% c("mRNA-1273 bivalent booster", "mRNA-1273.222 (bivalent COVID)") ~ "COVID - mRNA-1273",
      vax_type == "Multiple" & covid == 0 & rsv == 0 & flu == 1 ~ "Influenza - other",
      vax_type == "Multiple" & covid == 1 & rsv == 0 & flu == 0 ~ "COVID - other",
      vax_type == "Multiple" & covid == 0 & rsv == 1 & flu == 0 ~ "RSV - other",
      .default = vax_type
    ),
    vax_type_comments = ifelse(vax_type_other == vax_type, NA_character_, vax_type_other),
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
## Next line remove rows in which outcome is missing
ae <- ae |>
  filter(
    #    !(vax_type_notes == "mRNA-1010" & virus == "Influenza")
    vax_type != "Influenza - mRNA vaccines"
  )

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
      immunocomp == 1 & population %in% c("12y-64y") ~ "child/adult/immunocomp",
      population %in% c("12y-64y") ~ "child/adult",
      population %in% c("Patients 12y+", ">=16 years") ~ "child/adult/elder",
      population %in% c("All adults in Denmark recommended to receive the 2024-2025 JN.1-co ntaining booster vaccine (ie, those aged 65 years or individuals in high-risk groups)") ~ "elder",
      .default = population
    ),
    across(where(is.character), function(x) ifelse(x == "other", "Other", x))
  )

# Make sure "outcome_virus" and "virus" are identical
identical(ae$virus, ae$outcome_virus)

ae |>
  filter(is.na(virus) | is.na(outcome_virus) | virus != outcome_virus) |>
  distinct(virus, outcome_virus, vax_type, vax_type_comments, vax_type_notes)|>
  arrange(virus, vax_type, outcome_virus)
ae |>
  filter(
    (!is.na(virus) & !is.na(outcome_virus) & virus != outcome_virus) | (is.na(virus) & !is.na(outcome_virus))
  )
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
    vax_product, vax_type, vax_type_comments,
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
    #exclude, gen_info_status_complete, comments, comments_complete, second_review_yn, second_review_comment,
    ## Population specific
    pops_in_study,
    infant, child, adult, elder, preg, immunocomp, population_other, perc_premature,
    sot, heme_malig, hiv_controlled, solid_tumor, autoimmune, incl_immunocomp_other, undefined_immunocomp, immunocomp_other, immunocomp_def,
    population_disagg, population_comment,
    ## Other
    everything(),
    -c(n_vaccines_studied, vax_n_outcomes_reported)
  )

# Move notes columns to be next to parent column
z <- grepv("_notes$", names(ae))
move_notes_col <- function(df, notes_col) {
  df_names <- names(df)
  col <- gsub("_notes$", "", notes_col)
  if (!any(idx1 <- df_names == col)) return(df)
  idx1 <- which(idx1)
  idx2 <- which(df_names == notes_col)
  idx_pre <- c(setdiff(seq_len(idx1), idx2), idx2)
  idx_all <- seq_len(ncol(df))
  idx <- unique(c(idx_pre, idx_all))
  df[idx]
}

for (i in z) {
  ae <- move_notes_col(ae, i)
}

# Limit to AE of interest
#ae <- ae |> filter(is_of_interest == 1)
#ae <- ae |> select(-c(other_ae_incl, other))

# Data for AE not of interest
#df_ae_not_of_interest <- select(df_ae, id_redcap, id_covidence, reviewer, author, pubyear, study_design, study_setting, infant, child, adult, elder, preg, immunocomp, population_other, population_comment, is_of_interest, ae_not_of_interest, as_other_incl = ae_other_ae_incl, ae_other)

# Clean up workspace
remove(df_ae, move_notes_col, i, z)

# Export data -------------------------------------------------------------

usethis::use_data(ae, overwrite = TRUE)
