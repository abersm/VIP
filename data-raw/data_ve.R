# Make sure VIP::core is updated (if unsure, run data_core.R file in data-raw folder before running script below)

# Load packages -----------------------------------------------------------

library(tidyr)
library(dplyr)

# Functions ---------------------------------------------------------------

# Remove rows which contain NA in all selected columns
remove_rows_all_na <- function(df, ...) {
  df_subset <- dplyr::select(df, ...)
  rows_all_na <- apply(df_subset, 1, function(x) all(is.na(x)))
  df[!rows_all_na, , drop = FALSE]
}

# Add columns from comments data frame to df_ve data frame
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
    stop(sprintf("The following column names are already present in df_ve: %s", paste(z, collapse = ", ")))
  }
  if (any(z <- grepl("notes_notes", new_colnames))) {
    warning(sprintf("New column name(s) %s will be generated", paste(new_colnames[z], collapse = ", ")))
  }
  z <- .comments[c("id_redcap", cols)]
  names(z)[-1L] <- new_colnames
  x_new <- left_join(.x, z, by = "id_redcap")
  if (.verbose) {
    message(sprintf("Adding the following variables to df_ve: %s", paste(new_colnames, collapse = ", ")))
  }
  comments_new <- remove_rows_all_na(.comments[setdiff(comments_names, cols)], -id_redcap)
  list(
    x = x_new,
    comments = comments_new,
    new_cols = new_colnames
  )
}

# Create VE-specific data frame -------------------------------------------

# Check for studies in which ve == 0 but ve data entered
#VIP::core %>%
#  filter(ve == 0) %>%
#  select(id_redcap, starts_with("ve"), -contains("complete"), -ve) %>%
#  remove_rows_all_na(-id_redcap)

# Remove irrelevant variables
df_ve <- VIP::core |>
  filter(ve == 1) |>
  select(-starts_with(c("ae_", "epi_", "coadmin_")), -c(ae, coadmin, epi)) |>
  rename(n_vaccines_studied = ve_n_vaccines_studied) |>
  mutate(
    across(where(is.character), function(x) gsub("\002", "", x, fixed = TRUE))
  )

df_ve <- df_ve[!vapply(df_ve, function(x) all(is.na(x)) || length(unique(x)) == 1L, logical(1), USE.NAMES = FALSE)]

# Remove unused variables
df_ve <- df_ve |>
  select(-matches("ve_overall_yn|ve_outcome_n_reported"))

# Prepare comments data
comments <- VIP::comments |>
  select(-starts_with(c("ae_", "epi_", "coadmin_", "rob_", "nos_cc")), -matches("ve_overall_yn|ve_outcome_n_reported")) |>
  filter(id_redcap %in% .env$df_ve$id_redcap) |>
  remove_rows_all_na(-id_redcap)

if (any(names(comments) == "ve_n_vaccines_studied")) {
  names(comments)[names(comments) == "ve_n_vaccines_studied"] <- "n_vaccines_studied"
}

# Check for columns present in comments but not df_ve
## Output should be TRUE
length(setdiff(names(comments), names(df_ve))) == 0L
#setdiff(names(comments), names(df_ve))

# Add vaccine-specific id ("v#" pattern) to variables with implied "v1"
## df_ve
old_names <- names(df_ve)
needs_vax_id <- grepl("^ve_", old_names) & !grepl("_v[0-9]", old_names)
names(df_ve)[needs_vax_id] <- paste0(old_names[needs_vax_id], "_v1")

## comments
old_names <- names(comments)
needs_vax_id <- grepl("^ve_", old_names) & !grepl("_v[0-9]", old_names)
names(comments)[needs_vax_id] <- paste0(old_names[needs_vax_id], "_v1")
remove(old_names, needs_vax_id)

# Review endings
## Note: ve_outcome_text and ve_complete variables can have v3 suffix but all other columns that end in v3 must also have preceding v2 (i.e., ending is v2_v3)
## Next line should return ve_outcome_text_1_v3, ve_outcome_text_2_v3, ve_outcome_text_3_v3, ve_complete_v3
setdiff(grepv("_v3", names(df_ve)), grepv("_v2_v3", names(df_ve)))
## Next line should return character(0)
setdiff(grepv("_v3", names(comments)), grepv("_v2_v3", names(comments)))

# To fix this, just remove _v2 from _v2_v3 suffix
names(df_ve) <- gsub("_v2_v3", "_v3", names(df_ve))
names(comments) <- gsub("_v2_v3", "_v3", names(comments))

# Create long version of data ---------------------------------------------

# Next line needed because when chopping up data in subsequent steps, a single column could be numeric for one subset and character for another. Need to join them together later using bind_rows
z <- mutate(df_ve, across(-id_redcap, as.character))

# df_ve has 3 variable categories:

## 1. Variables that are independent of specific vaccine/VE outcome. These are article (study) specific variables
df_main_cols <- z[!grepl("_[0-9]|_v[0-9]", names(z))]
## Next line should be FALSE
if (length(intersect(names(df_main_cols), names(comments))) > 1L) {
  df_main_cols <- transfer_if_any(.comments = comments, .x = df_main_cols)
  if (length(df_main_cols$new_cols) > 0L && any(df_main_cols$new_cols %in% names(df_ve))) {
    stop(sprintf("The following columns are already present in df_ve: %", paste(df_main_cols$new_cols, collapse = ", ")))
  }
  comments <- df_main_cols$comments
  df_main_cols <- df_main_cols$x
}

## 2. Variables that are vaccine-specific but independent of VE outcome
df_vax <- z[grepl("ve_complete|ve_timeline|ve_comparator|ve_vax", names(z))]
df_vax$id_redcap <- z$id_redcap
## Next line should be TRUE
if (length(intersect(names(df_vax), names(comments))) > 1L) {
  df_vax <- transfer_if_any(.comments = comments, .x = df_vax, .transform_new_colnames = function(x) gsub("(ve_comparator_vax|ve_comparator|ve_complete|ve_timeline|ve_vax_other|ve_vax_type)", "\\1NOTES", x, perl = TRUE))
  ## Next line should be FALSE
  if (length(df_vax$new_cols) > 0L && any(df_vax$new_cols %in% names(df_ve))) {
    stop(sprintf("The following columns are already present in df_ve: %", paste(df_vax$new_cols, collapse = ", ")))
  }
  comments <- df_vax$comments
  df_vax <- df_vax$x
  #df_vax$id_redcap <- NULL
}

## 3. Variables that are both vaccine-specific and VE outcome-specific
df_vax_outcome <- z[setdiff(names(z), c(names(df_vax), names(df_main_cols)))]
df_vax_outcome$id_redcap <- z$id_redcap
## Next line should be TRUE
if (length(intersect(names(df_vax_outcome), names(comments))) > 1L) {
  df_vax_outcome <- transfer_if_any(.comments = comments, .x = df_vax_outcome, .transform_new_colnames = function(x) sub("(.*?)(?=(_v[0-9]|_[0-9]))", "\\1NOTES", x, perl = TRUE))
  ## Next line should be FALSE
  if (length(df_vax_outcome$new_cols) > 0L && any(df_vax_outcome$new_cols %in% names(df_ve))) {
    stop(sprintf("The following columns are already present in df_ve: %", paste(df_vax_outcome$new_cols, collapse = ", ")))
  }
  ## Next line should be FALSE
  if (any(idx <- !grepl("NOTES_v?[0-9]", df_vax_outcome$new_cols))) {
    stop(sprintf("The following columns do not contain 'NOTES' followed by _# or _v# pattern", paste(df_vax_outcome$new_cols[idx])))
  }
  comments <- df_vax_outcome$comments
  df_vax_outcome <- df_vax_outcome$x
  #df_vax_outcome$id_redcap <- NULL
}

# Check that row order is identical in df_ve, df_main_cols, df_vax, and df_vax_outcome
## Next line should be FALSE
if (length(unique(list(df_ve$id_redcap, df_main_cols$id_redcap, df_vax$id_redcap, df_vax_outcome$id_redcap))) != 1L) {
  stop("Rows are not identifcal for df_ve, df_main_cols, df_vax, and df_vax_outcome")
} else {
  # Remove id_redcap column from df_vax and df_vax_outcome
  df_vax$id_redcap <- df_vax_outcome$id_redcap <- NULL
}

# Review df_vax_outcome to make sure all column names end in v#
#grepv("_[0-9]|_v[0-9]", names(df_vax_outcome), invert = TRUE)
## Next line should be TRUE
length(grepv("v[0-9]", names(df_vax_outcome), invert = TRUE)) == 0L

# Check if comments contains any additional columns (other than id_redcap)
## Next line should be TRUE
length(setdiff(names(comments), "id_redcap")) == 0L

# Remove "ve_" prefix from columns in df_main_cols and df_vax to facilitate future pivoting
## Next line should be FALSE
if (length(grepv("ve_", names(df_main_cols))) != 0L) {
  names(df_main_cols) <- gsub("ve_complete", "vax_complete", names(df_main_cols))
  names(df_main_cols) <- gsub("^ve_", "", names(df_main_cols))
}

names(df_vax) <- gsub("ve_complete", "vax_complete", names(df_vax))
names(df_vax) <- gsub("^ve_", "", names(df_vax))

# Remove "ve_" prefix from population independent columns in df_vax_outcome
names(df_vax_outcome) <- gsub("^ve_(outcome|maternalvax|em_stat)", "\\1", names(df_vax_outcome))

# Prepare substring in column names that precedes population identifier to facilitate future pivoting
names(df_vax_outcome) <- gsub("em_stat_other", "EMstatOther", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("em_stat", "EMstat", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("em_adj", "EMadj", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("em_ci", "EMCI", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("_em", "_EM", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("unvax_total", "UnvaxTotal", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("unvax_with_outcome", "UnvaxWithOutcome", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("vax_total", "VaxTotal", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("vax_with_outcome", "VaxWithOutcome", names(df_vax_outcome), fixed = TRUE)

# Create list of vaccine IDs for each column in df_vax and df_vax_outcome
vax_suffix <- regmatches(names(df_vax), gregexpr("_v[0-9]+", names(df_vax)))
if (all(lengths(vax_suffix) != 1L)) {
  stop("1 or more rows in df_vax contain > 1 vaccine ID")
}
vax_suffix <- unlist(vax_suffix, use.names = FALSE)

# Create list of vaccine IDs and outcome IDs for each column in df_vax_outcome
id_vax <- regmatches(names(df_vax_outcome), gregexpr("_v[0-9]+", names(df_vax_outcome)))
if (!all(lengths(id_vax) == 1L)) {
  stop("1 or more rows in df_vax_outcome contain > 1 outcome ID")
}
id_vax <- unlist(id_vax, use.names = FALSE)
id_outcome <- regmatches(names(df_vax_outcome), gregexpr("_[0-9]+", names(df_vax_outcome)))
id_outcome <- vapply(id_outcome, function(x) if (length(x) == 0L) "_1" else x, character(1), USE.NAMES = FALSE)

# Add outcome to column names
names(df_vax_outcome) <- paste0(
  gsub("(_[0-9]+)?(_v[0-9+])$", "", names(df_vax_outcome)),
  id_outcome,
  id_vax
)

# Create ve data frame
## Loop through vax/outcome endings
vax_outcomes <- unique(unlist(regmatches(names(df_vax_outcome), gregexpr("_[0-9]+_v[0-9]+$", names(df_vax_outcome))), use.names = FALSE))
ve <- mapply(
  function(x, y) {
    out <- cbind(
      df_main_cols,
      df_vax[endsWith(names(df_vax), y)],
      df_vax_outcome[endsWith(names(df_vax_outcome), x)]
    )
    names(out) <- gsub(x, "", names(out))
    names(out) <- gsub(y, "", names(out))
    out
  },
  # vax/outcome endings
  x = vax_outcomes,
  # vax endings
  y = vapply(regmatches(vax_outcomes, gregexpr("_v[0-9]+$", vax_outcomes)), function(z) z[1L], character(1), USE.NAMES = FALSE),
  SIMPLIFY = FALSE
)

# Make sure all columns with "ve_" prefix have 3 pieces when split by "_"
z <- ve |>
  lapply(names) |>
  unlist(use.names = FALSE)
z <- grepv("^ve", z) |>
  strsplit("_")
if (!all(lengths(z) == 3L)) {
  stop("1 or more values in names(ve) do not generate 3 pieces when split by _")
}

# Pivot each data frame to longer version
ve <- ve |>
  lapply(function(x) {
    pivot_longer(
      x,
      cols = starts_with("ve_"),
      names_sep = "_",
      names_prefix = "ve_",
      names_to = c("population", "variable"),
      values_to = "value",
      values_drop_na = TRUE
    )
  }) %>%
  bind_rows()

ve <- ve |>
  pivot_wider(
    names_from = variable,
    values_from = value
  )

# Rename columns
names(ve) <- gsub("EMstatOther", "em_stat_other", names(ve), fixed = TRUE)
names(ve) <- gsub("EMstat", "em_stat", names(ve), fixed = TRUE)
names(ve) <- gsub("EMadj", "em_adj", names(ve), fixed = TRUE)
names(ve) <- gsub("EMCI", "em_ci", names(ve), fixed = TRUE)
names(ve) <- gsub("EM", "em", names(ve), fixed = TRUE)
names(ve) <- gsub("UnvaxTotal", "n_unvaccinated_total", names(ve), fixed = TRUE)
names(ve) <- gsub("UnvaxWithOutcome", "n_unvaccinated_with_outcome", names(ve), fixed = TRUE)
names(ve) <- gsub("VaxTotal", "n_vaccinated_total", names(ve), fixed = TRUE)
names(ve) <- gsub("VaxWithOutcome", "n_vaccinated_with_outcome", names(ve), fixed = TRUE)

# Clean data --------------------------------------------------------------

# Convert columns to their proper type (must run this line before subsequent steps)
ve <- utils::type.convert(ve, as.is = TRUE)

# For studies that report "overall" data, enter populations included into "pop" column
#f <- function(x) if (is.na(x)) NULL else x
ve <- ve |>
  rowwise() |>
  mutate(
    pops_in_study = paste("infant"[infant], "child"[child], "adult"[adult], "elder"[elder], "pregnant"[preg], "immunocomp"[immunocomp], "", sep = "/")
  ) |>
  ungroup()

ve$pops_in_study <- gsub("NA", "", ve$pops_in_study)
ve$pops_in_study <- gsub("[/]+", "/", ve$pops_in_study)
ve$pops_in_study <- gsub("^/|/$", "", ve$pops_in_study)
ve$pops_in_study[ve$pops_in_study == ""] <- NA_character_

ve <- ve |>
  mutate(
    is_pop_overall = ifelse(population == "overall", 1L, 0L),
    is_pop_other = ifelse(population == "other", 1L, 0L),
    population = case_when(
      #is_pop_overall & !is.na(pops_in_study) ~ paste0("Overall (", pops_in_study, ")"),
      is_pop_overall & !is.na(pops_in_study) ~ pops_in_study,
      is_pop_other & !is.na(population_other) ~ population_other,
      .default = population
    ),
    population = case_when(
      population == "preg" ~ "Pregnant",
      population %in% c("child", "Children 6 mo - 6 y") ~ "Child",
      population %in% c("Infants birth through 12 months") ~ "Infant/child",
      population %in% c("Adults 60+", "Adults 80+") ~ "Elder",
      population %in% c("Adults 18+", "All adults 18+", "All non-immunocompromised adults 18+") ~ "Adult/elder",
      population %in% c("Age 1-64 years", "0-64 years", "Infant/child/adult/elder") ~ "All ages",
      population_other %in% c("Adults >=18 years", ">=18 years") ~ "Adult/elder",
      population_other %in% c("Adults 60+", "Adults >=60 years", ">=60 years") ~ "Elder",
      .default = population
    ),
    article = paste(author, pubyear),
    study_period = case_when(
      is.na(date_start_year) ~ as.character(date_end_year),
      is.na(date_end_year) ~ as.character(date_start_year),
      .default = paste(date_start_year, date_end_year, sep = "-")
    ),
    vax = vax_type
  ) |>
  separate_wider_delim(
    cols = vax,
    delim = " - ",
    names = c("virus", "vax_product"),
    too_few = "align_start"
  )

substring(ve$population, 1L, 1L) <- toupper(substring(ve$population, 1L, 1L))

ve <- ve |>
  mutate(
    #population = case_when(
    #  population == "Overall (child)" ~ "Child",
    #  population == "Overall (infant)" ~ "Infant",
    #  population %in% c("Overall (infant, child)") ~ "Infant/child",
    #  population %in% c("Overall (elder)") ~ "Elder",
    #  population %in% c("Overall (adult/elder)") ~ "Adult/elder",
    #  .default = population
    #),
    across(where(is.character), function(x) ifelse(x == "other", "Other", x)),
    virus = case_when(
      virus == "Multiple" & covid == 1 & rsv == 0 & flu == 0 ~ "COVID",
      virus == "Multiple" & covid == 0 & rsv == 1 & flu == 0 ~ "RSV",
      virus == "Multiple" & covid == 0 & rsv == 0 & flu == 1 ~ "Influenza",
      .default = virus
    ),
    vax_product = case_when(
      virus == "Influenza" & vax_other %in% c("Surface antigen, MF59-adjuvanted, Fluad Tetra (®Seqirus)", "Surface antigen, Flucelvax Tetra (®Seqirus)", "Adjuvanated QIV (inactived): Fluad Tetra", "High dose inactivated QIV (Efluelda Tetra)") ~ "IIV",
      virus != "Multiple" & !is.na(vax_product) & !vax_product %in% c("Other", "Multiple") ~ vax_product,
      vax_product == "Other" & virus == "Influenza" ~ "Influenza - other",
      virus == "COVID" & vax_product %in% c("Comirnaty, Spikevax or Nuvaxovid") ~ "COVID - other",
      virus == "COVID" & tolower(vax_other) %in% c("comirnarty or spikevax", "pfizer, moderna", "bnt162b2 and mrna-1273 xbb1.5 formulations", "239 539 pfizer, 347 598 moderna vaccine recipients", "bivalent BA.4/5 (pfizer or moderna not specified)", "xbb.1.5 monovalent (pfizer or moderna not specified)", "monovalent xbb.1.5 (pfizer or moderna not specified)", "pfizer/moderna bivalent booster 8-120 days ago") ~ "COVID - mRNA vaccines",
      is.na(vax_product) ~ "Other",
      .default = vax_product
    )
  )

# New line added 8/15/25. Per Caitlin. RSV studies administered in pregnancy should be classified as population == "Pregnant", even if outcomes are infant-specific. Except Nirsevimab which should stay with peds
ve <- ve |>
  mutate(
    population = case_when(
      virus == "RSV" & preg == 1 & vax_product != "Nirsevimab" ~ "Pregnant",
      virus == "RSV" & population == "Pregnant" & vax_product == "Nirsevimab" ~ "Infant",
      .default = population
    )
  )

ve <- ve |>
  filter(
    !(population %in% c("Infant", "Child", "Infant/child", "Pregnant") & vax_product == "Arexvy")
  )

# Reorder columns ---------------------------------------------------------

ve <- ve |>
  select(
    # Key study (article) identifiers
    id_redcap, id_covidence, reviewer, article, study_period,
    # Highly relevant columns
    virus, vax_product, vax_type, vax_other,
    population,
    study_design, study_setting,
    comparator, comparator_vax,
    outcome, outcome_other, outcome_text, timeline,
    # Raw data
    n_vaccinated_total, n_vaccinated_with_outcome, n_unvaccinated_total, n_unvaccinated_with_outcome,
    em, em_ci, em_adj, em_stat, em_stat_other,
    # Other columns
    maternalvax,
    is_pop_overall, is_pop_other,
    ## Study specific
    author, pubyear, title, date_start_month, date_start_year, date_end_month, date_end_year, date_notes,
    ## Status
    #exclude, vax_complete, comments, comments_complete, second_review_yn, second_review_comment,
    #gen_info_status_complete
    ## Population specific
    pops_in_study,
    infant, child, adult, elder, preg, immunocomp, population_other, perc_premature,
    sot, heme_malig, hiv_controlled, hiv_uncontrolled, solid_tumor, autoimmune, incl_immunocomp_other, undefined_immunocomp, immunocomp_other, immunocomp_def,
    population_disagg, population_comment,
    ## Other
    everything(),
    -any_of(c("n_vaccines_studied", "second_review_yn", "exclude"))
  )

# Incorporate reclassified "VE other outcome"
z <- readxl::read_excel(system.file("data-raw", "ve_outcome_lookup.xlsx", package = "VIP")) |>
  select(outcome_other, outcome_text, Classification)

ve <- ve |>
  left_join(z, by = c("outcome_other", "outcome_text")) |>
  mutate(
    outcome = ifelse(!is.na(Classification), Classification, outcome),
    outcome = ifelse(outcome == "Hospitalization and/or death", "Hospitalization", outcome),
    population = case_when(
      id_redcap %in% c(297, 494) ~ paste0(population, "/immunocomp"),
      id_redcap %in% c(2, 5, 12, 14) ~ "Pregnant",
      .default = population
    )
  ) |>
  select(-Classification)

# Convert "NOTES" in column names to "_notes"
z <- gsub("NOTES", "_notes", names(ve), fixed = TRUE)
z <- gsub("[_]+", "_", z)
if (anyDuplicated(z)) {
  stop(sprintf("The following columns in ve already exist: %s", paste(z[duplicated(z)], collapse = ", ")))
}
names(ve) <- z

# Place "notes" columns immediately after their parent column
z <- grepv("_notes$", names(ve))
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
  ve <- move_notes_col(ve, i)
}

# Clean up workspace
remove(df_main_cols, df_vax, df_vax_outcome, id_outcome, id_vax, vax_outcomes, vax_suffix, z, move_notes_col, i, idx, df_ve, transfer_if_any, comments)

# Export data -------------------------------------------------------------

usethis::use_data(ve, overwrite = TRUE)
