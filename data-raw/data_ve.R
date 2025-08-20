# Make sure VIP::core is updated (if unsure, run data_core.R file in data-raw folder before running script below)

# Load packages -----------------------------------------------------------

library(tidyr)
library(dplyr)

# Create VE-specific data frame -------------------------------------------

# Remove irrelevant vars
df_ve <- VIP::core |>
  filter(ve == 1) |>
  select(-starts_with(c("ae_", "epi_", "coadmin_")), -c(ae, coadmin, epi, ae_other)) |>
  rename(n_vaccines_studied = ve_n_vaccines_studied) |>
  mutate(
    across(where(is.character), function(x) gsub("\002", "", x, fixed = TRUE))
  )

#df_ve <- remove_constant_cols(df_ve)
df_ve <- df_ve[!vapply(df_ve, function(x) all(is.na(x)) || length(unique(x)) == 1L, logical(1), USE.NAMES = FALSE)]

# Remove unused variables
df_ve <- df_ve |>
  select(-matches("ve_overall_yn|ve_outcome_n_reported"))

# Add vaccine-specific id ("v#" pattern) to variables with implied "v1"
old_names <- names(df_ve)
needs_vax_id <- grepl("^ve_", old_names) & !grepl("_v[0-9]", old_names)
names(df_ve)[needs_vax_id] <- paste0(old_names[needs_vax_id], "_v1")
remove(old_names, needs_vax_id)

# Review endings
## Note: ve_outcome_text and ve_complete variables can have v3 suffix but all other columns that end in v3 also have preceding v2 (i.e., ending is v2_v3)
setdiff(grepv("_v3", names(df_ve)), grepv("_v2_v3", names(df_ve)))

# To fix this, just remove _v2 from _v2_v3 suffix
names(df_ve) <- gsub("_v2_v3", "_v3", names(df_ve))

# Create long version of data ---------------------------------------------

# Next line needed because when chopping up data in subsequent steps, a single column could be numeric for one subset and character for another. Need to join them together later using bind_rows
z <- mutate(df_ve, across(everything(), as.character))

# df_ve has 3 categories of variables
## 1. Variables that are independent of specific vaccine/VE outcome. This are article (study) specific variables
df_main_cols <- z[!grepl("_[0-9]|_v[0-9]", names(z))]
## 2. Variables that are vaccine-specific but independent of VE outcome
df_vax <- z[grepl("ve_complete|ve_timeline|ve_comparator|ve_vax", names(z))]
## 3. Variables that are vaccine-specific and VE outcome-specific
df_vax_outcome <- z[setdiff(names(z), c(names(df_vax), names(df_main_cols)))]

# Review last category
#grepv("_[0-9]|_v[0-9]", names(df_vax_outcome), invert = TRUE)
grepv("v[0-9]", names(df_vax_outcome), invert = TRUE)

# Remove "ve_" prefix from columns in df_main_cols and df_vax to facilitate future pivoting
#grepv("ve_", names(df_main_cols))
names(df_vax) <- gsub("ve_complete", "vax_complete", names(df_vax))
names(df_vax) <- gsub("^ve_", "", names(df_vax))

# Remove "ve_" prefix from population independent columns in df_vax_outcome
names(df_vax_outcome) <- gsub("^ve_(outcome|maternalvax|em_stat)", "\\1", names(df_vax_outcome))

# Collapse future column names into after population identifier to facilitate future pivoting
names(df_vax_outcome) <- gsub("em_stat_other", "EMstatOther", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("em_stat", "EMstat", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("em_adj", "EMadj", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("em_ci", "EMCI", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("_em_", "_EM_", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("unvax_total", "UnvaxTotal", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("unvax_with_outcome", "UnvaxWithOutcome", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("vax_total", "VaxTotal", names(df_vax_outcome), fixed = TRUE)
names(df_vax_outcome) <- gsub("vax_with_outcome", "VaxWithOutcome", names(df_vax_outcome), fixed = TRUE)

# Create list of vaccine IDs for each column in df_vax and df_vax_outcome
vax_suffix <- regmatches(names(df_vax), gregexpr("_v[0-9]+", names(df_vax)))
all(lengths(vax_suffix) == 1L)
vax_suffix <- unlist(vax_suffix, use.names = FALSE)

# Create list of vaccine IDs and outcome IDs for each column in df_vax_outcome
id_vax <- regmatches(names(df_vax_outcome), gregexpr("_v[0-9]+", names(df_vax_outcome)))
all(lengths(id_vax) == 1L)
id_vax <- unlist(id_vax, use.names = FALSE)
id_outcome <- regmatches(names(df_vax_outcome), gregexpr("_[0-9]+", names(df_vax_outcome)))
id_outcome <- vapply(id_outcome, function(x) if (length(x) == 0L) "_1" else x, character(1), USE.NAMES = FALSE)

# Add outcome to column names
names(df_vax_outcome) <- paste0(
  gsub("(_[0-9]+)?(_v[0-9+])$", "", names(df_vax_outcome)),
  id_outcome,
  id_vax
)

# Loop through vax/outcome endings
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

# Make sure all columns with "ve_" prefix have 3 pieces when plot by "_"
z <- ve |>
  lapply(names) |>
  unlist(use.names = FALSE)
z <- grepv("^ve", z) |>
  strsplit("_")
all(lengths(z) == 3L)

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
  }) |>
  bind_rows() |>
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
f <- function(x) if (is.na(x)) NULL else x
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
      population %in% c("Adults 60+") ~ "Elder",
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
    exclude, vax_complete, gen_info_status_complete, comments, comments_complete, second_review_yn, second_review_comment,
    ## Population specific
    pops_in_study,
    infant, child, adult, elder, preg, immunocomp, population_other, perc_premature,
    sot, heme_malig, hiv_controlled, hiv_uncontrolled, solid_tumor, autoimmune, incl_immunocomp_other, undefined_immunocomp, immunocomp_other, immunocomp_def,
    population_disagg, population_comment,
    ## Other
    everything(),
    -c(n_vaccines_studied)
  )

# Incorporate reclassified "VE other outcome"
z <- readxl::read_excel(system.file("data-raw", "ve_outcome_other_ncm_cmd.xlsx", package = "VIP")) |>
  select(outcome_other, outcome_text, Classification)

ve <- ve |>
  left_join(z, by = c("outcome_other", "outcome_text")) |>
  mutate(
    outcome = ifelse(!is.na(Classification), Classification, outcome),
    outcome = ifelse(outcome == "Hospitalization and/or death", "Hospitalization", outcome),
    population = case_when(
      id_redcap %in% c(2, 5, 12, 14) ~ "Pregnant",
      .default = population
    )
  ) |>
  select(-Classification)

# Clean up workspace
remove(df_main_cols, df_vax, df_vax_outcome, f, id_outcome, id_vax, vax_outcomes, vax_suffix, z, df_ve)

# Export data -------------------------------------------------------------

# Store local version of cleaned VE data (including version in long format with 1 row per vaccine/VE outcome)
#usethis::use_data(df_ve, overwrite = TRUE)
usethis::use_data(ve, overwrite = TRUE)
