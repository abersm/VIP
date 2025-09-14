# Load packages -----------------------------------------------------------

library(dplyr)
library(tidyr)

remove_null <- function(x) x[lengths(x, use.names = FALSE) > 0L]
remove_cols_all_na <- function(.df) .df[!vapply(.df, function(x) all(is.na(x)), logical(1), USE.NAMES = FALSE)]

# Settings ----------------------------------------------------------------

# Populations
## Exclude Adult/pregnant (don't convert population to pregnant) from RedCAP 631. This is the "overall" estimate for a study that included pregnant and non-pregnant women. Study disaggregated results for each group
sort(unique(VIP::ve$population))
sort(unique(VIP::ae$population))
sort(unique(VIP::epi$population))

# Virus
stopifnot(setequal(unique(VIP::ve$virus), c("COVID", "Influenza", "RSV")))
stopifnot(setequal(unique(VIP::ae$virus), c("COVID", "Influenza", "RSV")))
stopifnot(setequal(unique(VIP::epi$virus), c("COVID", "Influenza", "RSV")))

# Function to filter data to population categories of interest, remove extra columns, and possibly split data
prep_data <- function(x, is_ve = FALSE, split_by = c("virus", "pop_category"), toc = FALSE, ve_outcome_other_tab = FALSE, pop_other_tab = FALSE) {
  sheet_other_pop <- if (pop_other_tab) "Multiple_pop" else NA_character_
  x <- if (is_ve) {
    if (ve_outcome_other_tab) {
      mutate(
        x,
        pop_category = case_when(
          outcome == "Other" ~ "Outcome_other",
          grepl("immunocomp", population, ignore.case = TRUE) ~ "Immunocomp",
          population %in% c("Infant", "Child", "Infant/child") ~ "Peds",
          population %in% c("Adult", "Elder", "Adult/elder") ~ "Adult",
          population == "Pregnant" ~ "Preg",
          .default = sheet_other_pop
        ),
        pop_category = factor(pop_category, levels = c("Preg", "Peds", "Adult", "Immunocomp", "Outcome_other", if (pop_other_tab) sheet_other_pop))
      )
    } else {
      mutate(
        x,
        pop_category = case_when(
          grepl("immunocomp", population, ignore.case = TRUE) ~ "Immunocomp",
          population %in% c("Infant", "Child", "Infant/child") ~ "Peds",
          population %in% c("Adult", "Elder", "Adult/elder") ~ "Adult",
          population == "Pregnant" ~ "Preg",
          .default = sheet_other_pop
        ),
        pop_category = factor(pop_category, levels = c("Preg", "Peds", "Adult", "Immunocomp", if (pop_other_tab) sheet_other_pop))
      )
    }
  } else {
    mutate(
      x,
      pop_category = case_when(
        grepl("immunocomp", population, ignore.case = TRUE) ~ "Immunocomp",
        population %in% c("Infant", "Child", "Infant/child") ~ "Peds",
        population %in% c("Adult", "Elder", "Adult/elder") ~ "Adult",
        population == "Pregnant" ~ "Preg",
        .default = sheet_other_pop
      ),
      pop_category = factor(pop_category, levels = c("Preg", "Peds", "Adult", "Immunocomp", if (pop_other_tab) sheet_other_pop))
    )
  }
  x <- x |>
    filter(
      virus %in% c("COVID", "RSV", "Influenza"),
      !is.na(pop_category)
    ) |>
    select(-any_of(c("bnt162b2", "bnt162b2_xbb", "mrna1273", "mrna1273_xbb", "cov2601", "novavax2601", "ad26", "covid_vax_other", "arexvy", "abrysvo", "mrna1345", "nirsevimab", "rsv_vax_other", "iiv", "laiv", "flu_vax_other", "other_ae_incl", "is_of_interest", "is_pop_other", "title", "infant", "child", "adult", "elder", "preg", "immunocomp", "covid", "rsv", "flu")))
  if (!is.null(split_by)) {
    by <- x[split_by]
    if (any(split_by == "virus")) {
      by$virus <- factor(by$virus, levels = c("COVID", "RSV", "Influenza"))
    }
    by <- if (length(split_by) == 1L) {
      by[[1L]]
    } else {
      interaction(as.list(by), sep = "_", lex.order = TRUE)
    }
    x <- split(x, by, sep = "_")
    x[grepv("Immunocomp", names(x), invert = TRUE)] <- lapply(x[grepv("Immunocomp", names(x), invert = TRUE)], function(z) select(z, -any_of(c("sot", "heme_malig", "hiv_controlled", "hiv_uncontrolled", "solid_tumor", "autoimmune", "incl_immunocomp_other", "undefined_immunocomp", "immunocomp_other", "immunocomp_def"))))
    if (ve_outcome_other_tab && is_ve) {
      x[grepv("Outcome_other", names(x), invert = TRUE)] <- lapply(x[grepv("Outcome_other", names(x), invert = TRUE)], function(z) select(z, -outcome_other))
    }
  }
  if (toc) {
    x <- lapply(x, function(z) if (nrow(z) == 0L) NULL else select(z, -any_of("pop_category")))
    x <- x[!vapply(x, is.null, logical(1), USE.NAMES = FALSE)]
    x <- c(
      TOC = list(
        data.frame(
          sheet = names(x),
          n_rows = vapply(x, nrow, integer(1), USE.NAMES = FALSE)
        )
      ),
      x
    )
  }
  x
}

# Prepare data ------------------------------------------------------------

# AE
ae_new <- VIP::ae |>
  filter(
    # Include gen_info_status_complete which might capture AE studies in which outcome was not of interest
    #comments_complete == "Complete" | gen_info_status_complete == "Complete",
    !is.na(outcome)
  ) |>
  arrange(outcome, population, vax_product, study_design) |>
  select(id_redcap, outcome, population, vax_product, study_design, everything()) |>
  prep_data(
    #pop_other_tab = TRUE
  ) |>
  lapply(remove_cols_all_na) |>
  lapply(function(x) if (nrow(x) == 0L) NULL else x) |>
  remove_null()

# VE
ve_new <- VIP::ve |>
  #filter(vax_complete == "Complete") |>
  arrange(vax_product, vax_type, population, study_design, outcome) |>
  select(id_redcap, population, vax_product, vax_type, outcome, outcome_other, outcome_text, study_design, everything()) |>
  prep_data(
    #pop_other_tab = TRUE,
    is_ve = TRUE
  ) |>
  lapply(remove_cols_all_na) |>
  lapply(function(x) if (nrow(x) == 0L) NULL else x) |>
  remove_null()

# Epi
epi_new <- VIP::epi |>
  prep_data(
    #pop_other_tab = TRUE
  ) |>
  lapply(remove_cols_all_na) |>
  lapply(function(x) if (nrow(x) == 0L) NULL else x) |>
  remove_null()

# Coadmin
coadmin_new <- VIP::coadmin |>
  select(
    -c(exclude, second_review_yn, coadmin_complete)
  ) |>
  arrange(covid, rsv, flu, pops_in_study) |>
  remove_cols_all_na()

# Export data -------------------------------------------------------------

export <- TRUE
if (exists("export") && isTRUE(export)) {
  #v <- gsub("-", "_", as.Date(floor(unclass(Sys.time())/86400)))
  v <- "V3"
  #abers::xlsx(ae_new, paste0("AE_studies_", v), freeze_col = 4)
  #abers::xlsx(ve_new, paste0("VE_studies_", v), freeze_col = 5)
  #abers::xlsx(epi_new, paste0("Epi_studies_", v), freeze_col = 5)
  #abers::xlsx(coadmin_new, paste0("Coadmin_studies_", v), freeze_col = 6)
  #v <- "v2"
 # abers::xlsx(ae_new, paste0("AE_studies_", v), freeze_col = 4)
 # abers::xlsx(ve_new, paste0("VE_studies_", v), freeze_col = 6)
 # abers::xlsx(epi_new, paste0("Epi_studies_", v), freeze_col = 5)
  remove(export, v)
}
