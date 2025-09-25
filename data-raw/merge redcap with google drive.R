# Integrate new data from RedCAP with existing data from google drive

# Load packages
library(dplyr)
library(tidyr)

# Load data from RedCAP
source(system.file("data-raw", "data_core.R", package = "VIP"))
source(system.file("data-raw", "data_ve.R", package = "VIP"))
source(system.file("data-raw", "data_ae.R", package = "VIP"))
source(system.file("data-raw", "data_epi.R", package = "VIP"))
remove(core, dict, ve, ae, epi)
pkgload::load_all(path = getwd())
devtools::document()

# Prepare data for google drive
source(system.file("data-raw", "data for google drive.R", package = "VIP"))

# Add sheet name to new data to match format of old data
ve_new <- lapply(setNames(nm = names(ve_new)), function(x) {
  ve_new[[x]]$sheet_name <- x
  ve_new[[x]]
})
ae_new <- lapply(setNames(nm = names(ae_new)), function(x) {
  ae_new[[x]]$sheet_name <- x
  ae_new[[x]]
})
epi_new <- lapply(setNames(nm = names(epi_new)), function(x) {
  epi_new[[x]]$sheet_name <- x
  epi_new[[x]]
})

# Load current data from google drive
ve_old <- read_xlsx_sheets("VE_studies_2025_08_12", merge = FALSE)
ae_old <- read_xlsx_sheets("AE_studies_2025_08_12", merge = FALSE)
epi_old <- read_xlsx_sheets("Epi_studies_2025_08_12", merge = FALSE)
ve_old$TOC <- ae_old$TOC <- epi_old$TOC <- NULL
ve_old <- ve_old[!grepl("_reorg", names(ve_old), ignore.case = TRUE)]
ae_old <- ae_old[!grepl("_reorg", names(ae_old), ignore.case = TRUE)]
epi_old <- epi_old[!grepl("_reorg", names(epi_old), ignore.case = TRUE)]

# Clean old data
## VE
ve_old <- ve_old |>
  lapply(function(x) {
    for (i in grepv("^\\.", names(x))) {
      if (all(is.na(x[[i]]))) {
        x[[i]] <- NULL
      }
    }
    x$pops_in_study <- gsub(", ", "/", x$pops_in_study)
    #substr(x$pops_in_study, 1L, 1L) <- toupper(substr(x$pops_in_study, 1L, 1L))
    names(x)[names(x) %in% c("study_overlap", "overlap", "Overlap?", "Overlap (Y/N)?")] <- "Overlap (Y/N)"
    names(x)[names(x) %in% c("Amenable to meta-analysis?", "Amenable to data issues")] <- "Amenable to meta-analysis"
    names(x)[names(x) %in% c("Type of Data")] <- "Type of data"
    names(x)[names(x) %in% c("Comments / data issues")] <- "Data comments/issues"
    names(x)[names(x) %in% c("population specified", "population specification")] <- "Population specification"
    names(x)[names(x) %in% c("Calculated VE upper bound")] <- "Calculated VE CI upper bound"
    names(x)[names(x) %in% c("EM Reported")] <- "EM reported"
    x$population[x$population == "Preg"] <- "Pregnant"
    x$population[x$population == "Preg-Infant"] <- "Pregnant/infant"
    x$population[x$population == "Overall (child, adult, elder, immunocomp)"] <- "Child/adult/elder/immunocomp"
    x$population[x$population == "Overall (adult, elder, immunocomp)"] <- "Adult/elder/immunocomp"
    for (i in intersect(names(x), c("Overlap (Y/N)", "Amenable to meta-analysis", "Type of data", "Data comments/issues", "Calculated VE CI lower bound", "Calculated VE CI upper bound", "EM reported"))) {
      x[[i]] <- as.character(x[[i]])
    }
    if (any(names(x) == "id_redcap")) {
      x <- x[!is.na(x$id_redcap), , drop = FALSE]
      x <- x |>
        group_by(id_redcap, id_covidence, reviewer, study_period, study_setting, virus, article, population, population_comment, population_other, population_disagg, vax_product, vax_type, vax_other, comparator, comparator_vax, outcome_text, study_design, outcome, timeline) |>
        mutate(is_pop_overall = 1L:dplyr::n(), is_pop_overall = is_pop_overall - 1L) |>
        ungroup()
    }
    x
  })

## AE
ae_old <- ae_old |>
  lapply(function(x) {
    for (i in grepv("^\\.", names(x))) {
      if (all(is.na(x[[i]]))) {
        x[[i]] <- NULL
      }
    }
    x <- x[grepv("exclude\\.\\.\\.", names(x), invert = TRUE)]
    x$pops_in_study <- gsub(", ", "/", x$pops_in_study)
    #substr(x$pops_in_study, 1L, 1L) <- toupper(substr(x$pops_in_study, 1L, 1L))
    names(x)[names(x) %in% c("study_overlap", "overlap", "Overlap?", "Overlap (Y/N)?")] <- "Overlap (Y/N)"
    names(x)[names(x) %in% c("Amenable to meta-analysis?", "Amenable to data issues", "Amenable to meta analysis")] <- "Amenable to meta-analysis"
    names(x)[names(x) %in% c("Type of Data", "type of data")] <- "Type of data"
    names(x)[names(x) %in% c("Comments / data issues", "Comments/data issues")] <- "Data comments/issues"
    names(x)[names(x) %in% c("population specified", "population specification")] <- "Population specification"
    names(x)[names(x) %in% c("EM Reported")] <- "EM reported"
    x$population[x$population == "Preg"] <- "Pregnant"
    x$population[x$population == "Preg-Infant"] <- "Pregnant/infant"
    x$population[x$population == "Overall (child, adult, elder, immunocomp)"] <- "Child/adult/elder/immunocomp"
    x$population[x$population == "Overall (adult, elder, immunocomp)"] <- "Adult/elder/immunocomp"
    for (i in intersect(names(x), c("Overlap (Y/N)", "Amenable to meta-analysis", "Type of data", "Data comments/issues", "EM reported"))) {
      x[[i]] <- as.character(x[[i]])
    }
    if (any(names(x) == "id_redcap")) {
      x <- x[!is.na(x$id_redcap), , drop = FALSE]
    }
    x
  })

## Epi
epi_old <- epi_old |>
  lapply(function(x) {
    for (i in grepv("^\\.", names(x))) {
      if (all(is.na(x[[i]]))) {
        x[[i]] <- NULL
      }
    }
    x$pops_in_study <- gsub(", ", "/", x$pops_in_study)
    #substr(x$pops_in_study, 1L, 1L) <- toupper(substr(x$pops_in_study, 1L, 1L))
    x$population[x$population == "Preg"] <- "Pregnant"
    x$population[x$population == "Preg-Infant"] <- "Pregnant/infant"
    x$population[x$population == "Overall (child, adult, elder, immunocomp)"] <- "Child/adult/elder/immunocomp"
    x$population[x$population == "Overall (adult, elder, immunocomp)"] <- "Adult/elder/immunocomp"
    if (any(names(x) == "id_redcap")) {
      x <- x[!is.na(x$id_redcap), , drop = FALSE]
    }
    x
  })

# Separate complete from unverified/incomplete
## VE
ve_old_complete <- ve_old |>
  bind_rows() |>
  filter(
    vax_complete == "Complete"
  ) |>
  mutate(
    population = case_when(
      id_redcap %in% c(2, 5, 12, 14) ~ "Pregnant",
      virus == "RSV" & grepl("preg", pops_in_study, ignore.case = TRUE) & vax_product != "Nirsevimab" ~ "Pregnant",
      virus == "RSV" & population == "Pregnant" & vax_product == "Nirsevimab" ~ "Infant",
      .default = population
    ),
    outcome = ifelse(outcome == "Hospitalization and/or death", "Hospitalization", outcome),
    vax_product = case_when(
      virus == "Influenza" & vax_other %in% c("Surface antigen, MF59-adjuvanted, Fluad Tetra (®Seqirus)", "Surface antigen, Flucelvax Tetra (®Seqirus)", "Adjuvanated QIV (inactived): Fluad Tetra", "High dose inactivated QIV (Efluelda Tetra)") ~ "IIV",
      vax_type == "RSV - other" & vax_other == "Ad26.RSV.preF" ~ "Abrysvo",
      virus != "Multiple" & !is.na(vax_product) & vax_product %!in% c("Other", "Multiple") ~ vax_product,
      vax_product == "Other" & virus == "Influenza" ~ "Influenza - other",
      virus == "COVID" & vax_product %in% c("Comirnaty, Spikevax or Nuvaxovid") ~ "COVID - other",
      virus == "COVID" & tolower(vax_other) %in% c("comirnarty or spikevax", "pfizer, moderna", "bnt162b2 and mrna-1273 xbb1.5 formulations", "239 539 pfizer, 347 598 moderna vaccine recipients", "bivalent BA.4/5 (pfizer or moderna not specified)", "xbb.1.5 monovalent (pfizer or moderna not specified)", "monovalent xbb.1.5 (pfizer or moderna not specified)", "pfizer/moderna bivalent booster 8-120 days ago") ~ "COVID - mRNA vaccines",
      .default = vax_product
    ),
    group = case_when(
      grepl("immunocomp", population, ignore.case = TRUE) ~ "Immunocomp",
      population %in% c("Infant", "Child", "Infant/child") ~ "Peds",
      population %in% c("Adult", "Elder", "Adult/elder") ~ "Adult",
      grepl("Preg", population, ignore.case = TRUE) ~ "Preg",
      .default = population
    ),
    group = paste(virus, group, sep = "_")
  ) |>
  filter(
    !(population %in% c("Infant", "Child", "Infant/child", "Pregnant") & vax_product == "Arexvy")
  ) |>
  select(-c(vax_type))

ids <- unique(ve_old_complete$id_redcap)

ve_out <- ve_old_complete |>
  select(-c(sheet_name)) |>
  rename(sheet_name = group) |>
  bind_rows(
    filter(bind_rows(ve_new[-1L]), id_redcap %!in% ids)
  ) |>
  arrange(vax_product, population, outcome) |>
  split_df("sheet_name")

ve_out <- ve_out[unique(c("COVID_Peds", "COVID_Adult", "COVID_Immunocomp", "RSV_Preg", "RSV_Peds", "RSV_Adult", "Influenza_Preg", "Influenza_Peds", "Influenza_Adult", names(ve_out)))]

#xlsx(ve_out, file_name = "VE_studies_2025_08_16", freeze_col = 5L)

#ve_old_incomplete <- ve_old |>
#  bind_rows() |>
#  filter(
#    is.na(vax_complete) | vax_complete != "Complete"
#  )

## AE
ae_old_complete <- ae_old |>
  bind_rows() |>
  filter(
    comments_complete == "Complete"
  ) |>
  mutate(
    population = case_when(
      grepl("preg", pops_in_study, ignore.case = TRUE) & outcome == "MI" ~ "Pregnant",
      virus == "RSV" & population == "Pregnant" & vax_product == "Nirsevimab" ~ "Infant",
      outcome %in% c("Placental abruption", "Pre-eclampsia/eclampsia", "Gestational HTN", "Stillbirth", "Congenital defect", "SGA", "Prematurity") ~ "Pregnant",
      .default = population
    ),
    vax_product = case_when(
      vax_product == "Other" & vax_type_notes %in% c("Influenza - type not specified") ~ "Influenza - other",
      vax_product == "Other" & vax_type_notes %in% c("BNT162b2 BA.4.5 vaccine") ~ "BNT162b2",
      vax_product == "Other" & vax_type_notes %in% c("COVID - mRNA vaccines") ~ vax_type_notes,
      vax_product == "Other" & virus == "Influenza" ~ "Influenza - other",
      .default = vax_product
    ),
    group = case_when(
      grepl("immunocomp", population, ignore.case = TRUE) ~ "Immunocomp",
      population %in% c("Infant", "Child", "Infant/child") ~ "Peds",
      population %in% c("Adult", "Elder", "Adult/elder") ~ "Adult",
      grepl("Preg", population, ignore.case = TRUE) ~ "Preg",
      .default = population
    ),
    group = paste(virus, group, sep = "_")
  ) |>
  filter(
    !(population %in% c("Infant", "Child", "Infant/child", "Pregnant") & vax_product == "Arexvy")
  ) |>
  select(-c(vax_type))

ids <- unique(ae_old_complete$id_redcap)

ae_out <- ae_old_complete |>
  select(-c(sheet_name)) |>
  rename(sheet_name = group) |>
  bind_rows(
    filter(bind_rows(ae_new[-1L]), id_redcap %!in% ids)
  ) |>
  arrange(outcome, population, vax_product) |>
  split_df("sheet_name")

ae_out <- ae_out[unique(c("COVID_Peds", "COVID_Adult", "COVID_Immunocomp", "COVID_Preg", "RSV_Preg", "RSV_Adult", "RSV_Immunocomp", "Influenza_Preg", "Influenza_Peds", "Influenza_Adult", names(ae_out)))]

#xlsx(ae_out, file_name = "AE_studies_2025_08_16", freeze_col = 5L)

#ae_old_incomplete <- ae_old |>
#  bind_rows() |>
#  filter(
#    is.na(vax_complete) | vax_complete != "Complete"
#  )

# QC ----------------------------------------------------------------------

if (FALSE) {
  # Compare old vs. new -----------------------------------------------------

  f <- function(old, new) {
    old$sheet_name <- new$sheet_name <- NULL
    old_names <- names(old)
    new_names <- names(new)
    col_names <- list(
      shared = intersect(old_names, new_names),
      old_only = setdiff(old_names, new_names),
      new_only = setdiff(new_names, old_names)
    )
    col_names
  }

  # Compare names of sheets
  f(ve_old, ve_new)[-1L]
  f(ae_old, ae_new)[-1L]
  #f(epi_old, epi_new)

  # Compare column names
  z <- f(ve_old, ve_new)$shared |>
    lapply(function(x) {
      out <- f(ve_old[[x]], ve_new[[x]])[-1L]
      if (length(out$old_only) == 0L && length(out$new_only) == 0L) {
        NULL
      } else {
        vec_to_df(
          sheet = x,
          old_only = if (length(out$old_only) == 0L) NA_character_ else out$old_only,
          new_only = if (length(out$new_only) == 0L) NA_character_ else out$new_only
        )
      }
    }) |>
    remove_null() |>
    bind_rows()

  z |>
    filter(
      grepl("^\\.", old_only)
    )

  # Check to make sure only complete data were edited
  g <- z |>
    drop_na(old_only) |>
    pull(sheet) |>
    unique() |>
    lapply(function(x) {
      cols <- z$old_only[z$sheet == x]
      out <- ve_old[[x]][unique(c("id_redcap", grepv("complete", names(ve_old[[x]]), ignore.case = TRUE), cols))]
      out <- filter(out, if_any(contains("complete", ignore.case = TRUE), function(i) is.na(i) | i != "Complete"))
      if (nrow(out) == 0L) return(NULL)
      idx <- apply(out[cols], 1, function(x) all(is.na(x)))
      out <- out[!idx, , drop = FALSE]
      if (nrow(out) == 0L) return(NULL)
      out$sheet <- x
      out <- out[c("sheet", setdiff(names(out), "sheet"))]
      for (i in cols) {
        out[[i]] <- as.character(out[[i]])
      }
      out
    }) |>
    bind_rows()

  # Compare values in column shared between old and new
  ## VE
  ve <- list(
    new = bind_rows(lapply(ve_new[-1L], function(x) mutate(x, across(everything(), as.character)))),
    old = bind_rows(lapply(ve_old, function(x) mutate(x, across(everything(), as.character))))
  )

  ## AE
  ae <- list(
    new = bind_rows(lapply(ae_new[-1L], function(x) mutate(x, across(everything(), as.character)))),
    old = bind_rows(lapply(ae_old, function(x) mutate(x, across(everything(), as.character))))
  )

  ## Epi
  epi <- list(
    new = bind_rows(lapply(epi_new[-1L], function(x) mutate(x, across(everything(), as.character)))),
    old = bind_rows(lapply(epi_old, function(x) mutate(x, across(everything(), as.character))))
  )

  ve$old$id <- ae$old$id <- epi$old$id <- "old"
  ve$new$id <- ae$new$id <- epi$new$id <- "new"

  # Identify differences
  ve_shared <- ve |>
    lapply(function(x) {
      pivot_longer(x[intersect(names(ve$new), names(ve$old))], cols = -c(id, sheet_name, id_redcap, id_covidence, reviewer, study_period, study_setting, virus, article, population, population_comment, population_other, population_disagg, vax_product, vax_type, vax_other, comparator, comparator_vax, outcome_text, study_design, outcome, timeline, is_pop_overall), names_to = "column", values_to = "value", values_drop_na = TRUE)
    }) |>
    bind_rows() |>
    distinct()

  g <- pivot_wider(
    ve_shared,
    names_from = id,
    values_from = value
  ) |>
    unnest(c(old, new)) |>
    mutate(
      same = ifelse(!is.na(old) & !is.na(new) & old == new, 1L, 0L)
    ) |>
    select(column, old, new, same, everything())

  g |> filter(same == 0) |> drop_na(old)

  #g <- ve$new |>
  #  group_by(population, population_other, population_comment, population_disagg, vax_product, vax_type, outcome, #outcome_other, outcome_text, study_design, id_redcap, id_covidence, reviewer, article, study_period, virus, vax_other, #study_setting, comparator, comparator_vax, timeline, is_pop_overall) |>
  #  filter(n() > 1) |>
  #  mutate(
  #    group = 1L:n()
  #  ) |>
  #  ungroup() |>
  #  select(group, everything())

  #g |> map_int(~length(unique(.x)))

  j <- ve_shared |>
    dplyr::summarise(n = dplyr::n(), .by = c(population, vax_product, vax_type, outcome, outcome_text, study_design, id_redcap, id_covidence, reviewer, article, study_period, virus, vax_other, study_setting, comparator, comparator_vax, timeline, is_pop_overall, population_other, population_disagg, population_comment, sheet_name, column, id)) |>
    dplyr::filter(n > 1L) |>
    select(n, everything()) |>
    #select(-n) |>
    left_join(ve_shared)

  j |> select(sheet_name, population, column, value)

  j$column |> unique() |> sort()

  g |>
    drop_na(old, new) |>
    filter(
      same == 0,
      id_redcap %!in% j$id_redcap,
      !grepl("^n|^em", column)
    )
}
