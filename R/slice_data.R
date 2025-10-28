#' Subset data frame
#'
#' @noRd
slice_data <- function(
  x = NULL,
  y = NULL,
  domain = "ae",
  pop = NULL,
  virus = c("COVID", "RSV", "Influenza"),
  vax_product = NULL,
  outcome = NULL,
  df = NULL) {
  if (is.null(df)) {
    df <- switch(
      domain,
      ae = ae,
      ve = ve,
      epi = epi,
      coadmin = core
    )
  }
  if (domain == "ve") {
    df <- df[df$outcome %in% .ve_outcome_levels, , drop = FALSE]
  }
  #z <- c(
  #  "Adult", "Adult, elder", "Adult/elder", "Elder",
  #  "Infant", "Infant, child", "Child",
  #  "Infant/child",
  #  "Immunocomp", "Overall (adult, elder, immunocomp)", "Overall (child, adult, elder, immunocomp)",
  #  "Preg", "Overall (adult, preg)", "Pregnant"
  #)
  #df <- df[df$population %in% z, , drop = FALSE]
  pop_entered <- !is.null(pop)
  if (pop_entered) {
    all_pops <- unique(df$population)
    all_pops <- all_pops[!is.na(all_pops)]
    if (length(pop) == 1L && pop == "immunocomp") {
      all_pops <- grepv("immunocomp", all_pops, ignore.case = TRUE)
    } else {
      pops_split <- strsplit(tolower(all_pops), "/", fixed = TRUE)
      pop <- tolower(pop)
      all_pops <- all_pops[vapply(pops_split, function(x) all(x %in% pop), logical(1), USE.NAMES = FALSE)]
    }
    df <- df[df$population %in% all_pops, , drop = FALSE]
  }
  df <- dplyr::mutate(
      df,
      #vax_product = ifelse(vax_product == "Other" & virus == "Influenza", "Influenza - other", vax_product),
      #population = dplyr::case_when(
      #  population == "Infant" & outcome == "MI" ~ "Pregnant",
      #  outcome %in% c("SGA", "Prematurity") ~ "Pregnant",
      #  .default = population
      #),
      pop_category = dplyr::case_when(
        grepl("immunocomp", population, ignore.case = TRUE) ~ "Immunocomp",
        population %in% c("Infant", "Child", "Infant/child") ~ "Peds",
        population %in% c("Adult", "Elder", "Adult/elder") ~ "Adult",
        .default = population
      )
    )
  df <- df[!(df$pop_category == "Peds" & df$outcome %in% c("Stroke", "ITP", "CVST")), , drop = FALSE]
  #df$population <- df$pop_category
  #filter_pop_at_end <- !is.null(pop) && any(pop == "Peds")
  #pop_original <- pop
  #if (filter_pop_at_end) {
  #  pop <- NULL
  #}
  #pop <- pop %||% unique(df$population)
  #df <- df[df$population %in% pop, , drop = FALSE]
  df <- df[!is.na(df[[x]]) & !is.na(df[[y]]), , drop = FALSE]
  if (any(names(df) == "virus")) {
    virus <- virus %||% unique(df$virus)
    df <- df[df$virus %in% virus, , drop = FALSE]
  }
  if (any(names(df) == "vax_product")) {
    vax_product <- vax_product %||% unique(df$vax_product)
    df <- df[df$vax_product %in% vax_product, , drop = FALSE]
  }
  if (any(names(df) == "outcome")) {
    outcome <- outcome %||% unique(df$outcome)
    df <- df[df$outcome %in% outcome, , drop = FALSE]
    if (domain == "ae") {
      df$outcome <- if (y == "outcome") {
        factor(df$outcome, levels = unique(c(rev(.ae_outcome_levels), outcome)))
      } else {
        factor(df$outcome, levels = unique(c(.ae_outcome_levels, outcome)))
      }
    } else if (domain == "ve") {
      df$outcome <- if (y == "outcome") {
        factor(df$outcome, levels = unique(c(rev(.ve_outcome_levels), outcome)))
      } else {
        factor(df$outcome, levels = unique(c(.ve_outcome_levels, outcome)))
      }
    }
  }
  #df$population <- factor(df$population, levels = c("Preg", "Peds", "Adult", "Immunocomp"))
  df$virus <- factor(df$virus, levels = .virus_levels)
  vax_levels <- unique(c(.vax_levels, df$vax_product))
  df$vax_product <- factor(df$vax_product, levels = vax_levels)
  df <- dplyr::distinct(df, .data$id_redcap, .data[[x]], .data[[y]], .keep_all = TRUE)
  df
}
