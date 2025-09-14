# Variable levels ---------------------------------------------------------

#' Levels for adverse event outcome
#'
#' @noRd
.ae_outcome_levels <- unique(
  c(
    # Pregnancy
    "Gestational HTN", "Pre-eclampsia/eclampsia", "Placental abruption",
    "Stillbirth", "Miscarriage",
    "Prematurity", "SGA", "Congenital defect",
    # Immune dysregulation
    "GBS", "Myocarditis",
    # Immunothrombosis
    "ITP", "CVST", "Stroke", "MI",
    #"School absenteeism",
    tryCatch(unique(ae$outcome), error = function(e) NULL)
  )
)

#' Levels for vaccine effectiveness outcomes
#'
#' @noRd
.ve_outcome_levels <- unique(
  c(
    "Long-COVID",
    "Test positivity",
    "Symptomatic infection",
    "Work absenteeism",
    "Medically-attended infection",
    "Hospitalization",
    "ICU admission",
    "Death",
    tryCatch(setdiff(unique(ve$outcome), c("Other", "Other- non-RSV LRTIs", "Other- composite of severe, critical, and death")), error = function(e) NULL)
  )
)

#' Levels for vaccines
#'
#' @noRd
.vax_levels <- unique(
  c(
    # COVID
    "BNT162b2",
    "BNT162b2_XBB.1.5",
    "mRNA-1273",
    "mRNA-1273_XBB.1.5",
    "COVID - mRNA vaccines",
    "NVX-CoV2373",
    "Ad26.COV2.S",
    # RSV
    "Abrysvo",
    "Arexvy",
    "mRNA-1345",
    "Nirsevimab",
    # Influenza
    "IIV",
    "LAIV",
    "Influenza - other",
    tryCatch(setdiff(c(ve$vax_product, ae$vax_product), c("Other", "Multiple")), error = function(e) NULL)
  )
)

#' Levels for populations
#'
#' @noRd
.population_levels <- unique(
  c(
    "Pregnant",
    "Infant",
    "Child",
    "Infant/child",
    "Adult",
    "Adult/elder",
    "Immunocomp",
    tryCatch(grepv("immunocomp", unique(c(ve$population, ae$population, epi$population)), ignore.case = TRUE), error = function(e) NULL),
    tryCatch(grepv("[0-9]", grepv("pregnant|\\binfant|\\bchild\\b|\\badult\\b|\\belder\\b", c(ve$population, ae$population, epi$population), ignore.case = TRUE), invert = TRUE), error = function(e) NULL)
  )
)

#' Levels for viruses
#'
#' @noRd
.virus_levels <- c("COVID", "RSV", "Influenza")

#' Colors for viruses
#'
#' @noRd
.virus_colors <- c(COVID = "#619150", RSV = "#366895", Influenza = "#F1C232")

# Helper functions --------------------------------------------------------

#' Guess variable type from inputs
#'
#' @param x Vector
#' @param threshold Minimum percentage of non-missing values in `x` that must match expected levels. Enter as length 1 numeric 0-1. Default is `0.6`
#' @param ignore_case If `TRUE` (default), case of `x` and expected levels will be ignored
#' @param ties_method Method for handling ties. Options: `"none"` (default. If ties present, output is `NA_character_`), `"shortest"` (variable containing fewest levels is returned), `"longest"` (variable containing most levels is returned), `"population"`, `"virus"`, `"vax_product"`, `"ve_outcome"`, `"ae_outcome"`, `"outcome"` (either ve_outcome or ae_outcome)
#' @returns Length 1 character vector containing likely variable type. If no matches found, output is `NA_character_`
#' @noRd
.guess_var <- function(x, threshold = 0.6, ignore_case = TRUE, ties_method = c("none", "shortest", "longest", "population", "virus", "vax_product", "ve_outcome", "ae_outcome", "outcome")) {
  x <- unique(x)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0L) return(NA_character_)
  levels <- list(
    population = .population_levels,
    virus = .virus_levels,
    vax_product = .vax_levels,
    ve_outcome = .ve_outcome_levels,
    ae_outcome = .ae_outcome_levels
  )
  levels <- lapply(levels, function(z) z[!is.na(z)])
  if (ignore_case) {
    x <- tolower(x)
    levels <- lapply(levels, tolower)
  }
  out <- vapply(levels, function(z) sum(match(x, z, nomatch = 0L) > 0L, na.rm = TRUE), integer(1))/n
  idx <- out >= threshold
  if (!any(idx)) return(NA_character_)
  out <- out[idx]
  out <- names(out)[out == max(out)]
  if (length(out) == 1L) return(out)
  ties_method <- match.arg(ties_method, choices = c("none", "shortest", "longest", "population", "virus", "vax_product", "ve_outcome", "ae_outcome", "outcome"))
  if (ties_method == "none") {
    NA_character_
  } else if (ties_method %in% c("shortest", "longest")) {
    out <- lengths(levels[out])
    out <- if (ties_method == "shortest") which.min(out) else which.max(out)
    names(out)
  } else if (ties_method == "outcome") {
    idx <- match(out, c("ve_outcome", "ae_outcome"), nomatch = 0L) > 0L
    if (any(idx)) {
      out[idx][1L]
    } else {
      NA_character_
    }
  } else if (any(idx <- out == ties_method)) {
    out[idx]
  } else {
    NA_character_
  }
}

#' Create factor levels
#'
#' @param x Vector
#' @param reverse If `TRUE`, levels of output are reversed. Default is `FALSE`
#' @param droplevels If `TRUE`, unused levels are dropped. Default is `FALSE`
#' @param guess_var If `TRUE` (default), variable type will be inferred using `.guess_var(x)`. If `FALSE`, factor levels of `x` will be alphabetical. Only relevant when `x` is a character vector
#' @param strict If `FALSE` (default), unique values in `x` not matching acceptable levels of inferred variable will be included in output. If `TRUE`, unique values in `x` not matching acceptable levels of inferred variable will be set to `NA`. Only relevant when `x` is a character vector and `!is.na(.guess_var(x))` is `TRUE`
#' @param ... Arguments passed to `.guess_var`
#' @returns Character vector containing factor levels
#' @noRd
.create_fct_levels <- function(x, reverse = FALSE, droplevels = FALSE, guess_var = TRUE, strict = FALSE, ...) {
  levels <- if (is.factor(x)) {
    levels <- attr(x, "levels")
    if (droplevels) {
      x <- factor(x, exclude = if (anyNA(levels)) NULL else NA)
      levels <- attr(x, "levels")
    }
    levels
  } else if (is.logical(x)) {
    c(TRUE, FALSE)
  } else if (is.numeric(x)) {
    sort.int(unique(x), method = "quick")
  } else if (guess_var && !is.na(levels <- .guess_var(x, ...))) {
    levels <- switch(
      levels,
      population = .population_levels,
      virus = .virus_levels,
      vax_product = .vax_levels,
      ve_outcome = .ve_outcome_levels,
      ae_outcome = .ae_outcome_levels
    )
    if (!strict) {
      levels <- unique(c(levels, x))
    }
    levels
  } else {
    sort.int(unique(x), method = "quick")
  }
  if (reverse) {
    levels <- rev(levels)
  }
  levels
}

#' Convert vector to factor
#'
#' @param x Vector
#' @param levels Factor levels. Enter as character vector. If `NULL` (default), levels will be generated using `.create_fct_levels`
#' @param ... Arguments passed to `.create_fct_levels`
#' @returns Factor vector with same length as input
#' @noRd
.as_fct <- function(x, levels = NULL, ...) {
  if (is.factor(x) && is.null(levels)) return(x)
  if (is.numeric(x)) {
    x <- as.integer(x)
    levels <- levels %||% sort.int(unique(x))
    x <- match(x, levels)
    levels <- as.character(levels)
  } else {
    x <- as.character(x)
    levels <- if (is.null(levels)) .create_fct_levels(x, ...) else as.character(levels)
    x <- match(x, levels)
  }
  levels(x) <- levels
  class(x) <- "factor"
  x
}

#' Order factor levels according to a continuous variable
#'
#' Functionality from forcats package
#' @param .x Vector
#' @param .reorder_by Vector used to determine new order of `.x` levels. Levels of `.x` will be determined according to `.f(.reorder_by)` calculation
#' @param .f Function applied to reorder_by to determine order of `.x` levels. Default is `mean`
#' @param ... Arguments passed to `.f`
#' @param .increasing If `TRUE` (default), levels of `.x` will be in ascending order of `.f(.reorder_by)` calculation. If `FALSE`, levels of `.x` will be in descending order of `.f(.reorder_by)` calculation
#' @returns Factor vector with same length as input
#' @noRd
.fct_reorder <- function(.x, .reorder_by, .f = function(x) mean(x, na.rm = TRUE), ..., .increasing = TRUE) {
  if (!is.factor(.x)) {
    if (is.numeric(.x)) {
      .x <- as.integer(.x)
      levels <- sort.int(unique(.x))
      .x <- match(.x, levels)
      levels <- as.character(levels)
    } else {
      if (is.logical(.x)) {
        levels <- c(TRUE, FALSE)
      } else {
        .x <- as.character(.x)
        levels <- sort.int(unique(.x), method = "quick")
      }
      .x <- match(.x, levels)
    }
    levels(.x) <- levels
    class(.x) <- "factor"
  }
  x_updated <- tapply(.reorder_by, .x, .f, ...)
  x_updated <- factor(.x, levels = attr(.x, "levels")[order(x_updated, decreasing = !.increasing)], exclude = NULL)
  attributes(x_updated) <- utils::modifyList(attributes(.x), attributes(x_updated))
  x_updated
}

#' Create a new factor level for subset of values
#'
#' @param x Factor
#' @param new_level New level. Enter as length 1 character vector
#' @param idx Positions in `x` to convert to `new_level`. Enter as integer vector with same length as `x` or an integer vector of positions in `x`
#' @returns Factor vector with same length as input
#' @noRd
.fct_new_level_last <- function(x, new_level = "", idx = is.na(x)) {
  if (!is.factor(x)) {
    x <- .as_fct(x)
  }
  x_levels <- levels(x)
  if (any(x_levels == new_level)) {
    new_level <- make.unique(c(x_levels, new_level), sep = "_")
    new_level <- new_level[length(new_level)]
  }
  levels(x) <- c(x_levels, new_level)
  x[idx] <- new_level
  x
}
