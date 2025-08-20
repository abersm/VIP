#' "Not in" operator
#'
#' @noRd
`%!in%` <- function(lhs, rhs) !lhs %in% rhs

#' Convert color name to hexadecimal code
#'
#' @param x Character vector of color names or hexadecimal codes
#' @returns Character vector of hexadecimal codes with same length as input
#' @noRd
col2hex <- function(x) {
  rgb <- grDevices::col2rgb(x)/255
  grDevices::rgb(red = rgb[1L, ], green = rgb[2L, ], blue = rgb[3L, ])
}

#' tryCatch that returns default value if error occurs
#'
#' @noRd
tryElse <- function(x, otherwise = NULL, silent = TRUE) {
  if (silent) {
    tryCatch(suppressWarnings(x), error = function(e) otherwise)
  } else {
    tryCatch(x, error = function(e) otherwise)
  }
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
fct_reorder <- function(.x, .reorder_by, .f = function(x) mean(x, na.rm = TRUE), ..., .increasing = TRUE) {
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
