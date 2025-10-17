utils::globalVariables(c("ae", "ve", "epi", "domain", "core", "fill", "population"))

#' "Not in" operator
#'
#' @noRd
`%!in%` <- function(lhs, rhs) !lhs %in% rhs

#' Convert list of vector inputs to data frame
#'
#' @noRd
vec2df <- function(..., .col_names = NULL, .prefix = "V") {
  x <- if (is.list(x <- c(...))) x else  list(...)
  z <- lengths(x, use.names = FALSE)
  if (length(z) == 0L) return(structure(list(), class = "data.frame", row.names = integer(), names = character()))
  zero_idx <- z == 0
  if (any(zero_idx)) {
    x[zero_idx] <- NULL
    z <- z[!zero_idx]
  }
  n <- max(z)
  idx <- z != n
  if (any(idx)) {
    for (i in which(idx)) {
      vals <- .subset2(x, i)
      x[[i]] <- rep_len(vals, length.out = n)
    }
  }
  if (is.null(.col_names) || length(.col_names) != length(x)) {
    .col_names <- names(x)
    if (is.null(.col_names)) {
      names(x) <- paste0(.prefix, seq_along(x))
    } else {
      idx <- !nzchar(.col_names)
      names(x)[idx] <- paste0(.prefix, seq_len(sum(idx)))
    }
  }
  names(x) <- .col_names
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -n)
  x
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

#' Round up
#'
#' @noRd
round_up <- function(x, digits = 2) {
  m <- 10^digits
  z <- abs(x)*m + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  sign(x)*z/m
}

#' Format P values
#'
#' @noRd
.format_p_value <- function(p, prefix = "P", show_leading_0 = TRUE, min_digits = 2) {
  label <- as.character(findInterval(as.numeric(p), vec = c(0, 0.001, 0.01, 0.045, 0.06, 1), all.inside = TRUE, left.open = TRUE, rightmost.closed = TRUE, checkSorted = FALSE))
  f <- function(x, n) paste(prefix, "=", sprintf(paste0("%.", n, "f"), round_up(x, n)))
  label[label == "1"] <- paste(prefix, "< 0.001")
  label[label == "2" | label == "4"] <- f(p, 3)[label == "2" | label == "4"]
  label[label == "3"] <- f(p, 2)[label == "3"]
  label[label == "5"] <- f(pmin(p, 0.99), min_digits)[label == "5"]
  if (is.null(prefix) || !nzchar(prefix)) {
    label[grepl("= ", label, fixed = TRUE)] <- gsub("= ", "", label, fixed = TRUE)
  }
  if (!show_leading_0) {
    label <- gsub("0.", ".", label, fixed = TRUE)
  }
  label
}

#' Format number and range
#'
#' @param estimate,lower,upper Numeric vectors (should have same length/order) for estimate, lower bound of CI, and upper bound of CI. If length 1, will be recycled for all values
#' @param digits Number of digits to display after decimal point. Default is `2`
#' @param sep Separator character between `lower` and `upper.` Default is `"-"`
#' @param bracket_lower,bracket_upper Brackets for lower bound and upper bound of CI, respectively. Default is `bracket_lower = "("` and `bracket_upper = ")"`
#' @param found_fn Function to round `estimate`, `lower`, and `upper`. Must take numeric vector as input and argument named digits (length 1 numeric indicating desired number of digits after decimal place for output). Default is `round_up`
#' @param na_value Output when `estimate`, `lower`, or `upper` is NA. Default is `NA_character_`
#' @returns Character vector with same length as input
#' @noRd
.format_num_range <- function(estimate, lower, upper, digits = 2, sep = "-", bracket_lower = "(", bracket_upper = ")", round_fn = round_up, na_value = NA_character_) {
  if (!is.null(round_fn)) {
    estimate <- round_fn(estimate, digits = digits)
    lower <- round_fn(lower, digits = digits)
    upper <- round_fn(upper, digits = digits)
  }
  z <- lengths(list(estimate, lower, upper), use.names = FALSE)
  if (any(z != 1L & z != max(z))) {
    stop("In '.format_num_range', inputs to 'estimate', 'lower', and 'upper' must share the same length (or length 1)")
  }
  idx_na <- is.na(estimate) | is.na(lower) | is.na(upper)
  out <- sprintf("%.*f %s%.*f%s%.*f%s", digits, estimate, bracket_lower, digits, lower, sep, digits, upper, bracket_upper)
  out[idx_na] <- na_value
  out
}

#' Format text for heterogeneity label
#'
#' @noRd
.format_heterogeneity_label <- function(i2 = NULL, tau2 = NULL, p = NULL, sep = "*','~", tau_digits = 4, title = "'Heterogeneity:'", i2_prefix = "italic(I)^2", tau2_prefix = "\u03c4^2", p_prefix = "italic(P)", as_string = FALSE) {
  f <- function(.x, .prefix) sprintf("%s~`=`~'%s'", .prefix, .x)
  i2_entered <- !is.null(i2)
  if (i2_entered) {
    i2 <- f(round_up(i2, 1), i2_prefix)
  }
  tau2_entered <- !is.null(tau2)
  if (tau2_entered) {
    tau2 <- f(round_up(tau2, tau_digits), tau2_prefix)
  }
  if (!is.null(p)) {
    p <- .format_p_value(p)
    idx <- grepl("<", p, fixed = TRUE)
    p <- f(gsub("[^\\.0-9]", "", p), p_prefix)
    p[idx] <- gsub("`=`", "`<`", p[idx], fixed = TRUE)
  }
  if (as_string && !is.null(sep)) {
    if (!is.null(title)) {
      if (substr(title, 1L, 1L) %in% LETTERS) {
        title <- shQuote(title)
      }
      if (!endsWith(title, "~")) {
        title <- paste0(title, "~")
      }
    }
    if (i2_entered) {
      i2 <- paste0(i2, sep)
    }
    if (tau2_entered) {
      tau2 <- paste0(tau2, sep)
    }
    paste0(title, i2, tau2, p)
  } else {
    c(title, i2, tau2, p)
  }
}

#' Create heterogeneity label from meta object
#'
#' @noRd
heterogeneity_label <- function(x, sep = "*','~", tau_digits = 4, title = "'Heterogeneity:'~") {
  x <- extract_meta_info(x)
  .format_heterogeneity_label(x$I2_estimate, x$tau2_estimate, x$p_het, sep = sep, tau_digits = tau_digits, title = title)
}
