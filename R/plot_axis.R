#' Scale for continuous axis
#'
#' @param axis Options: `"y"` (default) or `"x"`
#' @param scale Options: `"regular"` (default), `"scientific"`, `"log10"`, `"log"` (same as `"log10"`), `"log2"`
#' @param title Axis title
#' @param limits Minimum and maximum values in data. Must enter in order, i.e. c(lower, upper)
#' @param breaks Numeric vector or function specifying location of ticks along axis
#' @param labels Vector or function specifying axis tick labels
#' @param limits Numeric vector of axis limits positions or function to generate limits when passed raw data
#' @param breaks Numeric vector of axis break positions or function to generate breaks when passed axis limits
#' @param expand_lower,expand_upper Expansion to add to ends of axis. Default for `expand_lower` is `0.05` if `expand_method` is `"mult` and `0.6` if `expand_method` is `add`. Default for `expand_upper` is `0`
#' @param expand Alias for `c(expand_lower,expand_upper)`
#' @param expand_method Method for expanding limits of axes. Options: `"mult"` (default. Proportion of entire axis range to add to axis ends), `"add"` (add `expand_upper` value to upper end and subtract `expand_lower` from lower end of axis)
#' @param cap Input passed to `ggplot2::guide_axis`
#' @param position Location of axis. Options: `"left"` (default for y axis), `"right"`, "bottom" (default for x axis), "top"
#' @param transform Function to transform raw data. Enter as function or string containing name of function
#' @param include Value to include in axis. Only relevant when `limits = NULL`
#' @param ... Arguments passed to scale function
#' @returns ggproto object
#' @export
continuous_axis <- function(
    axis = "y",
    scale = "regular",
    title = ggplot2::waiver(),
    limits = NULL,
    breaks = NULL,
    labels = NULL,
    expand_lower = NULL,
    expand_upper = 0,
    expand = c(expand_lower, expand_upper),
    expand_method = c("mult", "add"),
    cap = "upper",
    position = NULL,
    transform = NULL,
    include = NULL,
    ...) {
  scale <- match.arg(scale, choices = c("regular", "scientific", "log10", "log10_1", "log", "log2"))
  is_log <- startsWith(scale, "log")
  position <- position %||% if (axis == "y") "left" else "bottom"
  axis_fn <- sprintf("scale_%s_continuous", axis)

  # Log vs. linear scale
  if (is_log) {
    base <- if (scale == "log2") 2 else 10
    breaks <- breaks %||% breaks_log(base = base)
    if (missing(labels)) {
      labels <- labels %||% scales::label_log(base = base)
    } else if (inherits(labels, "waiver")) {
      labels <- scales::label_log(base = base)
    }
    transform <- transform %||% scales::transform_log(base = base)
  } else {
    breaks <- breaks %||% breaks_linear()
    if (missing(labels)) {
      labels <- if (scale == "regular") axis_label_numeric else axis_label_x10
    }
    transform <- transform %||% scales::transform_identity()
  }

  # Limits
  if (is.null(limits)) {
    if (!is.null(include) && is.function(breaks)) {
      breaks_fn <- breaks
      breaks <- function(x) breaks_fn(c(x, include))
    }
    if (is.function(breaks)) {
      limits <- function(x) range(x, breaks(x), na.rm = TRUE)
    } else if (is.numeric(breaks)) {
      limits <- function(x) range(x, include, breaks, na.rm = TRUE)
    }
  }

  # Expand
  expand_method <- match.arg(expand_method, choices = c("mult", "add"))
  mult_expand <- expand_method == "mult"
  if (missing(expand) && is.null(expand_lower)) {
    expand_lower <- if (mult_expand) 0.05 else 0.6
  }
  expand <- if (max(expand) > 0) {
    expand <- rep_len(expand, length.out = 2L)
    if (mult_expand) {
      c(expand[1L], 0, expand[2L], 0)
    } else {
      c(0, expand[1L], 0, expand[2L])
    }
  } else {
    c(0, 0, 0, 0)
  }

  # All arguments
  args <- list(
    name = title,
    limits = limits,
    breaks = breaks,
    labels = labels,
    transform = transform,
    oob = function(x, ...) x,
    position = position,
    expand = expand,
    ...
  )
  if (!any(names(args) == "guide")) {
    if (missing(cap)) {
      cap_upper <- max(expand[c(3L, 4L)]) == 0
      cap_lower <- max(expand[c(1L, 2L)]) == 0
      cap <- if (cap_upper) {
        if (cap_lower) "both" else "upper"
      } else if (cap_lower) {
        "lower"
      } else {
        "none"
      }
    }
    args$guide <- ggplot2::guide_axis(cap = cap)
  }

  # Output
  do.call(axis_fn, args)
}

#' Create axis breaks for continuous variable
#'
#' @param n Desired number of axis breaks. Enter as length 1 numeric. Default is `4`
#' @param breaks_fn Function to generate breaks. Default is `pretty.` Alternative is `.extended`
#' @param min_breaks Minimum number of possible axis breaks. Enter as length 1 numeric. Default is `4`
#' @param max_breaks Maximum number of breaks allowed. Enter as length 1 numeric. Default is `6`
#' @param include_negative If `FALSE` (default), negative numbers are excluded from breaks
#' @returns Function. Enter as `scale_y_continuous(breaks = breaks_linear())`
#' @noRd
breaks_linear <- function(
    n = 4,
    breaks_fn = pretty,
    min_breaks = 4,
    max_breaks = 6,
    include_negative = TRUE) {
  list(min_breaks, max_breaks, include_negative)
  n_default <- n
  .breaks <- if (any(c("n", "...") %in% names(formals(breaks_fn)))) {
    breaks_fn
  } else {
    function(x, ...) breaks_fn(x)
  }
  function(x, n = n_default) {
    limits <- suppressWarnings(range(x[is.finite(x)]))
    #if (length(limits) == 0L) return(numeric())
    out <- .breaks(limits, n = n)
    if (!include_negative) {
      out <- out[out >= 0]
    }
    n_breaks <- length(out)
    if (n_breaks > max_breaks && n_breaks %% 2 != 0) {
      out <- out[seq.int(from = 1L, to = n_breaks, by = 2L)]
    }
    if (n_breaks == 3 && min_breaks > 3) {
      out <- seq.int(from = out[1L], to = out[3L], by = (out[3L] - out[1L])/4)
    }
    out
  }
}

#' Calculate break positions
#'
#' Functionality from `extended` function in labeling package
#' @param x Limits or raw data values
#' @param n Desired number of axis breaks. Default is `5`
#' @param expanded If `TRUE` (default), first breaks is less than `min(x)` and last breaks is greater than `max(x)`
#' @param nice Numeric vector of "nice" numbers
#' @returns Numeric vector of break positions
#' @noRd
.extended <- function(x, n = 5, expanded = TRUE, nice = c(1, 5, 2, 2.5, 4, 3)) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(numeric())
  eps <- 2.220446e-14
  rng <- range(x)
  l <- rng[1L]
  u <- rng[2L]
  rng <- u - l
  rng_sq <- rng*rng*0.01
  best <- list(score = -2)
  j <- 1
  h <- length(nice)
  hm1 <- h - 1
  idx <- seq_len(h)
  idx_m1 <- idx - 1
  m <- n - 1
  not_expanded <- !expanded
  while (j < Inf) {
    for (i in idx) {
      v <- nice[i]
      q1 <- idx_m1[i]
      sm <- 2 - q1/hm1 - j
      if ((0.25*sm + 0.75) < best$score) {
        j <- Inf
        break
      }
      k <- 2
      while (k < Inf) {
        dm <- if (k >= n) {
          2 - (k - 1)/m
        } else {
          1
        }
        if ((0.25*sm + 0.5*dm + 0.25) < best$score) {
          break
        }
        z <- ceiling(log10(rng/(k + 1)/j/v))
        while (z < Inf) {
          step <- j*v*10^z
          span <- step*(k - 1)
          cm <- if (span > rng) {
            half <- (span - rng)/2
            1 - half*half/rng_sq
          } else {
            1
          }
          if ((0.25*sm + 0.2*cm + 0.5*dm + 0.05) < best$score) {
            break
          }
          min_start <- floor(u/step)*j - (k - 1)*j
          max_start <- ceiling(l/step)*j
          if (min_start > max_start) {
            z <- z + 1
            next
          }
          for (start in min_start:max_start) {
            lmin <- start*step/j
            lmax <- lmin + step*(k - 1)
            s <- as.numeric((lmin %% step < eps || step - (lmin %% step) < eps) && lmin <= 0 && lmax >= 0)
            s <- 1 - q1/hm1 - j + s
            w <- 1 - 0.5*((u - lmax)^2 + (l - lmin)^2)/rng_sq
            r <- (k - 1)/(lmax - lmin)
            rt <- m/(max(lmax, u) - min(l, lmin))
            g <- 2 - max(r/rt, rt/r)
            score <- 0.25*s + 0.2*w + 0.5*g + 0.05
            if (score > best$score && (not_expanded || (lmin <= l && lmax >= u))) {
              best <- list(lmin = lmin, lmax = lmax, lstep = step, score = score)
            }
          }
          z <- z + 1
        }
        k <- k + 1
      }
    }
    j <- j + 1
  }
  seq.int(from = best$lmin, to = best$lmax, by = best$lstep)
}

#' Calculate break positions
#'
#' @param n Desired number of axis breaks. Default is `5`
#' @param expanded If `TRUE` (default), first breaks is less than `min(x)` and last breaks is greater than `max(x)`
#' @param nice Numeric vector of "nice" numbers
#' @returns Function to generate breaks positions
#' @noRd
breaks_extended <- function(n = 5, expanded = TRUE, nice = c(1, 5, 2, 2.5, 4, 3)) {
  n_default <- n
  expanded_default <- expanded
  nice_default <- nice
  function(x, n = n_default, expanded = expanded_default, nice = nice_default) {
    .extended(x, n = n_default, expanded = expanded_default, nice = nice_default)
  }
}

#' Calculate position of log breaks
#'
#' @param base Log base. Enter as length 1 numeric. Default is `10`
#' @param n Desired number of tick marks. Enter as length 1 numeric. Default is `5`
#' @param breaks_fn Function to generate breaks. Must accept a numeric vector as input and return a numeric vector as output. Default is `.extended`
#' @param log_fn Logarithm function. Must have argument named `base`. Default is `log`
#' @returns Function that can be used to generate numeric vector of break positions on log scale
#' @noRd
breaks_log <- function(base = 10, n = 5, breaks_fn = .extended, log_fn = log) {
  Log <- function(x) log_fn(x, base = base)
  n_default <- n
  .breaks <- if (any(c("n", "...") %in% names(formals(breaks_fn)))) {
    breaks_fn
  } else {
    function(x, ...) breaks_fn(x)
  }
  function(x, n = n_default) {
    x <- x[is.finite(x)]
    limits <- suppressWarnings(range(x, na.rm = TRUE))
    log_limits <- Log(limits)
    upper <- base^ceiling(log_limits[2L])
    lower <- base^floor(log_limits[1L])
    out <- base^.breaks(Log(c(upper, lower)), n = n)
    out[Log(out) %% 1 == 0]
  }
}

#' Labeling function for continuous axis
#'
#' @param x Numeric vector
#' @param max_leading_zeroes Maximum number of leading zeroes when max < 1. Default is `3`
#' @param max_trailing_zeroes Maximum number of leading zeroes when max > 1. Default is `4`
#' @param unicode If `TRUE` (default), multiplication sign is displayed as unicode. If `FALSE`, unicode not used
#' @returns Character vector. Enter as `scale_y_continuous(labels = axis_label_numeric)`
#' @noRd
axis_label_numeric <- function(x, max_leading_zeroes = 3, max_trailing_zeroes = 4, unicode = TRUE) {
  x_max <- abs(max(x, na.rm = TRUE))
  if (x_max < 10^(-1 - max_leading_zeroes) || x_max >= 10^max_trailing_zeroes) {
    axis_label_x10(x, unicode)
  } else {
    format(x, scientific = FALSE, trim = TRUE)
  }
}

#' Create axis labels with "multiplier x 10^exp" format
#'
#' @param x Numeric vector containing position of axis breaks
#' @param unicode If `TRUE` (default), multiplication sign is displayed as unicode. If `FALSE`, unicode not used
#' @returns Expression. Enter as `scale_y_continuous(labels = axis_label_x10)`. Output includes multiplier prior to x sign
#' @noRd
axis_label_x10 <- function(x, unicode = TRUE) {
  if (unicode) {
    has_decimal <- any(grepl(".", format(x, scientific = TRUE), fixed = TRUE))
    axis_labels <- function(j) {
      if (is.na(j)) {
        return("")
      } else if (j == 0) {
        return(0)
      }
      j <- unlist(strsplit(format(j, trim = TRUE, scientific = TRUE), "e", fixed = TRUE), use.names = FALSE)
      z <- j[1L]
      if (has_decimal && !grepl(".", z, fixed = TRUE)) {
        z <- paste0(z, ".0")
      }
      bquote(.(paste(z, "\u00d7", "10"))^.(as.integer(j[2L])))
    }
    as.expression(lapply(x, axis_labels))
  } else {
    z <- format(x, trim = TRUE, scientific = TRUE)
    z <- gsub("0e\\+00", "0", z)
    z <- gsub("^(.*)e", "'\\1'e", z)
    z <- gsub("e\\+", "e", z)
    parse(text = gsub("e", "%*%10^", z))
  }
}
