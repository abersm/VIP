# VE meta-analysis --------------------------------------------------------

#' Run complete meta-analysis for OR or RR
#'
#' @noRd
meta_analyze_ratio <- function(
    df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    grouping_var = NULL,
    study_label = "study_label",
    study_label_meta = "Meta-analysis",
    effect_measure = "OR",
    method = "DL",
    random_effects = TRUE,
    incl_cols = NULL,
    transform_to_log_odds = log,
    backtransform_from_log_odds = exp,
    force_lower_upper = TRUE,
    as_df = FALSE,
    ...) {
  df$is_meta <- df$id_meta <- NULL
  cols <- c(estimate, lower, upper)
  idx <- apply(df[cols], 1, function(x) all(is.finite(x)))
  #df <- df[stats::complete.cases(df[cols]), , drop = FALSE]
  if (!any(idx)) return(NULL)
  df <- df[idx, , drop = FALSE]
  df_names <- names(df)
  idx <- vapply(df, is.numeric, logical(1), USE.NAMES = FALSE)
  idx <- match(cols, df_names[idx], nomatch = 0L) == 0L
  if (any(idx)) {
    stop(sprintf("In 'meta_analyze_ratio', the following inputs to %s do not refer to numeric columns in 'df': %s", paste(shQuote(c("estimate", "lower", "upper")[idx]), collapse = "/"), paste(cols[idx], collapse = ", ")))
  }
  incl_cols <- setdiff(intersect(incl_cols, df_names), c(cols, study_label))
  estimate_original <- .subset2(df, estimate)
  lower_original <- .subset2(df, lower)
  upper_original <- .subset2(df, upper)
  est <- transform_to_log_odds(estimate_original)
  lo <- transform_to_log_odds(lower_original)
  hi <- transform_to_log_odds(upper_original)
  idx <- is.finite(est) & is.finite(lo) & is.finite(hi)
  if (!all(idx)) {
    if (!any(idx)) return(NULL)
    est <- est[idx]
    lo <- lo[idx]
    hi <- hi[idx]
    df <- df[idx, , drop = FALSE]
  }
  # Swap lower and upper if lower > upper after transformation
  if (force_lower_upper && any(idx <- lo > hi)) {
    z <- hi
    hi[idx] <- lo[idx]
    lo[idx] <- z[idx]
  }
  row_idx <- seq_along(est)
  if (is.null(grouping_var)) {
    row_idx <- list(a = row_idx)
  } else {
    incl_cols <- unique(c(grouping_var, incl_cols))
    m <- df
    for (i in grouping_var) {
      m[[i]] <- as.character(m[[i]])
      m[[i]][is.na(m[[i]])] <- "MISSING___"
    }
    row_idx <- split(row_idx, f = as.list(m[grouping_var]))
  }
  #idx <- lengths(row_idx, use.names = FALSE) > 1L
  #row_idx <- row_idx[idx]
  add_cols <- length(incl_cols) > 0L
  metagen_args <- list(
    sm = effect_measure,
    method.tau = method,
    random = random_effects,
    method.random.ci = "classic",
    ...
  )
  df$is_meta <- FALSE
  run_meta <- function(i) {
    row_id <- row_idx[[i]]
    x <- df[row_id, , drop = FALSE]
    x$id_meta <- i
    metagen_args$TE <- est[row_id]
    metagen_args$lower <- lo[row_id]
    metagen_args$upper <- hi[row_id]
    meta <- tryElse(do.call(meta::metagen, metagen_args))
    if (is.null(meta)) {
      return(list(plot_data = x, grouping_var = grouping_var))
    }
    df_meta <- summary(meta)
    df_meta <- df_meta$random
    df_meta <- vec2df(estimate = df_meta$TE, lower = df_meta$lower, upper = df_meta$upper, .col_names = cols)
    for (j in cols) {
      df_meta[[j]] <- backtransform_from_log_odds(.subset2(df_meta, j))
    }
    if (force_lower_upper) {
      lower <- .subset2(df_meta, cols[2L])
      upper <- .subset2(df_meta, cols[3L])
      idx <- lower > upper
      idx <- !is.na(idx) & idx
      if (any(idx)) {
        df_meta[[cols[3L]]][idx] <- lower[idx]
        df_meta[[cols[2L]]][idx] <- upper[idx]
      }
    }
    if (add_cols) {
      z <- x[, incl_cols, drop = FALSE]
      idx <- vapply(z, function(y) length(unique(y)) == 1L, logical(1), USE.NAMES = FALSE)
      if (any(idx)) {
        df_meta <- cbind(z[1L, idx], df_meta)
      }
    }
    df_meta$is_meta <- TRUE
    df_meta$id_meta <- i
    meta$summary_table <- df_meta
    meta$plot_data <- tryCatch(dplyr::bind_rows(x, df_meta), error = function(e) NULL)
    meta$grouping_var <- grouping_var
    meta
  }
  out <- lapply(seq_along(row_idx), run_meta)
  if (as_df) {
    out <- lapply(out, `[[`, "summary_table")
    out <- dplyr::bind_rows(out)
    if (!is.null(study_label)) {
      if (length(study_label) != 1L || match(study_label, names(out), nomatch = 0L) != 0L) {
        study_label <- "study_label"
      }
      out[[study_label]] <- study_label_meta
    }
    out <- out[unique(c(study_label, setdiff(names(out), cols), cols))]
  } else if (length(out) == 1L) {
    out <- out[[1L]]
  }
  out
}

#' Run meta-analysis for VE input
#'
#' Only use when VE estimated using (1 - OR)x100 or (1 - RR)x100
#' @noRd
meta_analyze_ve <- function(
    df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    grouping_var = NULL,
    study_label = "study_label",
    study_label_meta = "Meta-analysis",
    effect_measure = "OR",
    method = "DL",
    random_effects = TRUE,
    incl_cols = NULL,
    as_df = FALSE,
    ...) {
  meta_analyze_ratio(
    df,
    estimate = estimate,
    lower = lower,
    upper = upper,
    grouping_var = grouping_var,
    study_label = study_label,
    study_label_meta = study_label_meta,
    effect_measure = effect_measure,
    method = method,
    random_effects = random_effects,
    incl_cols = incl_cols,
    transform_to_log_odds = function(x) log(1 - x/100),
    backtransform_from_log_odds = function(x) (1 - exp(x))*100,
    force_lower_upper = TRUE,
    as_df = as_df,
    ...
  )
}

#' Extract data frame from meta-analysis results
#'
#' @noRd
meta2df <- function(x, type = "plot_data", ...) {
  if (length(x) == 0L) return(NULL)
  x <- if (inherits(x, "meta")) x[[type]] else tryElse(dplyr::bind_rows(lapply(x, `[[`, type)))
  if (is.null(x) || nrow(x) == 0L) return(NULL)
  if (...length() != 0L && all(nzchar(names(new_cols <- list(...))))) {
    n_rows <- lengths(new_cols, use.names = FALSE)
    new_cols <- new_cols[n_rows == 1L | n_rows == nrow(x)]
    if (length(new_cols) != 0L) {
      x <- cbind(new_cols, x)
    }
  }
  x
}

#' Extract info from meta object
#'
#' @noRd
extract_meta_info <- function(x, ..., .inv_trans_log_or = exp, .add_labels = TRUE, .estimate_digits = 2) {
  out <- vec2df(
    n_studies = x$k,
    estimate_type = x$sm,
    p_het = x$pval.Q,
    I2_estimate = x$I2*100,
    I2_lower = x$lower.I2*100,
    I2_upper = x$upper.I2*100,
    tau2_estimate = x$tau2,
    tau2_lower = x$lower.tau2,
    tau2_upper = x$upper.tau2,
    tau_method = x$method.tau,
    estimate_re = .inv_trans_log_or(x$TE.random),
    lower_re = .inv_trans_log_or(x$lower.random),
    upper_re = .inv_trans_log_or(x$upper.random),
    p_re = x$pval.random,
    estimate_ce = .inv_trans_log_or(x$TE.common),
    lower_ce = .inv_trans_log_or(x$lower.common),
    upper_ce = .inv_trans_log_or(x$upper.common),
    p_ce = x$pval.common,
    estimate_fe = .inv_trans_log_or(x$TE.fixed),
    lower_fe = .inv_trans_log_or(x$lower.fixed),
    upper_fe = .inv_trans_log_or(x$upper.fixed),
    p_fe = x$pval.fixed,
    H_estimate = x$H,
    H_lower = x$lower.H,
    H_upper = x$upper.H,
    Q = x$Q,
    ...
  )
  if (!.add_labels) return(out)
  new_var <- c("random_effects", "common_effects", "fixed_effects")
  var_suffix <- c("re", "ce", "fe")
  var_prefix <- c("estimate_", "lower_", "upper_")
  out_names <- names(out)
  for (i in seq_len(3)) {
    cols <- paste0(var_prefix, var_suffix[i])
    if (anyNA(match(cols, out_names))) next
    out[[new_var[i]]] <- .format_num_range(.subset2(out, cols[1L]), .subset2(out, cols[2L]), .subset2(out, cols[3L]), digits = .estimate_digits)
  }
  i2 <- .subset2(out, "I2_estimate")
  p <- .subset2(out, "p_het")
  out$het <- sprintf("I\u00B2 = %.1f%%, %s", round_up(i2, 1), .format_p_value(p))
  out$het_label <- .format_heterogeneity_label(i2 = i2, p = p, as_string = TRUE)
  out$het_label_with_tau <- .format_heterogeneity_label(i2 = i2, p = p, tau2 = .subset2(out, "tau2_estimate"), as_string = TRUE)
  out
}

# Labels ------------------------------------------------------------------

#' Format text for heterogeneity label
#'
#' @noRd
.format_heterogeneity_label <- function(
  i2 = NULL,
  tau2 = NULL,
  p = NULL,
  sep = "*','~",
  tau_digits = 4,
  title = "'Heterogeneity:'",
  i2_prefix = "italic(I)^2",
  tau2_prefix = "\u03c4^2",
  p_prefix = "italic(P)",
  as_string = FALSE) {
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
