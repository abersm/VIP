#' Complete a partially filled 2 x 2 cross tabulation
#'
#' @param df Data frame containing raw counts in separate columns. Each row represents a single 2 x 2 matrix
#' @param r1c1,r1c2,r2c1,r2c2 Either name of column in `df` (as length 1 character vector) containing raw counts for cell in 2 x 2 matrix or a vector of counts with length equal to `nrow(df)` (or 1). Use `NULL` if column not present in `df`
#' @param r1,r2,c1,c2 Either name of column in `df` (as length 1 character vector) containing row/column sums in 2 x 2 matrix or a vector of counts with length equal to `nrow(df)` (or 1). Use `NULL` if column not present in `df`
#' @param total Either name of column in `df` (as length 1 character vector) containing grand total of cell counts in 2 x 2 matrix or a vector of counts with length equal to `nrow(df)` (or 1). Use `NULL` if column not present in `df`
#' @param replace_columns If `TRUE`, original count columns are replaced by calculated counts with new columns added for counts not included in `df`. If `FALSE` (default), calculated counts added as separate columns. Names for any newly added columns include: ".r1c1", ".r1c2", ".r2c1", ".r2c2", ".r1", ".r2", ".c1", ".c2", ".total". If naming collisions occur, prior column names will be overwritten with new column names
#' @param haldane_correction If `TRUE`, Haldane correction is applied when 0 counts are present
#' @returns Data frame with containing calculated counts. Regardless of input to `replace_columns`, output will include a logical column named ".calculable" indicating whether all counts could be calculated for a given row
#' @export
fill_2x2 <- function(
    df,
    r1c1 = "n_vaxed_with_outcome",
    r2c1 = "n_unvaxed_with_outcome",
    r1c2 = "n_vaxed_without_outcome",
    r2c2 = "n_unvaxed_without_outcome",
    r1 = "n_vaxed",
    r2 = "n_unvaxed",
    c1 = "n_with_outcome",
    c2 = "n_without_outcome",
    total = "n_total",
    replace_columns = FALSE,
    haldane_correction = TRUE) {
  if (replace_columns) {
    df_names <- names(df)
    get_name <- function(x, default) if (is.character(x) && length(x) == 1L && any(df_names == x)) x else default
    col_names <- list(
      r1c1 = get_name(r1c1, ".r1c1"),
      r1c2 = get_name(r1c2, ".r1c2"),
      r2c1 = get_name(r2c1, ".r2c1"),
      r2c2 = get_name(r2c2, ".r2c2"),
      r1 = get_name(r1, ".r1"),
      r2 = get_name(r2, ".r2"),
      c1 = get_name(c1, ".c1"),
      c2 = get_name(c2, ".c2"),
      total = get_name(total, ".total")
    )
  }
  f <- function(x) {
    if (is.null(x)) {
      NA_integer_
    } else if (is.character(x) && length(x) == 1L) {
      .subset2(df, x) %||% NA_integer_
    } else {
      x
    }
  }
  r1c1 <- f(r1c1)
  r1c2 <- f(r1c2)
  r2c1 <- f(r2c1)
  r2c2 <- f(r2c2)
  r1 <- f(r1)
  r2 <- f(r2)
  c1 <- f(c1)
  c2 <- f(c2)
  total <- f(total)
  r1c1_nna <- !is.na(r1c1)
  r2c1_nna <- !is.na(r2c1)
  r1c2_nna <- !is.na(r1c2)
  r2c2_nna <- !is.na(r2c2)
  r1_nna <- !is.na(r1)
  r2_nna <- !is.na(r2)
  c1_nna <- !is.na(c1)
  c2_nna <- !is.na(c2)
  total_nna <- !is.na(total)
  total <- dplyr::case_when(
    total_nna ~ total,
    r1_nna & r2_nna ~ r1 + r2,
    c1_nna & c2_nna ~ c1 + c2,
    .default = r1c1 + r1c2 + r2c1 + r2c2
  )
  total_nna <- !is.na(total)
  r1 <- dplyr::case_when(
    r1_nna ~ r1,
    total_nna & r2_nna ~ total - r2,
    .default = r1c1 + r1c2
  )
  r1_nna <- !is.na(r1)
  r2 <- dplyr::case_when(
    r2_nna ~ r2,
    total_nna & r1_nna ~ total - r1,
    .default = r2c1 + r2c2
  )
  r2_nna <- !is.na(r2)
  c1 <- dplyr::case_when(
    c1_nna ~ c1,
    total_nna & c2_nna ~ total - c2,
    .default = r1c1 + r2c1
  )
  c1_nna <- !is.na(c1)
  c2 <- dplyr::case_when(
    c2_nna ~ c2,
    total_nna & c1_nna ~ total - c1,
    #r1c2_nna & r2c2_nna ~ r1c2 + r2c2,
    .default = r1c2 + r2c2
  )
  c2_nna <- !is.na(c2)
  r1c1 <- dplyr::case_when(
    r1c1_nna ~ r1c1,
    c1_nna & r2c1_nna ~ c1 - r2c1,
    r1_nna & r1c2_nna ~ r1 - r1c2,
    !r2c2_nna ~ NA_integer_,
    c1_nna & r2_nna ~ c1 - (r2 - r2c2),
    r1_nna & c2_nna ~ r1 - (c2 - r2c2),
    .default = total - r1c2 - r2c1 - r2c2
  )
  r1c1_nna <- !is.na(r1c1)
  r1c2 <- dplyr::case_when(
    r1c2_nna ~ r1c2,
    c2_nna & r2c2_nna ~ c2 - r2c2,
    r1_nna & r1c1_nna ~ r1 - r1c1,
    c2_nna & r2_nna & r2c1_nna ~ c2 - (r2 - r2c1),
    .default = total - r1c1 - r2c1 - r2c2
  )
  r1c2_nna <- !is.na(r1c2)
  r2c1 <- dplyr::case_when(
    r2c1_nna ~ r2c1,
    c1_nna & r1c1_nna ~ c1 - r1c1,
    r2_nna & r2c2_nna ~ r2 - r2c2,
    r2_nna & c2_nna & r1c2_nna ~ r2 - (c2 - r1c2),
    .default = total - r1c1 - r1c2 - r2c2
  )
  r2c2 <- dplyr::case_when(
    r2c2_nna ~ r2c2,
    c2_nna & r1c2_nna ~ c2 - r1c2,
    r2_nna & !is.na(r2c1) ~ r2 - r2c1,
    .default = total - r1c1 - r1c2 - r2c1
  )
  if (haldane_correction && any(idx <- r1c1 == 0 | r1c2 == 0L | r2c1 == 0L | r2c2 == 0L, na.rm = TRUE)) {
    idx <- !is.na(idx) & idx
    r1c1[idx] <- r1c1[idx] + 0.5
    r1c2[idx] <- r1c2[idx] + 0.5
    r2c1[idx] <- r2c1[idx] + 0.5
    r2c2[idx] <- r2c2[idx] + 0.5
  } else {
    haldane_correction <- FALSE
  }
  total <- r1c1 + r1c2 + r2c1 + r2c2
  if (replace_columns) {
    df[[col_names$r1c1]] <- r1c1
    df[[col_names$r1c2]] <- r1c2
    df[[col_names$r2c1]] <- r2c1
    df[[col_names$r2c2]] <- r2c2
    df[[col_names$r1]] <- r1c1 + r1c2
    df[[col_names$r2]] <- r2c1 + r2c2
    df[[col_names$c1]] <- r1c1 + r2c1
    df[[col_names$c2]] <- r1c2 + r2c2
    df[[col_names$total]] <- total
  } else {
    df$.r1c1 <- r1c1
    df$.r1c2 <- r1c2
    df$.r2c1 <- r2c1
    df$.r2c2 <- r2c2
    df$.r1 <- r1c1 + r1c2
    df$.r2 <- r2c1 + r2c2
    df$.c1 <- r1c1 + r2c1
    df$.c2 <- r1c2 + r2c2
    df$.total <- total
  }
  if (haldane_correction) {
    df$.haldane_correction <- idx
  }
  df$.calculable <- !is.na(total)
  df
}

#' Calculate odds ratio
#'
#' @noRd
.odds_ratio <- function(x, ci = 0.95, n_min_chisq = 5) {
  tn <- x[1L, 1L]
  tp <- x[2L, 2L]
  fn <- x[2L, 1L]
  fp <- x[1L, 2L]
  n <- sum(x)
  outcome <- tp + fn
  no_outcome <- tn + fp
  pos <- tp + fp
  neg <- tn + fn
  or <- tn*tp/(fp*fn)
  or_ci <- or*exp(stats::qnorm(0.5 + ci/2)*sqrt(sum(1/x))*c(-1, 1))
  p_fet <- suppressWarnings(stats::fisher.test(x)$p.value)
  p_chisq <- suppressWarnings(stats::chisq.test(x)$p.value)
  if (min(x, na.rm = TRUE) < 5) {
    p <- p_fet
    method <- "Fisher's exact test"
  } else {
    p <- p_chisq
    method <- "Chi-squared test"
  }
  vec2df(
    or = or[1L],
    or_lower = or_ci[1L],
    or_upper = or_ci[2L],
    p = p,
    method = method,
    p_fisher = p_fet,
    p_chi = p_chisq,
    n_total = n,
    pos = pos,
    perc_pos = pos/n,
    neg = neg,
    perc_neg = neg/n,
    n_outcome = outcome/n,
    perc_outcome = outcome/n,
    n_no_outcome = no_outcome,
    perc_no_outcome = no_outcome/n,
    tn = tn,
    fn = fn,
    fp = fp,
    tp = tp
  )
}

#' Main odds ratio function
#'
#' @noRd
odds_ratio <- function(..., ci = 0.95, n_min_chisq = 5) {
  tryElse(
    .odds_ratio(matrix(..., nrow = 2L), ci = ci, n_min_chisq = n_min_chisq),
    otherwise = data.frame(
      or = NA_real_,
      or_lower = NA_real_,
      or_upper = NA_real_,
      p = NA_real_,
      method = NA_character_,
      p_fisher = NA_real_,
      p_chi = NA_real_,
      n_total = NA_real_,
      pos = NA_real_,
      perc_pos = NA_real_,
      neg = NA_real_,
      perc_neg = NA_real_,
      n_outcome = NA_real_,
      perc_outcome = NA_real_,
      n_no_outcome = NA_real_,
      perc_no_outcome = NA_real_,
      tn = NA_real_,
      fn = NA_real_,
      fp = NA_real_,
      tp = NA_real_
    )
  )
}

#' Calculate odds ratio rowwise
#'
#' @noRd
odds_ratio_rowwise <- function(
    df,
    r1c1 = "n_vaxed_with_outcome",
    r2c1 = "n_unvaxed_with_outcome",
    r1c2 = "n_vaxed_without_outcome",
    r2c2 = "n_unvaxed_without_outcome",
    r1 = "n_vaxed",
    r2 = "n_unvaxed",
    c1 = "n_with_outcome",
    c2 = "n_without_outcome",
    total = "n_total",
    ci = 0.95,
    n_min_chisq = 5) {
  out <- fill_2x2(df, r1c1 = r1c1, r2c1 = r2c1, r1c2 = r1c2, r2c2 = r2c2, r1 = r1, r2 = r2, c1 = c1, c2 = c2, total = total)
  out <- out[c(".r1c1", ".r2c1", ".r1c2", ".r2c2")]
  out <- apply(out, 1L, function(x) odds_ratio(c(x), ci = ci, n_min_chisq = n_min_chisq))
  out <- as.data.frame(do.call(rbind, out))
  out <- cbind(df, out)
  names(out) <- make.unique(names(out), sep = "_")
  out
}

#' Confidence interval for proportion
#'
#' Functionality from Frank Harrell's excellent package HMisc
#' @param n Numerator of proportion. Enter as length 1 integer
#' @param total Denominator of proportion. Enter as length 1 integer
#' @param ci Type of confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param method Method for calculating confidence interval. Options include: `"wilson"` (default, used by Prism), `"wald"`, `"wald_corrected"`, `"agresti_coull"`, `"exact"`, `"asymptotic"`
#' @returns Length 3 numeric vector containing proportion, lower and upper bound of CI (in that order)
#' @noRd
CI_prop <- function(n, total, ci = 0.95, method = c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "exact", "asymptotic")) {
  method <- match.arg(method, choices = c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "exact", "asymptotic"))
  l <- 0.5 + ci/2
  p <- n/total
  switch(
    method,
    wilson = {
      z_crit <- stats::qnorm(l)
      z2 <- z_crit*z_crit
      ci_wilson <- (p + z2/2/total + c(-1, 1)*z_crit*sqrt((p*(1 - p) + z2/4/total)/total))/(1 + z2/total)
      if (n == 1) {
        ci_wilson[1L] <- -log(ci)/total
      }
      if (n == total - 1) {
        ci_wilson[2L] <- 1 + log(ci)/total
      }
      c(p, ci_wilson)
    },
    exact = {
      nu2 <- 2*n
      nu1 <- 2*total - nu2 + 2
      ll_exact <- if (n > 0) n/(n + stats::qf(l, nu1, nu2)*(total - n + 1)) else 0
      z <- if (n < total) stats::qf(l, nu2 + 2, nu1 - 2) else 1
      ul_exact <- (n + 1)*z/(total - n + (n + 1)*z)
      c(p, ll_exact, ul_exact)
    },
    wald = {
      z_crit <- stats::qnorm(l)
      z <- z_crit*sqrt(p*(1 - p)/total)
      c(p, max(0, p - z), min(1, p + z))
    },
    wald_corrected = {
      z_crit <- stats::qnorm(l)
      z <- z_crit*sqrt(p*(1 - p)/total) + 0.5/n
      c(p, max(0, p - z), min(1, p + z))
    },
    asymptotic = {
      z_crit <- stats::qnorm(l)
      c(p, p + z_crit*sqrt(p*(1 - p)/total)*c(-1, 1))
    },
    agresti_coull = {
      z_crit <- stats::qnorm(l)
      z2 <- z_crit*z_crit
      n_ac <- n + z2/2
      total_ac <- total + z2
      p_ac <- n_ac/total_ac
      z <- z_crit*sqrt(p_ac*p_ac*(1 - p_ac)/total_ac)
      c(p_ac, max(0, p_ac - z), min(1, p_ac + z))
    }
  )
}
