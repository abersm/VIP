#' Confidence interval for proportion
#'
#' Functionality from Frank Harrell's excellent package HMisc
#' @param n Numerator of proportion. Enter as length 1 integer
#' @param total Denominator of proportion. Enter as length 1 integer
#' @param ci Type of confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param method Method for calculating confidence interval. Options include: `"wilson"` (default, used by Prism), `"wald"`, `"wald_corrected"`, `"agresti_coull"`, `"exact"`, `"asymptotic"`
#' @returns Length 3 numeric vector containing proportion, lower bound of CI, and upper bound of CI (in that order). If `method = "all"`, output is a matrix with 1 row for each method
#' @noRd
CI_prop <- function(n, total, ci = 0.95, method = c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "exact", "asymptotic")) {
  method <- match.arg(method, choices = c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "exact", "asymptotic"))
  l <- 0.5 + ci/2
  p <- n/total
  switch(method,
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

.odds_ratio <- function(..., ci = 0.95, n_min_chisq = 5) {
  x <- matrix(..., nrow = 2)
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
  data.frame(
    or = or[1L],
    or_lower = or_ci[1L],
    or_upper = or_ci[2L],
    p = p,
    method = method,
    p_fisher = p_fet,
    p_chi = p_chisq,
    n = n,
    pos = pos,
    perc_pos = pos/n,
    neg = neg,
    perc_neg = neg/n,
    outcome = outcome/n,
    perc_outcome = outcome/n,
    no_outcome = no_outcome,
    perc_no_outcome = no_outcome/n,
    tn = tn,
    fn = fn,
    fp = fp,
    tp = tp
  )
}

odds_ratio <- function(..., ci = 0.95, n_min_chisq = 5) {
  tryElse(
    .odds_ratio(..., ci = ci, n_min_chisq = n_min_chisq),
    otherwise = data.frame(
      or = NA_real_,
      or_lower = NA_real_,
      or_upper = NA_real_,
      p = NA_real_,
      method = NA_character_,
      p_fisher = NA_real_,
      p_chi = NA_real_,
      n = NA_real_,
      pos = NA_real_,
      perc_pos = NA_real_,
      neg = NA_real_,
      perc_neg = NA_real_,
      outcome = NA_real_,
      perc_outcome = NA_real_,
      no_outcome = NA_real_,
      perc_no_outcome = NA_real_,
      tn = NA_real_,
      fn = NA_real_,
      fp = NA_real_,
      tp = NA_real_
    )
  )
}
