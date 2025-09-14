#' Complete a partially filled 2 x 2 cross tabulation
#'
#' @param df Data frame containing raw counts in separate columns. Each row represents a single 2 x 2 matrix
#' @param r1c1,r1c2,r2c1,r2c2 Either name of column in `df` (as length 1 character vector) containing raw counts for cell in 2 x 2 matrix or a vector of counts with length equal to `nrow(df)` (or 1). Use `NULL` if column not present in `df`
#' @param r1,r2,c1,c2 Either name of column in `df` (as length 1 character vector) containing row/column sums in 2 x 2 matrix or a vector of counts with length equal to `nrow(df)` (or 1). Use `NULL` if column not present in `df`
#' @param total Either name of column in `df` (as length 1 character vector) containing grand total of cell counts in 2 x 2 matrix or a vector of counts with length equal to `nrow(df)` (or 1). Use `NULL` if column not present in `df`
#' @param replace_columns If `TRUE`, original count columns are replaced by calculated counts with new columns added for counts not included in `df`. If `FALSE` (default), calculated counts added as separate columns. Names for any newly added columns include: ".r1c1", ".r1c2", ".r2c1", ".r2c2", ".r1", ".r2", ".c1", ".c2", ".total". If naming collisions occur, prior column names will be overwritten with new column names
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
    replace_columns = FALSE) {
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
  df$.calculable <- !is.na(total)
  df
}
