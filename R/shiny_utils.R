#' Namespacing function
#'
#' Rewritten version of `shiny::NS`
#' @param namespace Character vector indicating namespace. If `NULL` (default), output is equivalent to `identity`
#' @param sep Separator between namespace and id entered as input to namespacing function returned by `NS`. Enter as length 1 character vector. Default is `"-"`
#' @returns Function that accepts id as length 1 character vector as input
#' @noRd
NS <- function(namespace = NULL, sep = "-") {
  force(sep)
  prefix_empty <- length(namespace) == 0L
  ns_prefix <- if (prefix_empty) character(0) else paste(namespace, collapse = sep)
  function(id) {
    if (length(id) == 0L) return(ns_prefix)
    if (prefix_empty) return(id)
    paste(ns_prefix, id, sep = sep)
  }
}

#' Generate bookmark link for shiny app
#'
#' create_bookmark(studies_virus = c("COVID", "RSV"), studies_population = c("Adult", "Pregnant"))
#' @noRd
create_bookmark <- function(..., page = "https://cidrap.shinyapps.io/vip_v1/", tab = NULL) {
  n <- ...length()
  if (n == 0L) return(page)
  args <- if (n == 1L && length(...names()) == 0L) c(...) else list(...)
  input_ids <- names(args)
  out <- vapply(seq_along(args), function(i) {
    paste0(
      "&",
      input_ids[i],
      "=%5B%22",
      paste(args[[i]], collapse = "%22%2C%22"),
      "%22%5D"
    )
  }, character(1), USE.NAMES = FALSE)
  out <- paste(out, collapse = "")
  if (!is.null(tab)) {
    out <- paste0("&tabs=%22", tab, "%22", out)
  }
  paste0(
    page,
    "?_inputs_",
    out
  )
}
