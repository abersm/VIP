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
