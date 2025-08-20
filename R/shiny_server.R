#' Convert ggplot object to interactive plotly object
#'
#' @param x ggplot object
#' @param ... Named list of variables to display in tooltip. Enter using same syntax as `ggplot2::aes`. Names control variable label in tooltip. Values should be variables in `x$data` entered as unquoted variable name or columns selected using `.data` prefix
#' @param .show_legend If `TRUE`, legend is displayed. If `FALSE` (default), no legend is displayed
#' @param .dynamic_ticks If `TRUE` (default), dynamic ticks are shown. If `FALSE`, dynamic ticks are not shown
#' @returns plotly object
#' @noRd
plot_interactive <- function(
  x,
  ...,
  .show_legend = FALSE,
  .dynamic_ticks = TRUE) {
  if (...length() != 0L) {
    x$mapping <- c(x$mapping, do.call(ggplot2::aes, alist(...)))
  }
  if (!.show_legend) {
    x$theme$legend.position <- "none"
  }
  tryCatch(
    suppressWarnings(plotly::ggplotly(x, dynamicTicks = .dynamic_ticks)),
    error = function(e) suppressWarnings(plotly::ggplotly(x, dynamicTicks = FALSE))
  )
}
