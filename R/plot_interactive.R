#' Convert ggplot object to interactive plotly object
#'
#' @param .plot ggplot object
#' @param .vars Variables to display in tooltips. Enter as character vector of column names in `.plot@data`
#' @param .var_names Names of variables to display in tooltips. Order/length must match input to `.vars`
#' @param .hide_tooltips If `TRUE`, tooltip info is not displayed. Default is `FALSE`
#' @param .show_legend If `TRUE`, legend is displayed. If `FALSE` (default), no legend is displayed
#' @param .dynamic_ticks If `TRUE`, dynamic ticks are shown. If `FALSE` (default), dynamic ticks are not shown
#' @param .toolbar_buttons Character vector containing names of buttons to include in toolbar. Options: `"toImage"`, `"zoom"`, `"zoomIn"`, `"zoomOut"`, `"autoScale"`, `"resetScale"`, `"pan"`, `"select2d"`, `"lasso2d"`, `"drawclosedpath"`, `"drawopenpath"`, `"drawline"`, `"drawrect"`, `"eraseshape"`, `"hoverClosestCartesian"`, `"hoverCompareCartesian"`
#' @param .export_file_type File type for exported plot. Options: `"png"` (default), `"jpg"`, `"pdf"`, `"svg"`. Only relevant when "toImage" is included in `.toolbar_buttons`
#' @param .export_height,.export_width Height and width of exported plot. Enter as length 1 numeric vectors. Default is `NULL` for both (height/width determined by dimensions of plot as it currently appears). Only relevant when "toImage" is included in `.toolbar_buttons`
#' @param .export_scale Plot scaling on export Enter as length 1 numeric vector. Default is `1`. Only relevant when "toImage" is included in `.toolbar_buttons`
#' @param .file_name Default name for exported plot file. Enter as length 1 character vector. Default is `"vip_plot"`. Only relevant when "toImage" is included in `.toolbar_buttons`
#' @returns plotly object
#' @noRd
plot_interactive <- function(
  .plot,
  .vars = NULL,
  .var_names = NULL,
  .hide_tooltips = FALSE,
  .toolbar_buttons = c("toImage", "pan", "zoom", "zoomIn", "zoomOut", "resetScale"),
  .export_file_type = c("png", "jpg", "pdf", "svg"),
  .export_height = NULL,
  .export_width = NULL,
  .export_scale = 1,
  .file_name = "vip_plot",
  .show_legend = FALSE,
  .x_axis_position = NULL,
  .x_axis_title = NULL,
  .x_axis_labels = NULL,
  .y_axis_position = NULL,
  .y_axis_title = NULL,
  .y_axis_labels = NULL,
  .font_family = "sans-serif",
  .dynamic_ticks = FALSE,
  .traces_ignore_hover = NULL) {
  if (.hide_tooltips) {
    tooltip <- ""
  } else if (is.null(.vars)) {
    tooltip <- "all"
  } else {
    .var_names <- .var_names %||% .vars
    .var_names <- gsub("^\\.", "", .var_names)
    idx <- .vars %in% names(.plot@data)
    .vars <- .vars[idx]
    .var_names <- .var_names[idx]
    #.plot$mapping <- c(.plot$mapping, do.call(ggplot2::aes, alist(...)))
    for (i in seq_along(.vars)) {
      .plot@data[[.var_names[i]]] <- .subset2(.plot@data, .vars[i])
    }
    tooltip <- if (length(.var_names) == 0L) "" else .var_names
  }
  if (!.show_legend) {
    .plot@theme$legend.position <- "none"
  }
  out <- tryCatch(
    suppressWarnings(plotly::ggplotly(.plot, dynamicTicks = .dynamic_ticks, tooltip = tooltip)),
    error = function(e) suppressWarnings(plotly::ggplotly(.plot, dynamicTicks = FALSE, tooltip = tooltip))
  )

  # Toolbar buttons
  standard_buttons <- c("toImage", "zoom", "pan", "select2d", "lasso2d", "zoomIn", "zoomOut", "autoScale", "resetScale", "hoverClosestCartesian", "hoverCompareCartesian")
  remove_buttons <- setdiff(standard_buttons, .toolbar_buttons)
  #non_standard_buttons <- c("drawclosedpath", "drawopenpath", "drawline", "drawrect", "eraseshape")
  #add_buttons <- intersect(.toolbar_buttons, non_standard_buttons)
  if (length(.toolbar_buttons) != 0L && any(.toolbar_buttons == "toImage")) {
    .export_file_type <- match.arg(.export_file_type)
    export_options <- list(
      format = .export_file_type,
      height = .export_height,
      width = .export_width,
      scale = .export_scale,
      filename = .file_name
    )
  } else {
    export_options <- NULL
  }
  out <- plotly::config(
    out,
    toImageButtonOptions = export_options,
    modeBarButtonsToRemove = remove_buttons,
    modeBarButtonsToAdd = .toolbar_buttons,
    displaylogo = FALSE
  )

  # Plot style
  out$x$layout$font$family <- .font_family %||% ""

  # x axis
  #out$x$layout$xaxis$ticktext <- x_axis_labels
  #out$x$layout$xaxis$nticks <- n_ticks
  #out$x$layout$xaxis$tickfont$color <- x_axis_font_color
  #out$x$layout$xaxis$tickfont$size <- x_axis_font_size
  #out$x$layout$xaxis$tickangle <- x_axis_label_angle
  #out$x$layout$xaxis$showline <- show_x_axis_line
  #out$x$layout$xaxis$linecolor <- x_axis_line_color
  #out$x$layout$xaxis$linewidth <- x_axis_line_thickness
  #out$x$layout$xaxis$title$text <- x_axis_title
  #out$x$layout$xaxis$title$font$color <- x_axis_title_color
  #out$x$layout$xaxis$title$font$size <- x_axis_title_size

  # Axes
  if (!is.null(.x_axis_position)) {
    out$x$layout$xaxis$side <- .x_axis_position
  }
  out$x$layout$xaxis$showline <- out$x$layout$yaxis$showline <- TRUE
  if (!missing(.x_axis_title)) {
    out$x$layout$xaxis$title$text <- .x_axis_title
  }
  if (!missing(.y_axis_title)) {
    out$x$layout$yaxis$title$text <- .y_axis_title
  }
  #if (is.null(.x_axis_labels)) out$x$layout$xaxis$showticklabels <- FALSE

  if (is.numeric(.traces_ignore_hover) && length(.traces_ignore_hover <- .traces_ignore_hover[.traces_ignore_hover <= length(out$x$data)]) != 0L) {
   for (i in .traces_ignore_hover) {
     out$x$data[[i]]$hoverinfo <- "skip"
   }
  }
  out
}
