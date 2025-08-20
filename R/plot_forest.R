#' Forest plot
#'
#' @param df Data frame
#' @param estimate,lower,upper Columns in `df` containing effect estimate, lower, and upper bound of confidence interval, respectively
#' @param grouping_var Grouping variable along y axis
#' @param label_var Variable used to create row labels in plot
#' @param ordering_var Variable used to reorder rows in forest plot. If `NULL` (default), no reordering
#' @param x_axis_title Title for x axis
#' @param estimate_label Label for effect estimate
#' @param color_var Variable used to determine color of effect estimate/CI bars
#' @param y_axis_label_size Font size for y axis labels. Default is `10`
#' @param y_axis_label_color Font color for y axis labels. Default is `"grey30"`
#' @param hjust Horizontal adjustment. Default is `1`
#' @param ratio Aspect ratio of plot. Default is `2.5`
#' @param add_n_bands Number of extra grey/white bands to add above
#' @param n_bands Total number of grey/white bands
#' @param x_min,x_max Lower and upper limit for x axis values, respectively
#' @param colors Colors for effect estimate/CI bars
#' @param point_shape Shapes used to display effect estimate. Options: `square` (default), `circle`, `diamond`, `triangle`
#' @param point_size Size of effect estimate points. Default is `2`
#' @param errorbar_width Width of error bars. Default is `0.25`
#' @param line_thickness Line thickness for effect estimate/CI bars. Default is `0.9`
#' @param point_border_thickness Border thickness for effect estimate points. Default is `0.7`
#' @param vert_line_x_position x intercept of dashed line. Default is `NULL` (no dashed line)
#' @param vert_line_thickness Thickness of reference vertical line. Default is `0.5`
#' @param vert_line_color Color of vertical line. Default is `"#333333"`
#' @param vert_linetype Linetype for reference vertical line. Default is `"dashed"`
#' @param x_label_fn Function used to generate x axis labels
#' @param scale Scale for effect estimate. Default is `"regular"`
#' @param n_rows Number of rows. Default is `1`
#' @param scales Input to `scales` argument of `facet_wrap` or `facet_grid`. Default is `"fixed"`
#' @param show_stripes If `TRUE` (default), alternating white and grey rectangles displayed in background
#' @param stripe_color Color for stripes. Default is `"#E5E5E5"`
#' @param show_legend If `FALSE` (default), no legend is displayed
#' @param legend_title Title for legend. Only relevant when `show_legend = TRUE`
#' @param axis_line_thickness Thickness of x axis line and ticks. Default is `0.7`
#' @param base_size Base font size for plot. Default is `14`
#' @param dodge_width Width of dodging. Only relevant when `grouping_var` is specified. Default is `0.7`
#' @param ... Arguments passed to `theme_vip`
#' @returns ggplot object
#' @export
plot_forest <- function(
    df,
    estimate,
    lower,
    upper,
    grouping_var = NULL,
    label_var = "variable",
    ordering_var = NULL,
    dodge_width = 0.7,
    x_axis_title = NULL,
    estimate_label = "regular",
    color_var = grouping_var,
    y_axis_label_size = 10,
    y_axis_label_color = "grey30",
    hjust = 1,
    add_n_bands = NULL,
    n_bands = NULL,
    x_min = NULL,
    x_max = NULL,
    colors = c("#0072B5", "#BC3C29", "#333333"),
    point_shape = "square",
    point_size = 2,
    errorbar_width = 0.25,
    line_thickness = 0.9,
    point_border_thickness = 0.7,
    vert_line_x_position = NULL,
    vert_line_thickness = 0.5,
    vert_line_color = "#333333",
    vert_linetype = "dashed",
    x_label_fn = NULL,
    scale = "regular",
    n_rows = 1,
    scales = "fixed",
    show_stripes = TRUE,
    stripe_color = "#E5E5E5",
    show_legend = FALSE,
    legend_title = NULL,
    base_size = 14,
    ratio = 2.5,
    aspect_ratio = ratio,
    axis_line_thickness = 0.7,
    ...) {
  shape <- switch(point_shape, square = 22, circle = 21, diamond = 23, triangle = 24)
  color_var <- color_var %||% grouping_var
  if (is.null(color_var)) {
    df$color_var <- "a"
    color_var <- "color_var"
  }
  total_bands <- length(unique(df[[label_var]]))
  if (startsWith(scale, "log")) {
    base <- if (scale == "log2") 2 else 10
    x_min <- x_min %||% (base^(floor(log(min(df[[lower]], na.rm = TRUE), base = base))))
    x_max <- x_max %||% (base^(ceiling(log(max(df[[upper]], na.rm = TRUE), base = base))))
    x_breaks <- (scales::trans_breaks(scale, function(x) base^x))(c(x_min, x_max))
    x_breaks <- x_breaks[log(x_breaks, base = base) %% 1 == 0]
    x_label_fn <- x_label_fn %||% function(x) log(x, base)
    #x_scale <- ggplot2::scale_x_continuous(
    #    name = x_axis_title,
    #    trans = scales::log_trans(base = base),
    #    limits = c(x_min, x_max),
    #    expand = c(0, 0, 0, 0),
    #    breaks = x_breaks,
    #    labels = x_label_fn
    #  )
    x_scale <- NULL
  } else {
    x_min <- x_min %||% floor(min(df[[lower]], na.rm = TRUE))
    x_max <- x_max %||% ceiling(max(df[[upper]], na.rm = TRUE))
    x_breaks <- pretty(c(x_min, x_max))
    axis_limits <- range(c(x_min, x_max, x_breaks, vert_line_x_position))
    x_scale <- ggplot2::scale_x_continuous(
      name = x_axis_title,
      limits = axis_limits,
      expand = c(0, 0, 0, 0),
      breaks = x_breaks,
      guide = ggplot2::guide_axis(cap = "both")
    )
  }
  stripes <- if (show_stripes) {
    if (is.null(n_bands) && !is.null(add_n_bands)) {
      total_bands <- total_bands + add_n_bands
    }
    if (is.null(add_n_bands) && !is.null(n_bands)) {
      if (n_bands > total_bands) {
        total_bands <- n_bands
      }
    }
    ggplot2::annotate(
      geom = "rect",
      fill = stripe_color,
      xmin = axis_limits[1L],
      xmax = axis_limits[2L],
      ymin = seq.int(from = 0.5, to = total_bands - 0.5, by = 2L),
      ymax = seq.int(from = 1.5, to = total_bands + 0.5, by = 2L)
    )
  } else {
    NULL
  }
  vert_line <- if (is.null(vert_line_x_position)) {
    NULL
  } else {
    ggplot2::geom_hline(
      yintercept = vert_line_x_position,
      linetype = vert_linetype,
      color = vert_line_color,
      linewidth = vert_line_thickness
    )
  }
  blank <- ggplot2::element_blank()
  reorder <- !is.null(ordering_var) && any(names(df) == ordering_var)
  if (reorder) {
    df$label_var_reordered <- fct_reorder(df[[label_var]], df[[ordering_var]])
  }
  if (is.null(grouping_var)) {
    mapping <- if (reorder) ggplot2::aes(x = .data[[estimate]], y = .data$label_var_reordered) else ggplot2::aes(x = .data[[estimate]], y = .data[[label_var]])
    errorbar_mapping <- ggplot2::aes(
      xmin = .data[[lower]],
      xmax = .data[[upper]],
      color = .data[[color_var]]
    )
    position <- ggplot2::position_identity()
  } else {
    mapping <- if (reorder) ggplot2::aes(x = .data[[estimate]], y = .data$label_var_reordered, group = .data[[grouping_var]]) else ggplot2::aes(x = .data[[estimate]], y = .data[[label_var]], group = .data[[grouping_var]])
    errorbar_mapping <- ggplot2::aes(
      xmin = .data[[lower]],
      xmax = .data[[upper]],
      color = .data[[color_var]],
      group = .data[[grouping_var]]
    )
    position <- ggplot2::position_dodge(width = dodge_width)
  }
  p <- ggplot2::ggplot(df, mapping) +
    stripes +
    vert_line +
    ggplot2::geom_errorbar(
      mapping = errorbar_mapping,
      width = errorbar_width,
      linewidth = line_thickness,
      position = position,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(color = .data[[color_var]], fill = .data[[color_var]]),
      shape = shape,
      size = point_size,
      stroke = point_border_thickness,
      position = position,
      show.legend = show_legend
    ) +
    ggplot2::scale_fill_manual(name = legend_title, values = colors) +
    ggplot2::scale_color_manual(values = colors) +
    x_scale +
    ggplot2::scale_y_discrete(name = NULL) +
    theme_vip(base_size = base_size, aspect_ratio = aspect_ratio, ...) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      axis.ticks.y = blank,
      axis.line.y = blank,
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, -2.5, 0, 0), size = y_axis_label_size, hjust = hjust, color = y_axis_label_color),
      axis.line.x = ggplot2::element_line(linewidth = axis_line_thickness),
      axis.ticks.x = ggplot2::element_line(linewidth = axis_line_thickness),
      axis.ticks.length.x = grid::unit(3.75, units = "pt"),
      plot.margin = ggplot2::margin(r = 5, unit = "mm")
    )
  p
}
