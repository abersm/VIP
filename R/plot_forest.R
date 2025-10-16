#' Forest plot
#'
#' @param df Data frame
#' @param estimate,lower,upper Columns in `df` containing effect estimate, lower, and upper bound of confidence interval, respectively
#' @param y_var y axis variable. Should be categorical
#' @param grouping_var Grouping variable along y axis
#' @param ordering_var Variable used to reorder rows in forest plot. If `NULL` (default), input is ignored
#' @param point_color_var Variable used to determine point color
#' @param point_color Colors for points
#' @param point_border_color Border color (stroke) for points
#' @param point_shape_var Variable to determine point shapes
#' @param point_shape Shapes used to display effect estimate. Options: `square` (default), `circle`, `diamond`, `triangle`
#' @param point_size_var Variable to determine point size
#' @param point_size Size of effect estimate points. Default is `5`
#' @param meta_label Value for `label_var` to indicate rows with meta-analysis
#' @param meta_var Variable to indicate whether row in `df` refers to meta analysis. Enter as length 1 character vector. Default is `"is_meta"`
#' @param meta_id Variable to identify rows in `df` included in each meta-analysis
#' @param meta_point_color,meta_point_shape,meta_point_size Color, shape, and size for meta analysis points
#' @param errorbar_color_var Variable to determine color of effect estimate/CI bars
#' @param errorbar_color Color of effect estimate/CI bars. Default is `"#333333"`
#' @param label_var Variable used to create row labels in plot
#' @param x_axis_title Title for x axis
#' @param y_axis_label_size Font size for y axis labels. Default is `10`
#' @param y_axis_label_color Font color for y axis labels. Default is `"grey30"`
#' @param y_axis_labels Labels for y axis ticks
#' @param label_digits Number of digits to include in label
#' @param hjust Horizontal adjustment. Default is `1`
#' @param aspect_ratio Aspect ratio of plot. Default is `2.5`
#' @param ratio Alias for `aspect_ratio`
#' @param x_axis_limits Length 2 numeric vector contains limits for x axis
#' @param x_axis_breaks,x_axis_labels Breaks and labels for x axis
#' @param x_axis_scale Scale for effect estimate. Default is `"regular"`
#' @param expand_lower,expand_upper Expansion for lower and upper end of x axis. Enter each as length 1 numeric vector
#' @param cap Cap for x axis. Default is `"both"`
#' @param errorbar_width Width of error bars. Default is `0.25`
#' @param errorbar_thickness Line thickness for effect estimate/CI bars. Default is `0.7`
#' @param point_border_thickness Border thickness for effect estimate points. Default is `0.7`
#' @param vert_line_x_position x intercept of dashed line. Default is `NULL` (no dashed line)
#' @param vert_line_thickness Thickness of reference vertical line. Default is `0.5`
#' @param vert_line_color Color of vertical line. Default is `"#333333"`
#' @param vert_linetype Linetype for reference vertical line. Default is `"dashed"`
#' @param show_stripes If `TRUE` (default), alternating white and grey rectangles displayed in background
#' @param trim_stripes If `TRUE` (default), stripes are trimmed to axis limits
#' @param odd_stripe_colors,even_stripe_colors Colors for odd and even stripes. Enter as length character vector of hexadecimal codes or colors
#' @param show_legend If `FALSE` (default), no legend is displayed
#' @param legend_title Title for legend. Only relevant when `show_legend = TRUE`
#' @param axis_line_thickness Thickness of x axis line and ticks. Default is `0.7`
#' @param base_size Base font size for plot. Default is `14`
#' @param dodge_width Width of dodging. Only relevant when `grouping_var` is specified. Default is `0.9`
#' @param x_axis_include Value to include along x axis. Only relevant when `x_axis_limits = NULL`
#' @param bracket_type Type of bracket to use for 95% CI label. Default is `c("(", ")")`
#' @param data_limits x axis limits for error bars. Only changes plot appearance, not actually estimates/labels. Enter as length 2 numeric vector. Default is `NULL`, no limits applied
#' @param ... Arguments passed to `theme_vip`
#' @returns ggplot object
#' @export
plot_forest <- function(
    df,
    estimate,
    lower,
    upper,
    y_var = NULL,
    grouping_var = NULL,
    ordering_var = NULL,
    point_size_var = NULL,
    point_size = 5,
    point_shape_var = NULL,
    point_shape = "square",
    point_color_var = grouping_var,
    point_color = "#333333",
    meta_var = "is_meta",
    meta_id = "id_meta",
    meta_label = "",
    meta_point_size = point_size*1.05,
    meta_point_color = NULL,
    meta_point_shape = "diamond",
    errorbar_color_var = NULL,
    errorbar_color = "#333333",
    label_var = NULL,
    errorbar_width = 0.25,
    errorbar_thickness = 0.7,
    dodge_width = 0.9,
    point_border_thickness = 0.7,
    point_border_color = "#333333",
    axis_line_thickness = 0.7,
    y_axis_label_size = base_size,
    y_axis_label_color = "#333333",
    y_axis_labels = ggplot2::waiver(),
    x_axis_title = NULL,
    x_axis_limits = NULL,
    x_axis_breaks = NULL,
    x_axis_labels = ggplot2::waiver(),
    vert_line_x_position = NULL,
    vert_line_thickness = 0.5,
    vert_line_color = "#333333",
    vert_linetype = "dashed",
    legend_title = NULL,
    show_legend = FALSE,
    x_axis_scale = "regular",
    show_stripes = TRUE,
    trim_stripes = TRUE,
    odd_stripe_colors = "#22222222",
    even_stripe_colors = "#00000000",
    bracket_type = c("(", ")"),
    label_digits = 0,
    base_size = 14,
    hjust = 1,
    ratio = 2.5,
    aspect_ratio = ratio,
    expand_lower = 0,
    expand_upper = 0,
    x_axis_include = NULL,
    cap = "both",
    data_limits = NULL,
    ...) {
  # Create variables for plotting
  df$.x <- as.numeric(.subset2(df, estimate))
  df$.x_min <- as.numeric(.subset2(df, lower))
  df$.x_max <- as.numeric(.subset2(df, upper))
  df <- .add_plot_var(df, var = y_var, var_name = ".y_var", if_null = seq_len(nrow(df)), as_fct = TRUE)
  #df <- .add_plot_var(df, var = label_var, var_name = ".label_var", as_fct = TRUE)
  df <- .add_plot_var(df, var = label_var, var_name = ".label_var", if_null = df$.y_var, as_fct = TRUE)
  df <- .add_plot_var(df, var = grouping_var, var_name = ".grouping_var", as_fct = TRUE)
  df <- .add_plot_var(df, var = ordering_var, var_name = ".ordering_var")
  df <- .add_plot_var(df, var = point_shape_var, var_name = ".point_shape_var", if_null = point_shape[1L])
  df <- .add_plot_var(df, var = point_color_var, var_name = ".point_color_var", if_null = df$.grouping_var, as_fct = TRUE)
  df <- .add_plot_var(df, var = point_size_var, var_name = ".point_size_var", if_null = point_size)
  df <- .add_plot_var(df, var = errorbar_color_var, var_name = ".errorbar_color_var", as_fct = TRUE)
  if (length(unique(df$.grouping_var)) == 1L) {
    dodge_width <- 1
  }

  if (length(unique(df$.ordering_var)) > 1L) {
    df$.label_var <- .fct_reorder(df$.label_var, df$.ordering_var)
    df$.y_var <- .fct_reorder(df$.y_var, df$.ordering_var)
  }

  # Meta-analysis variables
  if (!is.null(meta_var) && (has_meta <- !is.null(is_meta <- .subset2(df, meta_var)))) {
    if (is.numeric(is_meta)) {
      is_meta <- as.logical(is_meta)
    } else if (!is.logical(is_meta)) {
      stop("In 'plot_forest', input to 'meta_var' must refer to a logical (TRUE for meta-analysis, FALSE for other) or numeric (1 for meta-analysis, 0 for other) column in 'df'")
    }
    if (anyNA(is_meta)) {
      is_meta[is.na(is_meta)] <- FALSE
    }
    df$.not_meta <- !is_meta
    if (is.null(y_var) || is.null(y_vals <- .subset2(df, y_var))) {
      y_vals <- if (!is.null(meta_id) && !is.null(meta_id_vals <- .subset2(df, meta_id))) {
        order(meta_id_vals)
      } else {
        seq_len(nrow(df))
      }
    }
    df$.y_var <- .as_fct(y_vals)
    df$.y_var <- .fct_reorder(df$.y_var, df$.not_meta)
    df$.label_var <- .fct_reorder(df$.label_var, df$.not_meta)
    df$.grouping_var <- .fct_reorder(df$.grouping_var, df$.not_meta)
  }

  # Legend
  no_legend <- ggplot2::guide_none()
  legend <- if (show_legend) ggplot2::guide_legend() else no_legend

  # Point shape
  shapes <- df$.point_shape_var
  unique_shapes <- unique(shapes)
  unique_shapes <- unique_shapes[!is.na(unique_shapes)]
  n_shapes <- length(unique_shapes)
  if (n_shapes == 0L || n_shapes > 4L) {
    df$.point_shape_var <- point_shape[1L]
  } else if (!all(unique_shapes %in% c("circle", "square", "triangle", "diamond"))) {
    df$.point_shape_var <- as.character(factor(shapes, labels = rep_len(point_shape, length.out = n_shapes)))
  }
  point_color <- rep_len(point_color, length.out = length(unique(df$.point_color_var)))

  # Point size
  sizes <- df$.point_size_var
  if (!is.numeric(sizes)) {
    unique_sizes <- unique(sizes[!is.na(sizes)])
    n_sizes <- length(unique_sizes)
    z <- point_size + seq.int(from = -1, to = 1, length.out = n_sizes)
    df$.point_size_var <- as.numeric(as.character(factor(sizes, labels = z)))
    #ggplot2::scale_size_area(guide = no_legend, max_size = z*(1 + n_sizes*0.5/2))
  }
  size_scale <- ggplot2::scale_size_identity(guide = no_legend)

  # Meta-analysis points
  if (has_meta && any(is_meta)) {
    df$.point_size_var[is_meta] <- meta_point_size %||% max(df$.point_size_var, na.rm = TRUE)
    if (!is.null(meta_point_shape)) {
      df$.point_shape_var[is_meta] <- meta_point_shape
    }
    if (all(is.na(df$.point_color_var[is_meta])) || !is.null(meta_point_color)) {
      meta_point_color <- meta_point_color %||% "#BC3C29"
      #color_levels <- levels(df$.point_color_var)
      #meta_level <- if (any(color_levels == "Meta-analysis")) "ZZZZZ" else "Meta-analysis"
      #levels(df$.point_color_var) <- c(color_levels, meta_level)
      #df$.point_color_var[is_meta] <- meta_level
      df$.point_color_var <- .fct_new_level_last(df$.point_color_var, new_level = "Meta-analysis", idx = is_meta)
      point_color <- c(point_color, meta_point_color)
    }
  }

  # Vertical lines
  vert_line <- if (is.null(vert_line_x_position)) {
    NULL
  } else {
    ggplot2::geom_vline(
      xintercept = vert_line_x_position,
      linetype = vert_linetype,
      color = vert_line_color,
      linewidth = vert_line_thickness
    )
  }

  # Stripes
  stripes <- if (show_stripes) {
    geom_stripes(odd = odd_stripe_colors, even = even_stripe_colors, trim = trim_stripes, direction = "horizontal")
  } else {
    NULL
  }

  # Axes
  #if (is.null(x_axis_breaks) && min(x_axis_limits %||% df$.x_min, x_axis_include, na.rm = TRUE) < 0) {
  #  x_axis_breaks <- breaks_linear(breaks_fn = .extended)
  #}
  x_axis_title <- x_axis_title %||% if (min(df$.x_min, na.rm = TRUE) < 0 || max(df$.x_max, na.rm = TRUE) > 40) "Vaccine effectiveness" else "OR"

  # Estimate and 95% CI labels
  bracket_type <- bracket_type %||% c("(", ")")
  if (length(bracket_type) == 1L) {
    bracket_type <- if (bracket_type %in% c("(", ")")) {
      c("(", ")")
    } else if (bracket_type %in% c("[", "]")) {
      c("[", "]")
    } else {
      c(bracket_type, bracket_type)
    }
  }
  label_digits <- label_digits %||% if (min(df$.x_min, na.rm = TRUE) < 0 || max(df$.x_max, na.rm = TRUE) > 40) 1L else 2L
  df$.estimate_label <- .format_num_range(
    estimate = df$.x,
    lower = df$.x_min,
    upper = df$.x_max,
    digits = label_digits,
    sep = "-",
    bracket_lower = bracket_type[1L],
    bracket_upper = bracket_type[2L]
  )
  if (!is.null(data_limits)) {
    if (!length(data_limits) == 2L) {
      data_limits <- c(data_limits, NA)
    }
    lower_limit <- data_limits[1L]
    if (!is.na(lower_limit)) {
      df$.x_min[!is.na(df$.x_min) & df$.x_min < lower_limit] <- lower_limit
      df$.x[!is.na(df$.x) & df$.x < lower_limit] <- lower_limit
    }
    upper_limit <- data_limits[2L]
    if (!is.na(upper_limit)) {
      df$.x_max[!is.na(df$.x_max) & df$.x_max > upper_limit] <- upper_limit
      df$.x[!is.na(df$.x) & df$.x > upper_limit] <- upper_limit
    }
  }
  #label_digits <- paste0("%.", label_digits, "f")
  #label_format <- paste0(label_digits, " ", bracket_type[1L], label_digits, "-", label_digits, bracket_type[2L])
  #df$.estimate_label <- sprintf(label_format, df$.x, df$.x_min, df$.x_max)
  #estimate_label <- sprintf(label_format, df$.x, df$.x_min, df$.x_max)
  #y_vals <- .subset2(df, ".y_var")
  #y_levels <- levels(y_vals)
  #y_idx <- seq_along(y_levels)
  #y_labeller <- function(x) {
  #  idx <- match(x, y_vals, nomatch = NA)
  #  estimate_label[idx]
  #}

  # Plot
  #y_levels <- tapply(df$.ordering_var, df$.y_var, function(x) mean(x, na.rm = TRUE))
  #levels(df$.y_var) <- unique(c("Pooled", names(y_levels)[order(y_levels, decreasing = TRUE)]))
  #y_levels <- tapply(df$.ordering_var, df$.label_var, function(x) mean(x, na.rm = TRUE))
  #levels(df$.label_var) <- unique(c("Pooled", names(y_levels)[order(y_levels, decreasing = TRUE)]))
  #levels(df$.y_var) <- unique(c("Pooled", levels(df$.y_var)))
  #levels(df$.label_var) <- unique(c("Pooled", levels(df$.label_var)))
  blank <- ggplot2::element_blank()
  p <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = .data$.x,
      y = .data$.y_var,
      group = .data$.grouping_var,
      fill = .data$.point_color_var,
      size = .data$.point_size_var,
      shape = .data$.point_shape_var
    )
  ) +
    stripes +
    vert_line +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        color = .data$.errorbar_color_var,
        xmin = .data$.x_min,
        xmax = .data$.x_max
      ),
      linewidth = errorbar_thickness,
      width = errorbar_width,
      position = ggplot2::position_dodge(width = dodge_width),
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      color = point_border_color,
      stroke = point_border_thickness,
      position = ggplot2::position_dodge(width = dodge_width)
    ) +
    ggplot2::scale_fill_manual(name = legend_title, values = point_color) +
    size_scale +
    ggplot2::scale_shape_manual(values = c(circle = 21, square = 22, triangle = 24, diamond = 23)) +
    ggplot2::scale_color_manual(values = errorbar_color) +
    ggplot2::guides(
      fill = legend,
      color = no_legend,
      shape = no_legend,
      size = no_legend
    ) +
    continuous_axis(
      axis = "x",
      scale = x_axis_scale,
      title = x_axis_title,
      limits = x_axis_limits,
      breaks = x_axis_breaks,
      labels = x_axis_labels,
      include = x_axis_include,
      expand_lower = expand_lower,
      expand_upper = expand_upper,
      cap = cap
    ) +
    ggplot2::scale_y_discrete(
      #sec.axis = ggplot2::dup_axis(
      #  #name = NULL,
      #  labels = y_labeller
      #),
      name = NULL,
      labels = y_axis_labels
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_vip(base_size = base_size, aspect_ratio = aspect_ratio, ...) +
    ggplot2::theme(
      axis.ticks.y = blank,
      axis.line.y = blank,
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = -2.5, b = 0, l = 0),
        #margin = ggplot2::margin(t = 0, r = 2.5, b = 0, l = 0),
        size = y_axis_label_size,
        hjust = hjust,
        color = y_axis_label_color
      ),
      axis.line.x = ggplot2::element_line(linewidth = axis_line_thickness),
      axis.ticks.x = ggplot2::element_line(linewidth = axis_line_thickness),
      axis.ticks.length.x = ggplot2::unit(3.75, units = "pt"),
      plot.margin = ggplot2::margin(r = 5, unit = "mm")
    )
  p
}

#' Alternating shaded background to create zebra stripe appearance
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @param trim If `TRUE` (default), stripes trimmed to axis limits. If `FALSE` (default), stripes extend to data limits
#' @param direction Options: `"horizontal"` (each stripe extends from left to right), `"vertical"` (each stripe extends from top to bottom). If `NULL`, direction guessed from variable types for x and y axis
#' @returns ggproto object
#' @export
geom_stripes <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    ...,
    show.legend = NA,
    inherit.aes = TRUE,
    trim = TRUE,
    direction = NULL) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStripes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      direction = direction,
      ...
    )
  )
}

#' Geom for alternating background zebra stripes
#'
#' @export
GeomStripes <- ggplot2::ggproto(
  "GeomStripes",
  ggplot2::Geom,
  required_aes = "x|y",
  default_aes = ggplot2::aes(
    xmin = -Inf,
    xmax = Inf,
    odd = "#22222222",
    even = "#00000000",
    # Change 'linewidth' below from 0 to NA. When not NA then when *printing in pdf device* borders are there despite requested 0th size. Seems to be some ggplot2 bug caused by grid overriding an lwd parameter somewhere, unless the linewidth is set to NA. Found solution here https://stackoverflow.com/questions/43417514/getting-rid-of-border-in-pdf-output-for-geom-label-for-ggplot2-in-r
    alpha = NA,
    colour = "black",
    linetype = "solid",
    linewidth = NA
  ),
  draw_key = ggplot2::draw_key_blank,
  setup_params = function(data, params) {
    if (is.null(params$direction)) {
      if (is.numeric(data$y) && !is.integer(utils::type.convert(data$y, as.is = TRUE))) {
        params$direction <- "vertical"
      } else {
        params$direction <- "horizontal"
      }
    }
    z <- if (params$direction == "horizontal") "y" else "x"
    params$n_breaks <- length(unique(data[[z]]))
    params
  },
  draw_panel = function(data, panel_params, coord, trim = TRUE, direction = "horizontal", n_breaks = NULL, odd = NULL, even = NULL) {
    if (direction == "horizontal") {
      axis <- "x"
      axis_min <- "xmin"
      axis_max <- "xmax"
      position <- "y.sec"
      opposite <- "y"
      opposite_min <- "ymin"
      opposite_max <- "ymax"
    } else {
      axis <- "y"
      axis_min <- "ymin"
      axis_max <- "ymax"
      position <- "x.sec"
      opposite <- "x"
      opposite_min <- "xmin"
      opposite_max <- "xmax"
    }
    z <- seq_len(n_breaks %||% attributes(panel_params[[position]]$breaks)$pos)
    data <- merge(data, vec2df(opposite = z, .col_names = opposite), by = opposite, all = TRUE, sort = FALSE)
    for (i in c(axis_min, axis_max, "odd", "even", "alpha", "colour", "linetype", "linewidth")) {
      col_values <- .subset2(data, i)
      idx_na <- is.na(col_values)
      data[[i]] <- col_values[!idx_na][1L]
      #data[[i]][idx_na] <- col_values[!idx_na][1L]
    }
    if (trim) {
      #limits <- panel_params[[axis]]$get_limits()
      limits <- range(panel_params[[axis]]$get_breaks(), na.rm = TRUE)
      data[[axis_min]] <- limits[1L]
      data[[axis_max]] <- limits[2L]
    }
    data[[opposite]] <- z <- round(data[[opposite]])
    data[[opposite_min]] <- z - 0.5
    data[[opposite_max]] <- z + 0.5
    data <- data[c("xmin", "xmax", "ymin", "ymax", "odd", "even", "alpha", "colour", "linetype", "linewidth")]
    data <- data[!duplicated(data), , drop = FALSE]
    data <- data[order(data[[opposite_min]]), , drop = FALSE]
    n_rows <- nrow(data)
    is_odd <- seq_len(n_rows) %% 2L == 1L
    if (!is.null(even)) {
      data$even[!is_odd] <- rep_len(even, length.out = sum(!is_odd))
    }
    if (!is.null(odd)) {
      data$odd[is_odd] <- rep_len(odd, length.out = sum(is_odd))
    }
    data$fill <- data$even
    data$fill[is_odd] <- data$odd[is_odd]
    data$odd <- data$even <- NULL
    data$colour <- data$fill
    ggplot2::GeomRect$draw_panel(data, panel_params, coord)
  }
)

#' Clip data limits for forest plot without influencing estimate and 95% CI labels
#'
#' @noRd
clip_forest_x_limits <- function(plot, min = NULL, max = NULL, idx_patchwork = 2) {
  replace_plot_data <- function(.x, .min = NULL, .max = NULL) {
    data <- .x@data
    if (!is.null(.min)) {
      data$.x_min[!is.na(data$.x_min) & data$.x_min < .min] <- .min
      data$.x[!is.na(data$.x) & data$.x < .min] <- .min
    }
    if (!is.null(.max)) {
      data$.x_max[!is.na(data$.x_max) & data$.x_max > .max] <- .max
      data$.x[!is.na(data$.x) & data$.x > .max] <- .max
    }
    .x@data <- data
    .x
  }
  if (inherits(plot, "patchwork")) {
    idx_patchwork <- idx_patchwork %||% seq_along(plot)
    for (i in idx_patchwork) {
      plot[[i]] <- replace_plot_data(plot[[i]], .min = min, .max = max)
    }
    plot
  } else if (inherits(plot, "ggplot")) {
    replace_plot_data(plot, .min = min, .max = max)
  } else {
    lapply(plot, clip_forest_x_limits, min = min, max = max, idx_patchwork = idx_patchwork)
  }
}

#' Reorder y axis levels of forest plot
#'
#' @noRd
reorder_y_axis <- function(plot, ..., .y = c(".y_var", ".study_label"), .first = "Pooled") {
  .relevel <- function(.x) {
    for (i in intersect(.y, names(.x))) {
      .x[[i]] <- factor(.subset2(.x, i), levels = unique(c(.first, as.character(.subset2(.x, i)))))
    }
    .x
  }
  if (inherits(plot, "patchwork")) {
    for (i in seq_along(plot)) {
      plot[[i]]@data <- .relevel(dplyr::arrange(plot[[i]]@data, ...))
    }
    plot
  } else if (inherits(plot, "ggplot")) {
    plot@data <- .relevel(dplyr::arrange(plot@data, ...))
    plot
  } else {
    lapply(plot, reorder_y_axis, ..., .y = .y, .first = .first)
  }
}
