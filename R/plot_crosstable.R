#' Plot cross table displaying the number of studies in a given domain for every virus (or vaccine product)
#'
#' @param df Data frame
#' @param x,y Categorical variables that will form x and y axis, respectively. Enter as length 1 character vectors
#' @param color_var Variable used to determine fill color of table. Default is `"n"`. Enter as length 1 character vectors
#' @param color_min,color_max Minimum and maximum values for continuous color scale. Enter as length 1 character vectors of hexadecimal codes or color names
#' @param x_axis_label_angle Angle for x axis tick labels
#' @param x_axis_title Title for x axis
#' @param x_axis_pos Position for x axis labels. Default is `"top"`
#' @param y_axis_title Title for y axis
#' @param font_size Font size for number of studies
#' @param label_size Font size of row/column titles
#' @param na.rm If `TRUE`, missing levels are removed from factor variables
#' @param show_missing_x,show_missing_y,show_empty_levels If `TRUE`, all levels of axis variables will be displayed. If `FALSE`, only levels with studies will be displayed in plot
#' @param show_zeros If `TRUE`, cells with no studies will be displayed as "0". If `FALSE`, no 0 is displayed is these cells
#' @param show_legend If `TRUE`, legend is displayed. Default is `FALSE`
#' @param legend_title Title for legend
#' @param facet Facet call. For example, calls to `ggplot2::facet_wrap` or `ggplot2::facet_grid`
#' @param aspect_ratio Aspect ratio for plot
#' @param plot_margin Plot margins
#' @param ... Not used
#' @returns ggplot object
#' @export
plot_crosstable <- function(
    df,
    x = "virus",
    y = "domain",
    color_var = "n",
    color_min = "#FFFFFF",
    color_max = "#A63B86",
    facet = NULL,
    na.rm = TRUE,
    show_missing_x = FALSE,
    show_missing_y = FALSE,
    show_empty_levels = TRUE,
    show_zeros = FALSE,
    show_legend = FALSE,
    legend_title = "No. of studies",
    x_axis_pos = "top",
    x_axis_label_angle = 45,
    x_axis_title = NULL,
    y_axis_title = NULL,
    label_size = 18,
    font_size = 16,
    plot_margin = NULL,
    aspect_ratio = NULL,
    ...) {
  if (!is.null(facet)) {
    facet_vars <- facet$params
    facet_vars <- c(names(facet_vars$rows), names(facet_vars$cols))
  }
  df$.x <-if (is.null(x)) "" else df[[x]]
  df$.y <-if (is.null(x)) "" else df[[y]]
  x <- x %||% ""
  if (!show_missing_x) {
    df <- df[!is.na(df$.x), , drop = FALSE]
  }
  if (!show_missing_y) {
    df <- df[!is.na(df$.y), , drop = FALSE]
  }
  if (show_empty_levels) {
    df <- dplyr::count(df, .data$.x, .data$.y)
    if (x == "virus") {
      df$.x <- as.character(df$.x)
      for (i in setdiff(c("COVID", "RSV", "Influenza"), unique(df$.x))) {
        df <- dplyr::bind_rows(df, df[1L, ])
        z <- nrow(df)
        df$.x[z] <- i
        df$n[z] <- NA_integer_
      }
      df$.x <- factor(df$.x, levels = c("COVID", "RSV", "Influenza"))
    }
    if (y == "virus") {
      df$.y <- as.character(df$.y)
      for (i in setdiff(c("COVID", "RSV", "Influenza"), unique(df$.y))) {
        df <- dplyr::bind_rows(df, df[1L, ])
        z <- nrow(df)
        df$.y[z] <- i
        df$n[z] <- NA_integer_
      }
      df$.y <- factor(df$.y, levels = c("COVID", "RSV", "Influenza"))
    }
  } else {
    df <- droplevels(df)
    df <- dplyr::count(df, .data$.x, .data$.y)
  }
  df$perc_rank <- dplyr::percent_rank(df$n)
  if (color_min != color_max) {
    df$text_color <- ifelse(df$perc_rank > 0.97, "white", "black")
  } else {
    df$text_color <- "black"
  }
  if (!show_zeros) {
    df$text_color[df$n == 0] <- "white"
  }
  n_rows <- length(unique(df$.y))
  n_cols <- length(unique(df$.x))
  if (is.null(aspect_ratio)) {
    aspect_ratio <- n_rows/n_cols
    if (max(n_rows, n_cols) > 10L) {
      aspect_ratio <- if (aspect_ratio > 1) aspect_ratio - 1 else aspect_ratio*1.1
    }
  }
  plot_margin <- plot_margin %||% if (aspect_ratio > 2) ggplot2::margin(t = 10, r = 30, b = 10) else if (aspect_ratio > 0.5) ggplot2::margin(t = 0, r = 40, b = 10) else NULL
  blank <- ggplot2::element_blank()
  theme_axis_text <- ggplot2::element_text(color = "black", size = label_size)
  theme_x_axis_text <- if (x_axis_label_angle == 0) {
    theme_axis_text
  } else if (x_axis_pos == "top") {
    ggplot2::element_text(angle = x_axis_label_angle, vjust = 0, hjust = 0)
  } else {
    ggplot2::element_text(angle = x_axis_label_angle, vjust = 0.5, hjust = 0.5)
  }

  # Colors
  df$.fill <- if (color_var != "n") {
  } else if (any(names(df) == color_var)) {
    df[[color_var]]
  } else {
    df$n
  }

  # Plot
  ggplot2::ggplot(df, mapping = ggplot2::aes(x = .data$.x, y = .data$.y, fill = .data$.fill)) +
    ggplot2::geom_tile(show.legend = show_legend) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = .data$n, color = ggplot2::after_scale(.clr_text(fill))),
      size = font_size/2.845276,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_gradientn(
      name = legend_title,
      colours = grDevices::colorRampPalette(c(color_min, color_max))(50),
      na.value = color_min
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_discrete(name = x_axis_title, expand = c(0, 0), position = x_axis_pos) +
    ggplot2::scale_y_discrete(name = y_axis_title, expand = c(0, 0)) +
    ggplot2::geom_hline(yintercept = seq.int(0.5, n_rows + 0.5, by = 1L), linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = seq.int(0.5, n_cols + 0.5, by = 1L), linewidth = 0.5) +
    ggplot2::theme(
      axis.ticks = blank,
      axis.text = theme_axis_text,
      axis.text.x = theme_x_axis_text,
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.background = blank,
      panel.grid = blank,
      axis.line = blank,
      plot.margin = plot_margin,
      aspect.ratio = aspect_ratio
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    facet
}

#' Version of `plot_crosstable` that uses separate colors for each virus
#'
#' @inheritParams plot_crosstable
#' @param colors Named vector of colors for each virus
#' @returns ggplot object
#' @export
plot_crosstable2 <- function(
    df,
    x = "virus",
    y = "domain",
    colors = c(COVID = "#619150", RSV = "#366895", Influenza = "#F1C232"),
    na.rm = TRUE,
    show_missing_x = FALSE,
    show_missing_y = FALSE,
    show_empty_levels = TRUE,
    show_zeros = FALSE,
    show_legend = FALSE,
    legend_title = "No. of studies",
    x_axis_pos = "top",
    x_axis_label_angle = 45,
    x_axis_title = NULL,
    y_axis_title = NULL,
    label_size = 18,
    font_size = 16,
    plot_margin = NULL,
    aspect_ratio = NULL,
    ...) {
  if (any(names(df) == "vax_product") && (identical(x, "vax_product") || identical(y, "vax_product"))) {
    if (is.null(plot_margin) && identical(x, "vax_product")) {
      plot_margin <- ggplot2::margin(r = 50)
    }
    df <- df[df$vax_product != "Other", , drop = FALSE]
  }
  df$.x <- if (is.null(x))  "" else df[[x]]
  df$.y <- if (is.null(y)) "" else df[[y]]
  x <- x %||% ""
  if (!show_missing_x) {
    df <- df[!is.na(df$.x), , drop = FALSE]
  }
  if (!show_missing_y) {
    df <- df[!is.na(df$.y), , drop = FALSE]
  }
  if (show_empty_levels) {
    df <- dplyr::count(df, .data$virus, .data$.x, .data$.y)
    df$virus <- as.character(df$virus)
    for (i in setdiff(c("COVID", "RSV", "Influenza"), unique(df$virus))) {
      df <- dplyr::bind_rows(df, df[1L, ])
      z <- nrow(df)
      df$virus[z] <- i
      if (x == "virus") {
        df$.x[z] <- i
      }
      if (y == "virus") {
        df$.y[z] <- i
      }
      df$n[z] <- NA_integer_
    }
    df$virus <- factor(df$virus, levels = c("COVID", "RSV", "Influenza"))
    if (x == "virus") {
      df$.x <- factor(df$.x, levels = c("COVID", "RSV", "Influenza"))
    }
    if (y == "virus") {
      df$.y <- factor(df$.y, levels = c("COVID", "RSV", "Influenza"))
    }
  } else {
    df <- droplevels(df)
    df <- dplyr::count(df, .data$virus, .data$.x, .data$.y)
  }
  #df$alpha <- dplyr::percent_rank(df$n)
  n <- df$n
  df$alpha <- (n - mean(n, na.rm = TRUE))/stats::sd(n, na.rm = TRUE)
  df$alpha <- fill_alpha <- scales::rescale(df$alpha, to = c(0.1, 1))
  fill_alpha[is.na(df$n)] <- df$alpha[is.na(df$n)] <- 0
  n_rows <- length(unique(df$.y))
  n_cols <- length(unique(df$.x))
  if (is.null(aspect_ratio)) {
    aspect_ratio <- n_rows/n_cols
    if (max(n_rows, n_cols) > 10L) {
      aspect_ratio <- if (aspect_ratio > 1) aspect_ratio - 1 else aspect_ratio*1.1
    }
  }
  plot_margin <- plot_margin %||% if (aspect_ratio > 2) ggplot2::margin(t = 10, r = 30, b = 10) else if (aspect_ratio > 0.5)  ggplot2::margin(t = 0, r = 40, b = 10) else NULL
  blank <- ggplot2::element_blank()
  theme_axis_text <- ggplot2::element_text(color = "black", size = label_size)
  theme_x_axis_text <- if (x_axis_label_angle == 0) {
    theme_axis_text
  } else if (x_axis_pos == "top") {
    ggplot2::element_text(angle = x_axis_label_angle, vjust = 0, hjust = 0)
  } else {
    ggplot2::element_text(angle = x_axis_label_angle, vjust = 0.5, hjust = 0.5)
  }
  df$.fill <- fill_color <- unname(colors[df$virus])
  df$text_color <- vapply(seq_along(fill_color), function(i) {
    tryElse(.clr_alpha_filter(fill_color[i], alpha = fill_alpha[i]), "#FFFFFF")
  }, character(1), USE.NAMES = FALSE)
  df$text_color <- .clr_text(df$text_color)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$.x, y = .data$.y, fill = .data$.fill, alpha = alpha)) +
    ggplot2::geom_tile(show.legend = show_legend) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n, color = .data$text_color), size = font_size/2.845276, alpha = 1, na.rm = TRUE) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_discrete(name = x_axis_title, expand = c(0, 0), position = x_axis_pos) +
    ggplot2::scale_y_discrete(name = y_axis_title, expand = c(0, 0)) +
    ggplot2::geom_hline(yintercept = seq.int(0.5, n_rows + 0.5, by = 1L), linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = seq.int(0.5, n_cols + 0.5, by = 1L), linewidth = 0.5) +
    ggplot2::theme(
      axis.ticks = blank,
      axis.text = theme_axis_text,
      axis.text.x = theme_x_axis_text,
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.background = blank,
      panel.grid = blank,
      axis.line = blank,
      plot.margin = plot_margin,
      aspect.ratio = aspect_ratio
    ) +
    ggplot2::coord_cartesian(clip = "off")
}
