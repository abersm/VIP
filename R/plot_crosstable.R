#' Plot cross table displaying the number of studies in a given domain for every virus (or vaccine product)
#'
#' @param df Data frame
#' @param x,y Categorical variables that will form x and y axis, respectively. Enter as length 1 character vectors
#' @param color_var Variable used to determine fill color of table. Default is `"n"`. Enter as length 1 character vectors
#' @param color_min,color_max Minimum and maximum values for continuous color scale. Enter as length 1 character vectors of hexadecimal codes or color names
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
    font_size = 18,
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
    df <- dplyr::count(df, .x, .y)
    if (x == "virus") {
      df$.x <- as.character(df$.x)
      for (i in setdiff(c("COVID", "RSV", "Influenza"), unique(df$.x))) {
        df <- bind_rows(df, df[1L, ])
        z <- nrow(df)
        df$.x[z] <- i
        df$n[z] <- NA_integer_
      }
      df$.x <- factor(df$.x, levels = c("COVID", "RSV", "Influenza"))
    }
    if (y == "virus") {
      df$.y <- as.character(df$.y)
      for (i in setdiff(c("COVID", "RSV", "Influenza"), unique(df$.y))) {
        df <- bind_rows(df, df[1L, ])
        z <- nrow(df)
        df$.y[z] <- i
        df$n[z] <- NA_integer_
      }
      df$.y <- factor(df$.y, levels = c("COVID", "RSV", "Influenza"))
    }
  } else {
    df <- droplevels(df)
    df <- dplyr::count(df, .x, .y)
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
  theme_axis_text <- ggplot2::element_text(color = "black", size = font_size)
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
      size = font_size/3,
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

plot_crosstable2 <- function(
    df,
    x = "virus",
    y = "domain",
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
    font_size = 18,
    plot_margin = NULL,
    aspect_ratio = NULL,
    ...) {
  if (any(names(df) == "vax_product") && (identical(x, "vax_product") || identical(y, "vax_product"))) {
    if (identical(x, "vax_product")) {
      plot_margin <- ggplot2::margin(r = 50)
    }
    df <- df[df$vax_product != "Other", , drop = FALSE]
  }
  #colors <- c(COVID = "#D9EAD3", RSV = "#CFE2F3", Influenza = "#FFF2CC")
  colors <- c(COVID = "#619150", RSV = "#366895", Influenza = "#F1C232")
  df$.x <- if (is.null(x))  "" else df[[x]]
  df$.y <- if (is.null(x)) "" else df[[y]]
  x <- x %||% ""
  if (!show_missing_x) {
    df <- df[!is.na(df$.x), , drop = FALSE]
  }
  if (!show_missing_y) {
    df <- df[!is.na(df$.y), , drop = FALSE]
  }
  if (show_empty_levels) {
    df <- dplyr::count(df, virus, .x, .y)
    df$virus <- as.character(df$virus)
    for (i in setdiff(c("COVID", "RSV", "Influenza"), unique(df$virus))) {
      df <- bind_rows(df, df[1L, ])
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
    df <- dplyr::count(df, virus, .x, .y)
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
  theme_axis_text <- ggplot2::element_text(color = "black", size = font_size)
  theme_x_axis_text <- if (x_axis_label_angle == 0) {
    theme_axis_text
  } else if (x_axis_pos == "top") {
    ggplot2::element_text(angle = x_axis_label_angle, vjust = 0, hjust = 0)
  } else {
    ggplot2::element_text(angle = x_axis_label_angle, vjust = 0.5, hjust = 0.5)
  }
  df$.fill <- fill_color <- unname(colors[df$virus])
  df$text_color <- vapply(seq_along(fill_color), function(i) {
    tryElse(abers::clr_alpha_filter(fill_color[i], alpha = fill_alpha[i]), "#FFFFFF")
  }, character(1), USE.NAMES = FALSE)
  df$text_color <- .clr_text(df$text_color)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$.x, y = .data$.y, fill = .data$.fill, alpha = alpha)) +
    ggplot2::geom_tile(show.legend = show_legend) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n, color = .data$text_color), size = font_size/3, alpha = 1, na.rm = TRUE) +
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
