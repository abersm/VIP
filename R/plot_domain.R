plot_domain <- function(
    df = tryElse(domain),
    pop = NULL,
    color_min = "#FFFFFF",
    color_max = "#A63B86",
    show_missing_x = TRUE,
    show_missing_y = TRUE,
    show_empty_levels = TRUE,
    show_zeros = FALSE,
    show_legend = FALSE,
    legend_title = "No. of studies",
    x_axis_pos = "top",
    x_axis_label_angle = 45,
    x_axis_title = NULL,
    y_axis_title = NULL,
    font_size = 18,
    aspect_ratio = 4/3,
    ...) {
  df <- df[!is.na(df$population), , drop = FALSE]
  pop <- pop %||% unique(df$population)
  df <- df[df$population %in% pop, , drop = FALSE]
  df <- dplyr::distinct(df, .data$id_redcap, .data$virus, .data$domain, .keep_all = TRUE)
  df <- dplyr::count(df, .data$virus, .data$domain)
  blank <- ggplot2::element_blank()
  theme_axis_text <- ggplot2::element_text(color = "black", size = font_size)
  theme_x_axis_text <- if (x_axis_label_angle == 0) {
    theme_axis_text
  } else if (x_axis_pos == "top") {
    ggplot2::element_text(angle = x_axis_label_angle, vjust = 0, hjust = 0)
  } else {
    ggplot2::element_text(angle = x_axis_label_angle, vjust = 0.5, hjust = 0.5)
  }
  df$domain <- factor(df$domain, levels = rev(c("Vaccine effectiveness", "Vaccine safety", "Epidemiology", "Co-administration")))
  if (show_missing_y) {
    df <- tidyr::complete(df, .data$domain)
  }
  if (any(idx <- is.na(df$n))) {
    df$virus[idx] <- "COVID"
  }
  df$virus <- factor(df$virus, levels = c("COVID", "RSV", "Influenza"))
  if (show_missing_x) {
    df <- tidyr::complete(df, .data$virus)
  }
  n_rows <- length(unique(df$domain))
  n_cols <- length(unique(df$virus))
  if (is.null(aspect_ratio)) {
    aspect_ratio <- n_rows/n_cols
    if (max(n_rows, n_cols) > 10L) {
      aspect_ratio <- if (aspect_ratio > 1) aspect_ratio - 1 else aspect_ratio*1.1
    }
  }
  ggplot2::ggplot(df, ggplot2::aes(x = .data$virus, y = .data$domain, fill = .data$n)) +
    ggplot2::geom_tile(show.legend = show_legend) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = .data$n, color = ggplot2::after_scale(.clr_text(fill))),
      size = font_size/3,
      alpha = 1,
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
      aspect.ratio = aspect_ratio
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

plot_domain2 <- function(
    df = tryElse(domain),
    pop = NULL,
    show_missing_x = TRUE,
    show_missing_y = TRUE,
    show_empty_levels = TRUE,
    show_zeros = FALSE,
    show_legend = FALSE,
    legend_title = "No. of studies",
    x_axis_pos = "top",
    x_axis_label_angle = 45,
    x_axis_title = NULL,
    y_axis_title = NULL,
    font_size = 18,
    aspect_ratio = 4/3,
    ...) {
  colors <- c(COVID = "#619150", RSV = "#366895", Influenza = "#F1C232")
  df <- df[!is.na(df$population), , drop = FALSE]
  pop <- pop %||% unique(df$population)
  df <- df[df$population %in% pop, , drop = FALSE]
  if (!any(pop == "Immunocomp")) {
    df <- df[df$immunocomp == 0, , drop = FALSE]
  }
  df <- dplyr::distinct(df, .data$id_redcap, .data$virus, .data$domain, .keep_all = TRUE)
  df <- dplyr::count(df, .data$virus, .data$domain)
  n <- df$n
  df$alpha <- (n - mean(n, na.rm = TRUE))/stats::sd(n, na.rm = TRUE)
  df$alpha <- fill_alpha <- scales::rescale(df$alpha, to = c(0.1, 1))
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
  n_rows <- length(unique(df$domain))
  n_cols <- length(unique(df$virus))
  if (is.null(aspect_ratio)) {
    aspect_ratio <- n_rows/n_cols
    if (max(n_rows, n_cols) > 10L) {
      aspect_ratio <- if (aspect_ratio > 1) aspect_ratio - 1 else aspect_ratio*1.1
    }
  }
  df$text_color <- vapply(seq_along(fill_color), function(i) {
    .clr_alpha_filter(fill_color[i], alpha = fill_alpha[i])
  }, character(1), USE.NAMES = FALSE)
  df$text_color <- .clr_text(df$text_color)
  df$domain <- factor(df$domain, levels = rev(c("Vaccine effectiveness", "Vaccine safety", "Epidemiology", "Co-administration")))
  if (show_missing_y) {
    df <- tidyr::complete(df, .data$domain)
  }
  if (any(idx <- is.na(df$n))) {
    df$virus[idx] <- "COVID"
  }
  df$virus <- factor(df$virus, levels = c("COVID", "RSV", "Influenza"))
  if (show_missing_x) {
    df <- tidyr::complete(df, .data$virus)
  }
  ggplot2::ggplot(df, ggplot2::aes(x = .data$virus, y = .data$domain, fill = .data$.fill, alpha = alpha)) +
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
      aspect.ratio = aspect_ratio
    ) +
    ggplot2::coord_cartesian(clip = "off")
}
