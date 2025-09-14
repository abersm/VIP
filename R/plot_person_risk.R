plot_person_risk <- function(
    ...,
    size = 15,
    n_per_row = 10,
    colors = c("grey85", "#A63B86"),
    outcome_labels = c("No", "Yes"),
    outcome_name = "Infection",
    facet_color = "#246A87",
    facet_text_color = NULL,
    facet_text_size = 24,
    legend_text_size = 20,
    show_legend = TRUE,
    legend_direction = NULL,
    legend_position = NULL,
    n_col = NULL,
    ratio = NULL,
    aspect_ratio = ratio,
    expand_x = 1,
    expand_y = 1,
    coord_fn = ggplot2::coord_cartesian) {
  emojifont::load.fontawesome()
  x <- c(...)
  n <- length(x)
  idx <- seq_len(n)
  groups <- names(x) %||% LETTERS[idx]
  df <- expand.grid(.x = 1L:10L, .y = 1L:10L)
  df$.label <- emojifont::fontawesome("fa-male")
  df$.outcome <- outcome_labels[1L]
  outcome_present <- outcome_labels[2L]
  out <- vector(mode = "list", n)
  for (i in seq_len(n)) {
    out[[i]] <- df
    out[[i]]$.tx_group <- groups[i]
    out[[i]]$.outcome[seq_len(x[i])] <- outcome_present
  }
  df <- do.call(rbind, out)
  n_col <- n_col %||% n
  facet_text_color <- facet_text_color %||% .clr_text(facet_color)
  if (show_legend) {
    legend_direction_null <- is.null(legend_direction)
    legend_position_null <- is.null(legend_position)
    if (legend_direction_null && legend_position_null) {
      legend_direction <- "horizontal"
      legend_position <- "bottom"
    } else if (legend_direction_null) {
      legend_direction <- if (legend_position %in% c("bottom", "top")) "horizontal" else "vertical"
    } else if (legend_position_null) {
      legend_position <- if (legend_direction == "horizontal") "bottom" else "right"
    }
  } else {
    legend_position <- "none"
    legend_direction <- "horizontal"
  }
  blank <- ggplot2::element_blank()
  ggplot2::ggplot(df, ggplot2::aes(x = .data$.x, y = .data$.y, label = .data$.label, color = .data$.outcome)) +
    ggplot2::geom_text(family = "fontawesome-webfont", size = size) +
    ggplot2::scale_color_manual(
      name = outcome_name,
      values = colors,
      labels = outcome_labels
    ) +
    ggplot2::scale_x_continuous(expand = c(0, expand_x, 0, expand_x)) +
    ggplot2::scale_y_continuous(expand = c(0, expand_y, 0, expand_y)) +
    #ggplot2::facet_wrap(dplyr::vars(.tx_group), ncol = n_col) +
    ggplot2::facet_wrap(dplyr::vars(.data$.tx_group), ncol = n_col) +
    coord_fn(clip = "off") +
    ggplot2::theme(
      line = blank,
      axis.text = blank,
      axis.title = blank,
      panel.background = blank,
      legend.text = ggplot2::element_text(family = "fontawesome-webfont"),
      text = ggplot2::element_text(family = "sans", size = legend_text_size),
      panel.border = ggplot2::element_rect(fill = NA),
      strip.background = ggplot2::element_rect(fill = facet_color),
      strip.text = ggplot2::element_text(color = facet_text_color, size = facet_text_size),
      legend.position = legend_position,
      legend.direction = legend_direction,
      plot.margin = ggplot2::margin(5, 5, 5, 5),
      aspect.ratio = aspect_ratio
    )
}

plot_person_risk2 <- function(
    df = NULL,
    tx_group,
    outcome,
    n,
    size = 12,
    n_per_row = 10,
    colors = c("grey85", "#A63B86"),
    facet_color = "#246A87",
    facet_text_color = NULL,
    facet_text_size = 24,
    legend_text_size = 20,
    legend_labels = c("No", "Yes"),
    legend_title = "Infection",
    show_legend = TRUE,
    legend_direction = NULL,
    legend_position = NULL,
    ...) {
  emojifont::load.fontawesome()
  if (is.null(df)) {
    df <- data.frame(
      .tx_group = tx_group,
      .outcome = outcome,
      .n = n
    )
  } else {
    df$.tx_group <- df[[tx_group]]
    df$.outcome <- df[[outcome]]
    df$.n <- df[[n]]
  }
  df <- df[order(df$.n), , drop = FALSE]
  n_groups <- length(unique(df$.tx_group))
  facet_text_color <- facet_text_color %||% .clr_text(facet_color)
  if (show_legend) {
    legend_direction_null <- is.null(legend_direction)
    legend_position_null <- is.null(legend_position)
    if (legend_direction_null && legend_position_null) {
      legend_direction <- "horizontal"
      legend_position <- "bottom"
    } else if (legend_direction_null) {
      legend_direction <- if (legend_position %in% c("bottom", "top")) "horizontal" else "vertical"
    } else if (legend_position_null) {
      legend_position <- if (legend_direction == "horizontal") "bottom" else "right"
    }
  } else {
    legend_position <- "none"
    legend_direction <- "horizontal"
  }
  blank <- ggplot2::element_blank()
  ggplot2::ggplot(df, ggplot2::aes(label = .data$.outcome)) +
    waffle::geom_pictogram(
      mapping = ggplot2::aes(color = .data$.outcome, values = .data$.n),
      n_rows = n_per_row,
      size = size,
      #family = "fontawesome-webfont",
      flip = TRUE
    ) +
    waffle::scale_label_pictogram(
      name = legend_title,
      values = "male",
      labels = legend_labels
    ) +
    ggplot2::scale_color_manual(
      name = legend_title,
      values = colors,
      labels = legend_labels
    ) +
    #ggplot2::facet_wrap(dplyr::vars(.tx_group), ncol = n_groups) +
    ggplot2::facet_wrap(dplyr::vars(.data$.tx_group), ncol = n_groups) +
    ggplot2::theme(
      line = blank,
      text = ggplot2::element_text(family = "sans", size = legend_text_size),
      axis.text = blank,
      panel.background = blank,
      panel.border = ggplot2::element_rect(fill = NA),
      strip.background = ggplot2::element_rect(fill = facet_color),
      strip.text = ggplot2::element_text(color = facet_text_color, size = facet_text_size),
      legend.position = legend_position,
      legend.direction = legend_direction
    )
}
