#' Plot table to horizontally adjacent to plot
#'
#' @returns ggplot object
#' @noRd
plot_column_table <- function(
    p,
    cols = NULL,
    header_face = "bold",
    header_size = NULL,
    header_color = "black",
    header_hjust = NULL,
    header_vjust = 0.5,
    label_face = "plain",
    label_size = NULL,
    label_color = "black",
    label_hjust = NULL,
    label_vjust = 0.5,
    label_parse = FALSE,
    stripes = ggplot2::waiver(),
    plot_margin = NULL) {
  if (is.null(cols)) return(NULL)
  plot_data <- p@data
  col_names <- names(plot_data)
  plot_theme <- p@theme
  if (is.list(cols)) {
    n <- length(cols)
    col_headers <- names(cols) %||% rep_len("", length.out = n)
  } else {
    cols <- list(cols)
    col_headers <- ""
    n <- 1L
  }
  idx <- seq_len(n)
  if (inherits(stripes, "waiver")) {
    stripes <- vapply(p@layers, function(x) tryElse(paste(deparse(x$constructor[[1L]]), collapse = " "), ""), character(1), USE.NAMES = FALSE)
    stripes <- list(if (any(idx_stripes <- stripes == "geom_stripes")) p@layers[idx_stripes][[1L]] else NULL)
  } else if (is.null(stripes) || inherits(stripes, "ggproto")) {
    stripes <- list(stripes)
  }
  stripes <- rep_len(stripes, length.out = n)

  # Label style
  label_size <- rep_len(label_size %||% plot_theme$axis.text.x$size %||% plot_theme$text$size, length.out = n)
  label_face <- rep_len(label_face, length.out = n)
  label_color <- rep_len(label_color, length.out = n)
  label_hjust <- rep_len(label_hjust, length.out = n)
  label_vjust <- rep_len(label_vjust, length.out = n)

  # Header style
  header_size <- rep_len(header_size %||% (label_size + 2L), length.out = n)
  header_face <- rep_len(header_face, length.out = n)
  header_color <- rep_len(header_color, length.out = n)
  header_hjust <- rep_len(header_hjust, length.out = n)
  header_vjust <- rep_len(header_vjust, length.out = n)

  # Core plot features
  plot_core <- ggplot2::ggplot(plot_data, ggplot2::aes(y = .data$.y_var)) + ggplot2::labs(y = NULL, x = "  ")
  col_names <- names(plot_data)
  #plot_margin <- plot_margin %||% ggplot2::margin(t = 0, b = 0, l = 0, r = 0)
  if (inherits(plot_margin, "margin")) {
    #plot_margin <- list(plot_margin)
  }
  #plot_margin <- rep_len(plot_margin, length.out = n)
  blank <- ggplot2::element_blank()

  # Replace plot components with transparent form
  replace_element_feature <- function(theme, component, feature = "colour") {
    theme_elements <- names(theme)
    #component_other <- setdiff(component, theme_elements)
    component_incl <- intersect(component, theme_elements)
    for (i in component_incl) {
      z <- theme[[i]]
      if (inherits(z, "element_blank")) next
      #if (inherits(z, "element_blank") || match(feature, names(z), nomatch = 0L) == 0L) next
      theme[[i]][[feature]] <- "transparent"
    }
    theme
  }
  plot_theme <- replace_element_feature(plot_theme, c("axis.line.x", "axis.ticks.x", "axis.title.x", "axis.text.x"))
  plot_theme$axis.line.x <- plot_theme$axis.ticks.x <- plot_theme$axis.title.x <- plot_theme$axis.text.x <- blank
  plot_theme$axis.line.y <- plot_theme$axis.ticks.y <- plot_theme$axis.title.y <- plot_theme$axis.text.y <- blank
  plot_theme$legend.position <- "none"

  # Loop through inputs
  label_parse <- rep_len(label_parse, length.out = n)
  label_size <- label_size/.pt
  lapply(idx, function(i) {
    col <- .subset2(cols, i)
    hjust <- label_hjust[i]
    args <- list(
      size = label_size[i],
      hjust = hjust,
      vjust = label_vjust[i],
      color = label_color[i],
      fontface = label_face[i],
      parse = label_parse[i]
    )
    if (length(col) == 1L && any(col_names == col)) {
      #args$mapping <- ggplot2::aes(y = .data$.y_var, x = 1, label = .data[[col]])
      args$mapping <- ggplot2::aes(y = .data$.y_var, label = .data[[col]])
    } else {
      #args$mapping <- ggplot2::aes(y = .data$.y_var, x = 1)
      args$mapping <- ggplot2::aes(y = .data$.y_var)
      args$label <- col
    }
    plot_theme$plot.title <- ggplot2::element_text(
      hjust = header_hjust[i],
      vjust = header_vjust[i],
      face = header_face[i],
      size = header_size[i],
      color = header_color[i]
    )
    x_scale <- ggplot2::scale_x_continuous(name = NULL, limits = c(0, 1), expand = c(0, 0, 0, 0))
    args$x <- if (is.numeric(hjust)) {
      if (hjust == 0) 0.05 else if (hjust == 1) 0.95 else 0.5
    } else {
      1
    }
    #plot_theme$plot.margin <- plot_margin[[i]]
    plot_table <- do.call(ggplot2::geom_text, args)
    plot_core +
      x_scale +
      stripes[[i]] +
      plot_table +
      ggplot2::ggtitle(col_headers[i]) +
      plot_theme
  })
}

#' Add table horizontally adjacent to plot
#'
#' @param p ggplot object
#' @param ... Arguments common to both left and right plot tables
#' @param right_cols,left_cols Character vector of column names in `p@data` to display in plot
#' @param left_args,right_args List of arguments used to generate plots to left and right of `p`
#' @param widths Widths of plots. Length must match `length(left_cols) + length(right_cols) + 1`. Default is `NULL`
#' @param plot_margin Margin for `p` and plots to right and left of `p`
#' @returns patchwork object
#' @export
add_column_table <- function(
    p,
    ...,
    left_cols = NULL,
    right_cols = NULL,
    left_args = NULL,
    right_args = NULL,
    widths = NULL,
    plot_margin = ggplot2::margin(t = 0, b = 0, r = 1, l = 1, unit = "pt")) {
  args <- list(...)
  if (...length() != 0L) {
    modify_list <- function(old, new) {
      old_names <- names(old)
      new_names <- names(new)
      new_names <- new_names[nzchar(new_names)]
      for (i in new_names) {
        old[[i]] <- if (i %in% old_names && is.list(.subset2(old, i)) && is.list(.subset2(new, i))) {
          modify_list(.subset2(old, i), .subset2(new, i))
        } else {
          .subset2(new, i)
        }
      }
      old
    }
    # left_args/right_args takes priority over input to dots
    left_args <- if (length(left_args) == 0L) args else modify_list(args, left_args)
    right_args <- if (length(right_args) == 0L) args else modify_list(args, right_args)
  }
  left_args$label_hjust <- left_args$label_hjust %||% 1
  left_args$header_hjust <- left_args$header_hjust %||% 0
  right_args$label_hjust <- right_args$label_hjust %||% 0.5
  right_args$header_hjust <- right_args$header_hjust %||% 0.5
  #p@theme$plot.margin <- plot_margin
  left_args$p <- right_args$p <- p
  left_args$cols <- left_cols
  right_args$cols <- right_cols
  right <- do.call(plot_column_table, right_args)
  left <- do.call(plot_column_table, left_args)
  titles <- lapply(c(right, left), function(x) x$theme$plot.title)
  titles <- titles[lengths(titles, use.names = FALSE) > 0L]
  title_sizes <- vapply(titles, function(x) x$size %||% 0L, numeric(1), USE.NAMES = FALSE)
  titles <- titles[[which.max(title_sizes)]]
  if (is.null(p$theme$plot.title)) {
    p@theme$plot.title <- titles
  } else {
    p@theme$plot.title$size <- titles$size
  }
  patchwork::wrap_plots(c(left, list(p), right), widths = widths)
}
