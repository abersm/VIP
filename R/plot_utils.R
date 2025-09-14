#' Conversion factor between points and mm
#'
#' @noRd
.pt <- 72.27/25.4

#' Alpha filter
#'
#' @noRd
alpha <- function(color, alpha = 1) {
  color <- grDevices::col2rgb(color, alpha = TRUE)/255
  alpha[is.na(alpha)] <- 1
  color[] <- pmax(0, pmin(1, diag(c(1, 1, 1, alpha)) %*% color + matrix(0, nrow = 4L, ncol = ncol(color))))
  grDevices::rgb(color[1L, ], color[2L, ], color[3L, ], color[4L, ])
}

#' Convert color name to hexadecimal code
#'
#' @param x Character vector of color names or hexadecimal codes
#' @returns Character vector of hexadecimal codes with same length as input
#' @noRd
col2hex <- function(x) {
  rgb <- grDevices::col2rgb(x)/255
  grDevices::rgb(red = rgb[1L, ], green = rgb[2L, ], blue = rgb[3L, ])
}

#' Adjust text color based on color of background
#'
#' Used by`plot_crosstable`
#' @param x Background colors. Enter as character vector of hexidecimal codes or color names
#' @param light_text_color,dark_text_color Lightest and darkest allowable colors for text. Enter each as length 1 character vector of hexidecimal codes or color names
#' @returns Character vector of hexidecimal codes for text color. Note that output values will be `light_text_color` or `dark_text_color`
#' @noRd
.clr_text <- function(x, light_text_color = "white", dark_text_color = "black") {
  luminance <- function(color) {
    col_rgb <- t(grDevices::col2rgb(color))/255
    col_rgb[] <- col_rgb^2.4
    z <- as.numeric(col_rgb %*% c(0.212647813391364, 0.715179147533615, 0.0721730390750208))
    clamp <- z <= 0.022
    z[clamp] <- z[clamp] + (0.022 - z[clamp])^1.414
    z
  }
  contrast <- function(col1, col2) {
    n <- max(length(col1), length(col2))
    col1 <- rep_len(col1, length.out = n)
    col2 <- rep_len(col2, length.out = n)
    bg <- luminance(col1)
    txt <- luminance(col2)
    ratio <- (bg^0.56 - txt^0.57)*1.14
    idx <- ratio < 0.1
    ratio <- ratio - 0.027
    ratio[idx] <- 0
    rev <- bg <= txt
    ratio[rev] <- (bg[rev]^0.65 - txt[rev]^0.62)*1.14
    idx <- ratio[rev] > -0.1
    ratio[rev] <- ratio[rev] + 0.027
    ratio[idx] <- 0
    ratio[abs(bg - txt) < 0.0005] <- 0
    abs(ratio*100)
  }
  ifelse(contrast(x, dark_text_color) >= contrast(x, light_text_color), dark_text_color, light_text_color)
}

#' Create color when alpha is applied to base color
#'
#' @param color Character vector of color names or hexadecimal codes
#' @param alpha Opacity. Enter as numeric 0-1 (1 for completely opaque, 0 for completely transparent). Default is `0.8`
#' @returns Character vector of hexadecimal codes
#' @noRd
.clr_alpha_filter <- function(color, alpha = 0.8) {
  color <- col2hex(color)
  new_color <- grDevices::col2rgb(color, alpha = TRUE)
  new_color <- t(new_color + c(rep.int(1 - alpha, times = 3L), 0)*(255 - new_color))
  gsub("FF$", "", grDevices::rgb(new_color, alpha = new_color[, 4L], maxColorValue = 255))
}

#' Custom ggplot2 theme
#'
#' @param base_size Size of text in pts. Default is `16`
#' @param aspect_ratio Aspect ratio, y/x. Default is `1`
#' @param ratio Alias for `aspect_ratio`
#' @param axis_line_thickness Axis and tick thickness in mm. Default is `0.7`
#' @param x_axis_title_font_size,y_axis_title_font_size Default is `base_size + 2`
#' @param x_axis_label_angle Angle of x axis tick labels. Default is `0`
#' @param x_angle Alias for `x_axis_label_angle`
#' @param x_axis_label_hjust,x_axis_label_vjust Horizontal and vertical justification of x axis text. Default is `0.5`
#' @param x_axis_label_margin_right Margin to the right of x axis tick labels. Default is `0`
#' @param y_axis_title_angle Angle of text used for y axis title. Default is 90
#' @param x_axis_label_angle Angle for x axis tick labels
#' @param x_angle Alias for `x_axis_label_angle`
#' @param ... Arguments passed to `ggplot2::theme`
#' @returns theme object
#' @export
theme_vip <- function(
    base_size = 14,
    axis_line_thickness = 0.7,
    aspect_ratio = NULL,
    ratio = aspect_ratio,
    y_axis_title_angle = 90,
    y_axis_title_font_size = base_size + 2,
    x_axis_label_angle = 0,
    x_angle = x_axis_label_angle,
    x_axis_label_hjust = 0.5,
    x_axis_label_vjust = 0.5,
    x_axis_label_margin_right = 0,
    x_axis_title_font_size = base_size + 2,
    ...) {
  if (x_angle > 0) {
    x_axis_label_hjust <- 1
    x_axis_label_vjust <- 1
  }
  blank <- ggplot2::element_blank()
  ggplot2::theme(
    text = ggplot2::element_text(size = base_size, color = "black"),
    rect = blank,
    line = ggplot2::element_line(color = "black", linewidth = axis_line_thickness, linetype = 1, lineend = "square"),
    plot.background = blank,
    panel.grid = blank,
    panel.background = blank,
    axis.text = ggplot2::element_text(size = base_size, color = "black"),
    axis.line = ggplot2::element_line(color = "black", linewidth = axis_line_thickness, linetype = 1, lineend = "square"),
    axis.ticks = ggplot2::element_line(color = "black", linewidth = axis_line_thickness, linetype = 1, lineend = "square"),
    axis.ticks.length = ggplot2::unit(0.4*base_size, "pt"),
    axis.text.x = ggplot2::element_text(
      face = "plain",
      color = "black",
      angle = x_angle,
      size = base_size,
      margin = ggplot2::margin(t = 0.3*base_size, r = x_axis_label_margin_right, unit = "pt"),
      vjust = x_axis_label_vjust,
      hjust = x_axis_label_hjust
    ),
    axis.text.y = ggplot2::element_text(
      face = "plain",
      color = "black",
      angle = 0,
      size = base_size,
      margin = ggplot2::margin(r = 0.3*base_size, unit = "pt"),
      hjust = 1
    ),
    axis.title.x = ggplot2::element_text(
      color = "black",
      size = x_axis_title_font_size,
      angle = 0,
      margin = ggplot2::margin(t = base_size/3, unit = "pt"),
      vjust = 0.5
    ),
    axis.title.y = ggplot2::element_text(
      color = "black",
      size = y_axis_title_font_size,
      angle = 90,
      margin = ggplot2::margin(r = 0.5*base_size, unit = "pt"),
      vjust = 0.5
    ),
    aspect.ratio = aspect_ratio,
    complete = TRUE,
    ...
  )
}

#' Determine plot margin to prevent clipping of axis labels when angle is 45
#'
#' @param x Character or factor vector of labels
#' @param location Label position to accommodate. Enter as length 1 character vector. Options: "t", "b", "l", "r", or combination of these
#' @returns margin object
#' @noRd
margin_pad <- function(x, location = "tr") {
  x <- if (is.factor(x)) x else factor(x)
  x <- levels(x)
  n <- nchar(x)
  is_top <- grepl("t", location, fixed = TRUE)
  is_bottom <- grepl("b", location, fixed = TRUE)
  is_left <- grepl("l", location, fixed = TRUE)
  is_right <- grepl("r", location, fixed = TRUE)
  is_last <- is_top || is_right
  # Determine whether margins need to be padded to account for first or last level
  out <- list(t = 0, b = 0, l = 0, r = 0)
  if (is_right && is_top) {
    out$t <- 10
    out$r <- n[length(n)]*2
  }
  if (is_left && is_bottom) {
    out$b <- 10
    out$l <- 4*(n[1L] - 8)
  }
  do.call(ggplot2::margin, out)
}

#' Add a new variable to data frame used for plotting
#'
#' @param df Data frame
#' @param var Name of variable to create or convert to factor. Enter as length 1 character vector
#' @param var_name Name for new variable. Enter as length 1 character vector. If `NULL` (default), new variable name is generated by adding "." prefix to `var`
#' @param if_null Character vector to return if `var` does not refer to a column in `df`
#' @param as_fct If `TRUE`, output is coerced to a factor vector. Default is `FALSE`
#' @param ... Arguments passed to `.as_fct`
#' @returns Data frame with new column added
#' @noRd
.add_plot_var <- function(df, var, var_name = NULL, if_null = "", as_fct = FALSE, ...) {
  var_name <- var_name %||% if (is.null(var)) ".new_var" else paste0(".", var[1L])
  if (length(var) != 1L || is.null(var <- .subset2(df, var))) {
    var <- if_null
  }
  if (as_fct) {
    var <- .as_fct(var, ...)
  }
  df[[var_name]] <- var
  df
}
