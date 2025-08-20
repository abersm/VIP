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
    col_rgb[] <- ifelse(col_rgb <= 0.03928, col_rgb/12.92, ((col_rgb + 0.055)/1.055)^2.4)
    as.numeric(col_rgb %*% c(0.2126, 0.7152, 0.0722))
  }
  contrast <- function(col1, col2) {
    r <- (luminance(col1) + 0.05)/(luminance(col2) + 0.05)
    r[r < 1] <- 1/r[r < 1]
    r
  }
  ifelse(contrast(x, light_text_color) > contrast(x, dark_text_color), light_text_color, dark_text_color)
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
    aspect_ratio = ratio,
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
    axis.ticks.length = grid::unit(0.4*base_size, "pt"),
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
