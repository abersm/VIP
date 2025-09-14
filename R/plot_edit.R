#' Replace theme element
#'
#' @noRd
replace_theme_element <- function(
  theme,
  element,
  replacement = ggplot2::element_blank()
) {
  theme_elements <- names(theme)
  idx <- which(element %in% theme_elements)
  if (length(idx) == 0L) return(theme)
  if (is.list(replacement)) {
    n_replacements <- length(replacement)
  } else {
    replacement <- list(replacement)
    n_replacements <- 1L
  }
  n_elements <- length(element)
  if (n_replacements != 1L && n_replacements != n_elements) {
    stop("In 'replace_theme_element', length of 'replacement' must be 1 or length('element')")
  }
  replacement <- rep_len(replacement, length.out = n_elements)
  for (i in idx) {
    theme[[element[i]]] <- replacement[[i]]
  }
  theme
}

#' Replace component of theme element
#'
#' @noRd
replace_theme_component <- function(theme, element, component = "colour", replacement = "transparent") {
  theme_elements <- names(theme)
  idx <- which(element %in% theme_elements)
  if (length(idx) == 0L) return(theme)
  n_components <- length(component)
  if (is.list(replacement)) {
    n_replacements <- length(replacement)
  } else {
    replacement <- list(replacement)
    n_replacements <- 1L
  }
  n_elements <- length(element)
  if ((n_components != 1L && n_components != n_elements) || (n_replacements != 1L && n_replacements != n_elements)) {
    stop("In 'replace_theme_component', lengths of 'component' and 'replacement' must be 1 or length('element')")
  }
  replacement <- rep_len(replacement, length.out = n_elements)
  component <- rep_len(component, length.out = n_elements)
  for (i in idx) {
    j <- element[i]
    z <- theme[[j]]
    if (inherits(z, "element_blank")) next
    theme[[j]][[component[i]]] <- replacement[[i]]
  }
  theme
}
