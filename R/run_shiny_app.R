#rsconnect::deployApp("/Users/michaelabers/Desktop/VIP")

#' Run shiny app v1
#'
#' @export
runV1 <- function() {
  appDir <- system.file("v1", package = "VIP")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `VIP`", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
