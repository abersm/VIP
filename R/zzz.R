.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = system.file("assets", package = "VIP")
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("assets")
}
