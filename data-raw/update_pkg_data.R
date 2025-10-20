update_file <- function(file_name) {
  message(file_name)
  remove(list = setdiff(ls(envir = globalenv(), all.names = TRUE), "update_file"), envir = globalenv())
  suppressMessages(source(system.file("data-raw", sprintf("%s.R", file_name), package = "VIP")))
  redoc(silent = TRUE)
  objects <- setdiff(ls(envir = globalenv(), all.names = TRUE), "update_file")
  on.exit(remove(list = objects, envir = globalenv()), add = TRUE)
}

lapply(
  c("data_core", "data_ve", "data_ae", "data_epi", "data_coadmin", "data_domain", "data_rob", "data_pubs", "data_studies", "shiny_app_data"),
  update_file
)

remove(update_file)
