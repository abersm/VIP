# Reload data
source(system.file("data-raw", "data_core.R", package = "VIP"))
remove(list = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())
redoc()
source(system.file("data-raw", "data_ve.R", package = "VIP"))
source(system.file("data-raw", "data_ae.R", package = "VIP"))
source(system.file("data-raw", "data_epi.R", package = "VIP"))
source(system.file("data-raw", "data_coadmin.R", package = "VIP"))
source(system.file("data-raw", "data_domain.R", package = "VIP"))
remove(list = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())
redoc()

# Clean data
# Detect non-ASCII characters in data
tools::showNonASCIIfile("data/core.rda")
contains_non_ascii <- function(x) any(grepl("I_WAS_NOT_ASCII", iconv(as.character(x), "latin1", "ASCII", sub = "I_WAS_NOT_ASCII")))
vars_which(VIP::core, contains_non_ascii)
clean_data <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::where(is.character), function(x) iconv(x, "latin1", "ASCII", sub = "")))
}
core <- clean_data(VIP::core)
dict <- clean_data(VIP::dict)
comments <- clean_data(VIP::comments)
domain <- clean_data(VIP::domain)
ve <- clean_data(VIP::ve)
ae <- clean_data(VIP::ae)
coadmin <- clean_data(VIP::coadmin)
epi <- clean_data(VIP::epi)

# Check if identical to old version
#identical(VIP::core, core)
#identical(VIP::dict, dict)
#identical(VIP::comments, comments)
#identical(VIP::domain, domain)
#identical(VIP::ve, ve)
#identical(VIP::ae, ae)
#identical(VIP::coadmin, coadmin)
#identical(VIP::epi, epi)

# Lines below should be character(0)
vars_which(core, contains_non_ascii)
vars_which(dict, contains_non_ascii)
vars_which(comments, contains_non_ascii)
vars_which(domain, contains_non_ascii)
vars_which(ve, contains_non_ascii)
vars_which(ae, contains_non_ascii)
vars_which(coadmin, contains_non_ascii)
vars_which(epi, contains_non_ascii)

# Export data
usethis::use_data(core, overwrite = TRUE)
usethis::use_data(dict, overwrite = TRUE)
usethis::use_data(comments, overwrite = TRUE)
usethis::use_data(domain, overwrite = TRUE)
usethis::use_data(ve, overwrite = TRUE)
usethis::use_data(ae, overwrite = TRUE)
usethis::use_data(coadmin, overwrite = TRUE)
usethis::use_data(epi, overwrite = TRUE)
redoc()
