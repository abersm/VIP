# Data --------------------------------------------------------------------

ve_meta_raw <- suppressWarnings(readxl::read_excel(system.file("data-raw", "meta_raw_data.xlsx", package = "VIP")))
ve_meta <- suppressWarnings(readxl::read_excel(system.file("data-raw", "meta_results.xlsx", package = "VIP")))
ve_meta_low_rob <- suppressWarnings(readxl::read_excel(system.file("data-raw", "meta_results.xlsx", package = "VIP"), sheet = "sens_low_rob"))
ve_meta <- dplyr::bind_rows(
  ve_meta[intersect(names(ve_meta), names(ve_meta_low_rob))],
  ve_meta_low_rob[intersect(names(ve_meta), names(ve_meta_low_rob))]
)
remove(ve_meta_low_rob)

# Export data -------------------------------------------------------------

usethis::use_data(ve_meta_raw, overwrite = TRUE)
usethis::use_data(ve_meta, overwrite = TRUE)
