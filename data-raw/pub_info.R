library(dplyr)
pub_info <- utils::read.csv(system.file("data-raw", "covidence_pmid.csv", package = "VIP"), check.names = FALSE)
pub_info <- pub_info |>
  mutate(
    across(is.character, function(x) {
      x <- ifelse(x == "", NA_character_, x)
      iconv(x, from = "", to = "ASCII", sub = "")
    }),
    link = case_when(
      !is.na(pmcid) ~ paste0("https://pmc.ncbi.nlm.nih.gov/articles/", pmcid),
      !is.na(pmid) ~ paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid),
      !is.na(doi) ~ paste0("https://doi.org/", doi),
      .default = link
    )
  ) |>
  rename(
    article = study
  )

# Export data -------------------------------------------------------------

usethis::use_data(pub_info, overwrite = TRUE)
