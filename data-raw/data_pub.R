library(dplyr)
pub_info <- utils::read.csv(system.file("data-raw", "covidence_pmid.csv", package = "VIP"), check.names = FALSE)
pub_info <- pub_info |>
  mutate(
    across(where(is.character), function(x) {
      x <- ifelse(x == "", NA_character_, x)
      iconv(x, from = "", to = "ASCII", sub = "")
    }),
    #id_covidence = ifelse(id_covidence == "#3972", "#40038", id_covidence),
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

# Add column for redcap ID
pub_info <- pub_info |>
  left_join(select(VIP::core, id_covidence, id_redcap), by = "id_covidence") |>
  select(id_redcap, id_covidence, article, published_year, journal, pmid, pmcid, doi, link, everything())

# Check studies with missing redcap ID
if (nrow(filter(VIP::core, id_covidence %in% pub_info$id_covidence[is.na(pub_info$id_redcap)])) != 0L) {
  filter(VIP::core, id_covidence %in% pub_info$id_covidence[is.na(pub_info$id_redcap)])
  stop("1 or more studies in 'pub_info' has a missing redcap ID")
}

# Remove rows with missing redcap IDs
pub_info <- pub_info[!is.na(pub_info$id_redcap), ]

# Check that number of rows matches VIP::core
if (nrow(VIP::core) != nrow(pub_info)) {
  stop("Mismatch in number of studies included in 'core' and 'pub_info'")
}
if (!setequal(VIP::core$id_redcap, pub_info$id_redcap)) {
  stop("Mismatch in redcap IDs included in 'core' and 'pub_info'")
}
if (!setequal(VIP::core$id_covidence, pub_info$id_covidence)) {
  stop("Mismatch in covidence IDs included in 'core' and 'pub_info'")
}

# Export data -------------------------------------------------------------

usethis::use_data(pub_info, overwrite = TRUE)
