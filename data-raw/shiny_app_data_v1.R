library(dplyr)
df_shiny <- VIP::study_info |>
  filter(id_redcap != 629) |>
  select(
    id_redcap, id_covidence, article,
    virus, covid, rsv, flu,
    population, preg, infant, child, adult, elder, immunocomp,
    rob,
    published_year, journal, pmid, pmcid, doi, link, title
  )

# Next line should be TRUE
#length(unique(df_shiny$link)) == length(unique(df_shiny$id_redcap))

df_shiny <- df_shiny |>
  separate_longer_delim(cols = population, delim = "/") |>
  separate_longer_delim(cols = virus, delim = "/") |>
  mutate(
    population = case_when(
      grepl("Child|Infant", population) ~ "Pediatric",
      grepl("Adult|Elder", population) ~ "Adult",
      .default = population
    )
  ) |>
  distinct()

# Export data -------------------------------------------------------------

usethis::use_data(df_shiny, overwrite = TRUE)
