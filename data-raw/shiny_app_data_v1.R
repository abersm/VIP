library(dplyr)
#redoc()
setdiff(VIP::core$id_covidence, VIP::pub_info$id_covidence)
intersect(names(VIP::core), names(VIP::pub_info))
df_shiny <- VIP::core |>
  filter(!(id_redcap %in% c(609))) |>
  select(
    id_redcap, id_covidence,
    covid, rsv, flu,
    infant, child, adult, elder, preg, immunocomp
  )
df_shiny <- left_join(df_shiny, VIP::pub_info, by = "id_covidence") |>
  mutate(
    virus = case_when(
      covid == 1 & rsv == 0 & flu == 0 ~ "COVID",
      covid == 0 & rsv == 1 & flu == 0 ~ "RSV",
      covid == 0 & rsv == 0 & flu == 1 ~ "Influenza",
      covid == 1 & rsv == 1 & flu == 1 ~ "COVID/RSV/Influenza",
      covid == 1 & rsv == 1 & flu == 0 ~ "COVID/RSV",
      covid == 1 & rsv == 0 & flu == 1 ~ "COVID/Influenza",
      covid == 0 & rsv == 1 & flu == 1 ~ "RSV/Influenza",
      .default = NA_character_
    )
  ) |>
  rowwise() |>
  mutate(
    population = paste("Infant"[infant], "Child"[child], "Adult"[adult], "Elder"[elder], "Pregnant"[preg], "Immunocomp"[immunocomp], "", sep = "/")
  ) |>
  ungroup()

df_shiny$population <- gsub("NA", "", df_shiny$population)
df_shiny$population <- gsub("[/]+", "/", df_shiny$population)
df_shiny$population <- gsub("^/|/$", "", df_shiny$population)
df_shiny$population[df_shiny$population == ""] <- NA_character_

df_shiny <- df_shiny |>
  mutate(
    population = case_when(
      grepl("Immunocomp", population) ~ "Immunocomp",
      .default = population
    )
  ) |>
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
