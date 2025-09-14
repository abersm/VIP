# Make sure VIP::core is updated (if unsure, run data_core.R file in data-raw folder before running script below)

# Load packages -----------------------------------------------------------

library(tidyr)
library(dplyr)

# Create data frame for epidemiology studies ------------------------------

# Remove irrelevant vars
epi <- VIP::core |>
  filter(epi == 1) |>
  select(-starts_with(c("ae_", "ve_", "coadmin_")), -c(ae, ve, coadmin)) |>
  #rename(n_vaccines_studied = ve_n_vaccines_studied) |>
  mutate(
    article = paste(author, pubyear),
    study_period = case_when(
      is.na(date_start_year) ~ as.character(date_end_year),
      is.na(date_end_year) ~ as.character(date_start_year),
      .default = paste(date_start_year, date_end_year, sep = "-")
    ),
    across(where(is.character), function(x) gsub("\002", "", x, fixed = TRUE))
  )

#epi <- remove_constant_cols(epi)
epi <- epi[!vapply(epi, function(x) all(is.na(x)) || length(unique(x)) == 1L, logical(1), USE.NAMES = FALSE)]

epi |> select(starts_with("epi")) |> names() |> unique() |> sort()

# Prepare comments data
comments <- VIP::comments |>
  select(-starts_with(c("ve_", "ae_", "coadmin_", "rob_"))) |>
  filter(id_redcap %in% .env$epi$id_redcap) |>
  remove_rows_all_na(-id_redcap) |>
  remove_cols_all_na()

#setdiff(names(comments), names(epi))

comments <- comments |>
  mutate(across(-id_redcap, as.character)) |>
  pivot_longer(
    cols = starts_with("epi"),
    names_to = c("variable", "virus"),
    names_pattern = "epi_(.*)_(covid|rsv|flu)$",
    values_to = "value",
    values_drop_na = TRUE
  ) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  ) |>
  utils::type.convert(as.is = TRUE) |>
  mutate(
    virus = case_when(
      virus == "covid" ~ "COVID",
      virus == "rsv" ~ "RSV",
      virus == "flu" ~ "Influenza"
    )
  )

names(comments)[!names(comments) %in% c("id_redcap", "virus")] <- paste0(names(comments)[!names(comments) %in% c("id_redcap", "virus")], "_notes")

# Reformat and clean data -------------------------------------------------

epi <- epi |>
  #select(-c(covid, rsv, flu)) |>
  mutate(across(-id_redcap, as.character)) |>
  pivot_longer(
    cols = starts_with("epi"),
    names_to = c("variable", "virus"),
    names_pattern = "epi_(.*)_(covid|rsv|flu)$",
    values_to = "value",
    values_drop_na = TRUE
  ) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  ) |>
  utils::type.convert(as.is = TRUE) |>
  mutate(
    virus = case_when(
      virus == "covid" ~ "COVID",
      virus == "rsv" ~ "RSV",
      virus == "flu" ~ "Influenza"
    ),
    geo = gsub("U\\.S\\.|U\\.S\\.A\\.|USA|United States", "US", geo),
    us = case_when(
      grepl("Mexico|Italy", geo) ~ 0L,
      grepl("US|Mayo|Veterans", geo) ~ 1L,
      .default = NA_integer_
    )
  ) |>
  filter(
    !(virus == "COVID" & covid == 0),
    !(virus == "RSV" & rsv == 0),
    !(virus == "Influenza" & flu == 0),
    is.na(us) | us == 1L
  ) |>
  rowwise() |>
  mutate(
    pops_in_study = paste("infant"[infant], "child"[child], "adult"[adult], "elder"[elder], "pregnant"[preg], "immunocomp"[immunocomp], "", sep = "/")
  ) |>
  ungroup()

epi$pops_in_study <- gsub("NA", "", epi$pops_in_study)
epi$pops_in_study <- gsub("[/]+", "/", epi$pops_in_study)
epi$pops_in_study <- gsub("^/|/$", "", epi$pops_in_study)
epi$pops_in_study[epi$pops_in_study == ""] <- NA_character_

epi <- epi |>
  mutate(
    is_pop_other = ifelse(!is.na(population_other), 1L, 0L),
    population = ifelse(is_pop_other & !is.na(population_other), population_other, pops_in_study)
  )

substring(epi$population, 1L, 1L) <- toupper(substring(epi$population, 1L, 1L))

epi <- epi |>
  mutate(
    population = case_when(
      population %in% c("Adults 75+ years", "Long-term care facility resident or community dwelling") ~ "Elder",
      population %in% c("Adults 18+", "All adults 18+", ">=18 years") ~ "Adult/elder",
      .default = population
    )
  )

# Add comments column
#setdiff(names(comments), names(epi))
epi <- epi |>
  left_join(comments, by = c("id_redcap", "virus"))

# Reorder columns ---------------------------------------------------------

epi <- epi |>
  select(
    # Key study (article) identifiers
    id_redcap, id_covidence, reviewer, article,
    # Highly relevant columns
    virus,
    study_period, date_notes, date_start_month, date_start_year, date_end_month, date_end_year,
    study_design, us, study_setting, geo,
    pops_in_study,
    seroprev, incidence,
    medical, hospitalization, icu, ifr,
    epi_other = other,
    contains("complete"),
    # Article specific columns
    author, pubyear, title, date_start_month, date_start_year, date_end_month, date_end_year, date_notes,
    everything()
  )

# Move notes columns to be next to parent column
z <- grepv("_notes$", names(epi))
move_notes_col <- function(df, notes_col) {
  df_names <- names(df)
  col <- gsub("_notes$", "", notes_col)
  if (!any(idx1 <- df_names == col)) return(df)
  idx1 <- which(idx1)
  idx2 <- which(df_names == notes_col)
  idx_pre <- c(setdiff(seq_len(idx1), idx2), idx2)
  idx_all <- seq_len(ncol(df))
  idx <- unique(c(idx_pre, idx_all))
  df[idx]
}

for (i in z) {
  epi <- move_notes_col(epi, i)
}

# Clean up workspace
remove(comments, move_notes_col, i, z)

# Export data -------------------------------------------------------------

usethis::use_data(epi, overwrite = TRUE)
