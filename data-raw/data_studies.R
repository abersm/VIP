# NOTE: VIP::core, VIP::rob, and VIP::pub_info must be updated prior to running script below
library(dplyr)
study_info <- VIP::core |>
  filter(
    exclude == 0
  ) |>
  select(
    id_redcap, id_covidence,
    covid, rsv, flu,
    ve, ae, coadmin, epi,
    ae_is_of_interest, ae_other_ae_incl, ae_other,
    rob_domain, rob_domain_ve, rob_domain_ae, rob_domain_aensi, rob_domain_aensi_other, rob_domain_coadmin, rob_domain_epi,
    infant, child, adult, elder, preg, immunocomp,
    year_start = date_start_year, year_end = date_end_year
  ) |>
  left_join(VIP::rob, by = c("id_redcap", "id_covidence")) |>
  left_join(VIP::pub_info, by = c("id_redcap", "id_covidence")) |>
  rowwise() |>
  mutate(
    population = paste("Infant"[infant], "Child"[child], "Adult"[adult], "Elder"[elder], "Pregnant"[preg], "Immunocomp"[immunocomp], "", sep = "/")
  ) |>
  ungroup()

study_info$population <- gsub("NA", "", study_info$population)
study_info$population <- gsub("[/]+", "/", study_info$population)
study_info$population <- gsub("^/|/$", "", study_info$population)
study_info$population[study_info$population == ""] <- NA_character_

study_info <- study_info |>
  mutate(
    population = ifelse(immunocomp == 1L, "Immunocomp", population),
    peds = ifelse(infant == 1L | child == 1L, 1L, 0L),
    any_adult = ifelse(adult == 1L | child == 1L, 1L, 0L),
    virus = case_when(
      covid == 1 & rsv == 0 & flu == 0 ~ "COVID",
      covid == 0 & rsv == 1 & flu == 0 ~ "RSV",
      covid == 0 & rsv == 0 & flu == 1 ~ "Influenza",
      covid == 1 & rsv == 1 & flu == 1 ~ "COVID/RSV/Influenza",
      covid == 1 & rsv == 1 & flu == 0 ~ "COVID/RSV",
      covid == 1 & rsv == 0 & flu == 1 ~ "COVID/Influenza",
      covid == 0 & rsv == 1 & flu == 1 ~ "RSV/Influenza",
      .default = NA_character_
    ),
    ae_is_of_interest = ifelse(is.na(ae_is_of_interest), 0L, ae_is_of_interest),
    aensi = case_when(
       rob_domain == "AENSI" | !is.na(rob_domain_aensi_other) | !is.na(ae_other) | ae_other_ae_incl == 1L ~ 1L,
      ae == 1L & ae_is_of_interest == 0L ~ 1L,
      !is.na(rob_domain_aensi) ~ rob_domain_aensi,
      ae == 0L ~ 0L,
      .default = 0L
    ),
    aesi = case_when(
      rob_domain == "AESI" | rob_domain_ae == 1L ~ 1L,
      ae == 1L & ae_is_of_interest == 1L ~ 1L,
      .default = 0L
    )
  )

# Add study metadata
table_s4 <- suppressWarnings(readxl::read_excel(system.file("data-raw", "Table S4 Characteristics of included studies.xlsx", package = "VIP")))
table_s4 <- table_s4[c("id_redcap", "id_covidence", "study_design", "country", "study_setting")]

# Studies present in VIP::core but missing from table_s4
anti_join(VIP::core, select(table_s4, -c(study_design, study_setting)), by = c("id_redcap", "id_covidence"))

# Studies present in table_s4 but missing from VIP::core
anti_join(select(table_s4, -c(study_design, study_setting)), VIP::core, by = c("id_redcap", "id_covidence"))
#VIP::core |> filter(id_redcap %in% c(93, 198, 517, 143, 594))
#VIP::core |> filter(id_covidence %in% c("#1798", "#9740", "#290", "#9612", "#11175"))

# Limit table_s4 to studies included in VIP::core
table_s4 <- table_s4 |> filter(id_redcap %in% VIP::core$id_redcap)

# redcap IDs should match those in study_info
if (!setequal(table_s4$id_redcap, study_info$id_redcap)) {
  z <- setdiff(table_s4$id_redcap, study_info$id_redcap)
  if (length(z) == 0L) {
    z <- setdiff(study_info$id_redcap, table_s4$id_redcap)
  }
  stop("'study_info' and 'table_s4' do not share the same redcap IDs")
}

# covidence IDs should match those in study_info
if (!setequal(table_s4$id_covidence, study_info$id_covidence)) {
  z <- setdiff(table_s4$id_covidence, study_info$id_covidence)
  if (length(z) == 0L) {
    z <- setdiff(study_info$id_covidence, table_s4$id_covidence)
  }
  stop("'study_info' and 'table_s4' do not share the same covidence IDs")
}

# Add data from table_s4 to study_info
#intersect(names(table_s4), names(study_info))
study_info <- study_info |>
  left_join(table_s4, by = c("id_redcap", "id_covidence")) |>
  select(
    id_redcap, id_covidence, article,
    study_design, country, study_setting, year_start, year_end,
    virus, covid, rsv, flu,
    ve, ae, coadmin, epi,
    population, infant, child, adult, elder, preg, immunocomp, peds, any_adult,
    rob,
    published_year, journal, pmid, pmcid, doi, link, title,
    everything(),
    -c(ae_is_of_interest, ae_other_ae_incl, ae_other, rob_domain, rob_domain_ve, rob_domain_ae, rob_domain_aensi, rob_domain_aensi_other, rob_domain_coadmin, rob_domain_epi, aensi, aesi)
  )

remove(table_s4)


# Export data -------------------------------------------------------------

usethis::use_data(study_info, overwrite = TRUE)
