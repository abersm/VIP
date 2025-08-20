# Update VIP::ve, VIP::ae, VIP::epi, VIP::coadmin prior to running below
domain <- list(
  ve = select(VIP::ve, -starts_with("ve")),
  ae = select(VIP::ae, -starts_with("ae")),
  epi = select(VIP::epi, -starts_with("epi"))
)
domain$ve$domain <- "Vaccine effectiveness"
domain$ae$domain <- "Vaccine safety"
domain$epi$domain <- "Epidemiology"

domain <- lapply(domain, function(x) {
  x <- x |>
    distinct(id_redcap, domain, virus, population) |>
    filter(
      grepl("immunocomp", population, ignore.case = TRUE) | population %in% c("Pregnant", "Infant", "Child", "Infant/child", "Adult", "Elder", "Adult/elder")
    ) |>
    mutate(immunocomp = ifelse(grepl("immunocomp", population, ignore.case = TRUE), 1L, 0L)) |>
    separate_longer_delim(cols = population, delim = "/")
  substring(x$population, 1L, 1L) <- toupper(substring(x$population, 1L, 1L))
  x <- x |>
    filter(
      population %in% c("Pregnant", "Child", "Infant", "Adult", "Elder", "Immunocomp")
    ) |>
    distinct(id_redcap, domain, virus, population, immunocomp)
  x
})

domain$coadmin <- VIP::coadmin |>
  pivot_longer(
    cols = c(covid, rsv, flu),
    names_to = "virus",
    values_to = "present",
    values_drop_na = TRUE
  ) |>
  filter(present == 1) |>
  mutate(
    domain = "Co-administration",
    virus = case_when(
      virus == "covid" ~ "COVID",
      virus == "rsv" ~ "RSV",
      virus == "flu" ~ "Influenza",
      .default = NA_character_
    ),
    immunocomp = ifelse(grepl("immunocomp", pops_in_study, ignore.case = TRUE), 1L, 0L),
    population = pops_in_study
  ) |>
  distinct(id_redcap, domain, virus, population, immunocomp) |>
  separate_longer_delim(cols = population, delim = "/")
substring(domain$coadmin$population, 1L, 1L) <- toupper(substring(domain$coadmin$population, 1L, 1L))

# Shared columns
length(unique(lapply(domain, names))) == 1L

# Merge data
domain <- bind_rows(domain)

# Export ------------------------------------------------------------------

usethis::use_data(domain, overwrite = TRUE)
