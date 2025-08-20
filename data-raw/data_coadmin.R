# Update VIP::core, VIP::ve, VIP::ae, VIP::epi prior to running below
coadmin <- VIP::core
coadmin <- coadmin[grepv("^ve|^ae|^epi", names(coadmin), invert = TRUE)]
coadmin <- coadmin[coadmin$coadmin == 1, , drop = FALSE]

# Force numeric ending to vaccine-specific columns
for (i in c("coadmin_outcome_prime", "coadmin_pop1_n", "coadmin_vax2_type", "coadmin_outcome_sec", "coadmin_vax_type", "coadmin_outcome_sec_other", "coadmin_sero", "coadmin_sero_other", "coadmin_outcome_other", "coadmin_ae_describe", "coadmin_outcome_pr_other", "coadmin_ae_yn")) {
  names(coadmin)[names(coadmin) == i] <- paste0(i, "_1")
}

remove(i)

coadmin <- coadmin |>
  rowwise() |>
  mutate(
    pops_in_study = paste("infant"[infant], "child"[child], "adult"[adult], "elder"[elder], "pregnant"[preg], "immunocomp"[immunocomp], "", sep = "/")
  ) |>
  ungroup()

coadmin$pops_in_study <- gsub("NA", "", coadmin$pops_in_study)
coadmin$pops_in_study <- gsub("[/]+", "/", coadmin$pops_in_study)
coadmin$pops_in_study <- gsub("^/|/$", "", coadmin$pops_in_study)
coadmin$pops_in_study[coadmin$pops_in_study == ""] <- NA_character_

if (FALSE) {
  # Identify coadmin columns not associated with a specific vaccine
  grepv("coadmin", grepv("coadmin_(.*)+_[0-9]+", names(coadmin), invert = TRUE))

  # Merge data
  vax_cols <- grepv("_[0-9]+$", grepv("coadmin_", names(coadmin)))
  df_main_cols <- coadmin[setdiff(names(coadmin), vax_cols)]
  df_vax_cols <- coadmin[vax_cols]

  vax_suffix <- regmatches(vax_cols, gregexpr("_[0-9]+$", vax_cols))
  all(lengths(vax_suffix) == 1L)
  vax_suffix <- unlist(vax_suffix, use.names = FALSE)

  # Loop through vax/outcome endings
  coadmin_long <- lapply(unique(vax_suffix), function(x) {
    out <- cbind(
      df_main_cols,
      df_vax_cols[vax_cols[endsWith(vax_cols, x)]]
    )
    names(out) <- gsub(paste0(x, "$"), "", names(out))
    out
  })

  coadmin_long <- bind_rows(coadmin_long)

  # Clean up workspace
  remove(df_main_cols, df_vax_cols, vax_cols, vax_suffix)
}

# Export ------------------------------------------------------------------

usethis::use_data(coadmin, overwrite = TRUE)
