# Update VIP::core, VIP::ve, VIP::ae, VIP::epi prior to running below
coadmin <- VIP::core
coadmin <- coadmin[grepv("^ve|^ae|^epi", names(coadmin), invert = TRUE)]
coadmin <- coadmin[coadmin$coadmin == 1, , drop = FALSE]

# Prepare comments data
comments <- VIP::comments |>
  select(-starts_with(c("ve_", "ae_", "epi_", "rob_"))) |>
  filter(id_redcap %in% .env$coadmin$id_redcap) |>
  remove_rows_all_na(-id_redcap) |>
  remove_cols_all_na()
setdiff(names(comments), names(coadmin))

# Force numeric ending to vaccine-specific columns
for (i in c("coadmin_outcome_prime", "coadmin_pop1_n", "coadmin_vax2_type", "coadmin_outcome_sec", "coadmin_vax_type", "coadmin_outcome_sec_other", "coadmin_sero", "coadmin_sero_other", "coadmin_outcome_other", "coadmin_ae_describe", "coadmin_outcome_pr_other", "coadmin_ae_yn")) {
  names(coadmin)[names(coadmin) == i] <- paste0(i, "_1")
  names(comments)[names(comments) == i] <- paste0(i, "_1")
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

# Add comments columns
setdiff(names(comments), names(coadmin))

names(comments)[-1L] <- paste0(names(comments)[-1L], "_notes")

# Add additional columns
coadmin <- coadmin |>
  remove_cols_all_na() |>
  mutate(
    article = paste(author, pubyear),
    study_period = case_when(
      is.na(date_start_year) ~ as.character(date_end_year),
      is.na(date_end_year) ~ as.character(date_start_year),
      .default = paste(date_start_year, date_end_year, sep = "-")
    )
  )
substring(coadmin$pops_in_study, 1L, 1L) <- toupper(substring(coadmin$pops_in_study, 1L, 1L))


# Reorder columns
coadmin <- coadmin |>
  select(
    id_redcap, id_covidence, covid, rsv, flu, pops_in_study, study_period, article,
    everything()
  )

# Move notes columns to be next to parent column
z <- grepv("_notes$", names(coadmin))
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
  coadmin <- move_notes_col(coadmin, i)
}

# Clean up workspace
remove(comments, move_notes_col, i, z)

# Export ------------------------------------------------------------------

usethis::use_data(coadmin, overwrite = TRUE)
