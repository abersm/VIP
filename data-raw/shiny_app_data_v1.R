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
df_shiny <- left_join(df_shiny, VIP::pub_info, by = "id_covidence")

# Export data -------------------------------------------------------------

usethis::use_data(df_shiny, overwrite = TRUE)
