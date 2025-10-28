# Upload data from each sheet
library(dplyr)
df_tables <- readxl::excel_sheets(system.file("data-raw", "manuscript_tables.xlsx", package = "VIP"))|>
  lapply(function(x) {
  out <- suppressWarnings(readxl::read_excel(system.file("data-raw", "manuscript_tables.xlsx", package = "VIP"), sheet = x, na = c("", "NA", "N/A", "na", "n/a", "?")))
  out$sheet <- x
  out
}) |>
  bind_rows()

# Clean columns
df_tables <- df_tables |>
  mutate(
    study_design = case_when(
      study_design == "Case-Control" ~ "Case-control",
      .default = study_design
    ),
    estimate_original = estimate,
    lower = gsub("(.* \\()(-?[0-9\\.]+)( to.*)", "\\2", estimate),
    upper = gsub("(.*to )(-?[0-9\\.]+)(\\))", "\\2", estimate),
    estimate = gsub("(-?[0-9\\.]+)( .*)", "\\1", estimate)
  )

if (all(is.na(df_tables$estimate) | !is.na(suppressWarnings(as.numeric(df_tables$estimate))))) {
  df_tables$estimate <- as.numeric(df_tables$estimate)
}

if (all(is.na(df_tables$lower) | !is.na(suppressWarnings(as.numeric(df_tables$lower))))) {
  df_tables$lower <- as.numeric(df_tables$lower)
}

if (all(is.na(df_tables$upper) | !is.na(suppressWarnings(as.numeric(df_tables$upper))))) {
  df_tables$upper <- as.numeric(df_tables$upper)
}

if (nrow(filter(df_tables, !is.na(estimate_original), is.na(estimate) | is.na(lower) | is.na(upper))) == 0L) {
  df_tables$estimate_original <- NULL
}

# Separate by sheet
df_tables <- lapply(split(seq_nrow(df_tables), f = df_tables$sheet), function(i) df_tables[i, , drop = FALSE])
