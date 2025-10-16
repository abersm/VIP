# Redcap report: "RoB Final - rob_overall_text_domain"
# Make sure VIP::core is updated (if unsure, run data_core.R file in data-raw folder before running script below)

# Load packages -----------------------------------------------------------

library(tidyr)
library(dplyr)

rob <- utils::read.csv(system.file("data-raw", "rob_data.csv", package = "VIP"), check.names = FALSE)
# rob_labels doesn't contain any useful information
#rob_labels <- utils::read.csv(system.file("data-raw", "rob_labels.csv", package = "VIP"), check.names = FALSE)
names(rob) <- c("id_redcap", "id_covidence", "rob")
rob <- rob |>
  mutate(
    id_redcap = as.numeric(id_redcap),
    id_covidence = trimws(id_covidence),
    #id_covidence = ifelse(id_covidence == "#3972", "#40038", id_covidence),
    rob = case_when(
      rob == "LOW (0)" ~ "Low",
      rob == "MODERATE/SOME CONCERNS(1)" ~ "Moderate",
      rob == "HIGH (2)" ~ "High",
      .default = NA_character_
    )
  )

#count(rob, score, rob)

# Export data -------------------------------------------------------------

usethis::use_data(rob, overwrite = TRUE)
