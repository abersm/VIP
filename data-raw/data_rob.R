# Make sure VIP::core is updated (if unsure, run data_core.R file in data-raw folder before running script below)

# Load packages -----------------------------------------------------------

library(tidyr)
library(dplyr)

bias <- utils::read.csv(system.file("data-raw", "redcap_rob_domain.csv", package = "VIP"), check.names = FALSE)
bias_labels <- utils::read.csv(system.file("data-raw", "redcap_rob_domain_labels.csv", package = "VIP"), check.names = FALSE)
#names(bias) <- names(bias_labels)
names(bias) <- c("id_redcap", "ae", "ve", "coadmin", "epi")
remove(bias_labels)

# Export data -------------------------------------------------------------

#usethis::use_data(bias, overwrite = TRUE)
