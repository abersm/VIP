# Make sure VIP::core is updated (if unsure, run data_core.R file in data-raw folder before running script below)

# Load packages -----------------------------------------------------------

library(tidyr)
library(dplyr)

bias <- utils::read.csv(paste_path("VIPDataExtraction-ROBDomain_DATA_2025-09-09_1152.csv"), check.names = FALSE)
bias_labels <- utils::read.csv(paste_path("VIPDataExtraction-ROBDomain_DATA_LABELS_2025-09-09_1152.csv"), check.names = FALSE)
#names(bias) <- names(bias_labels)
names(bias) <- c("id_redcap", "ae", "ve", "coadmin", "epi")
remove(bias_labels)

# Clean up workspace
#remove()

# Export data -------------------------------------------------------------

#usethis::use_data(bias, overwrite = TRUE)
