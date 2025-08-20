#' Raw data from RedCAP
#'
#' See df_core.R file in data-raw folder for reproducible workflow
#' 1 row per study
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"core"

#' Vaccine effectiveness data
#'
#' 1 row for each combination of patient population, vaccine product, and VE outcome per study
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"ve"

#' Vaccine safety (adverse event) data
#'
#' 1 row for each combination of patient population, vaccine product, and AE outcome per study
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"ae"

#' Epidemiological data for SARS-CoV2, RSV, and Influenza
#'
#' 1 row for each combination of patient population and virus per study
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"epi"

#' Data dictionary
#'
#' @format Data frame
#' \describe{
#'   \item{original}{Variable name in csv file directly exported from RedCAP}
#'   \item{new}{Variable name in `core`}
#'   \item{description}{Variable definition used in RedCAP entry form}
#'   }
"dict"
