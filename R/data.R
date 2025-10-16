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

#' Co-administration data
#'
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"coadmin"

#' Epidemiological data for SARS-CoV2, RSV, and Influenza
#'
#' 1 row for each combination of patient population and virus per study
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"epi"

#' Domain data for all studies
#'
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"domain"

#' Data dictionary
#'
#' @format Data frame
#' \describe{
#'   \item{original}{Variable name in csv file directly exported from RedCAP}
#'   \item{new}{Variable name in `core`}
#'   \item{description}{Variable definition used in RedCAP entry form}
#'   }
"dict"


#' Comments in REDCap forms
#'
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"comments"

#' Risk of bias assessment
#'
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"rob"

#' Publication info
#'
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"pub_info"

#' Study meta data
#'
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"study_info"

#' Data for 1st version of shiny app
#'
#' @format Data frame
#' \describe{
#'   \item{id_redcap}{RedCAP id}
#'   }
"df_shiny"
