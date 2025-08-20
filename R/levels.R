#' Levels for adverse event outcome
#'
#' @noRd
.ae_outcome_levels <- unique(
  c(
    # Pregnancy
    "Gestational HTN", "Pre-eclampsia/eclampsia", "Placental abruption",
    "Stillbirth", "Miscarriage",
    "Prematurity", "SGA", "Congenital defect",
    # Immune dysregulation
    "GBS", "Myocarditis",
    # Immunothrombosis
    "ITP", "CVST", "Stroke", "MI",
    #"School absenteeism",
    tryElse(unique(ae$outcome))
  )
)

#' Levels for vaccine effectiveness outcomes
#'
#' @noRd
.ve_outcome_levels <- unique(
  c(
    "Long-COVID",
    "Test positivity",
    "Symptomatic infection",
    "Work absenteeism",
    "Medically-attended infection",
    "Hospitalization",
    "ICU admission",
    "Death",
    tryElse(setdiff(unique(ve$outcome), c("Other", "Other- non-RSV LRTIs", "Other- composite of severe, critical, and death")))
  )
)

#' Levels for viruses
#'
#' @noRd
.virus_levels <- c("COVID", "RSV", "Influenza")

#' Levels for vaccines
#'
#' @noRd
.vax_levels <- unique(
  c(
    "BNT162b2",
    "BNT162b2_XBB.1.5",
    "mRNA-1273",
    "mRNA-1273_XBB.1.5",
    "COVID - mRNA vaccines",
    "NVX-CoV2373",
    "Ad26.COV2.S",
    "Abrysvo",
    "Arexvy",
    "mRNA-1345",
    "Nirsevimab",
    "IIV",
    "LAIV",
    "Influenza - other",
    tryElse(setdiff(c(ve$vax_product, ae$vax_product), "Other"))
  )
)

#' Colors for patient populations
#'
#' @noRd
.pop_colors <- c(
  Infant = "#CDC2BE",
  Child = "#AC9A92",
  Adult = "#8B7267",
  Elder = "#6A4A3C",
  Pregnant = "#D52780",
  Immunocomp = "#4490BA"
)

#' Colors for viruses
#'
#' @noRd
.virus_colors <- c(COVID = "#619150", RSV = "#366895", Influenza = "#F1C232")
