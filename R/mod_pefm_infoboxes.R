#' pefm_infoboxes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pefm_infoboxes_ui <- function(id){
  ns <- NS(id)
  Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = 200),
    mod_dollar_infobox_ui(NS(id, "rent")),
    mod_pct_infobox_ui(NS(id, "occupancy")),
    mod_dollar_infobox_ui(NS(id, "revenue"))
  )
}

#' pefm_infoboxes Server Functions
#'
#' @noRd
mod_pefm_infoboxes_server <- function(id, pefm_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_pct_infobox_server("occupancy", "Occupancy", pefm_df()$mean_occupancy)
    mod_dollar_infobox_server("rent", "Effective Rent", pefm_df()$mean_effective_rent_per_sq_ft)
    mod_dollar_infobox_server("revenue", "Revenue Per Unit", pefm_df()$mean_revenue_per_unit)
  })
}

## To be copied in the UI
# mod_pefm_infoboxes_ui("pefm_infoboxes_1")

## To be copied in the server
# mod_pefm_infoboxes_server("pefm_infoboxes_1")
