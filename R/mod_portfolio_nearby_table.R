#' portfolio_nearby_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_portfolio_nearby_table_ui <- function(id){
  ns <- NS(id)
  tableOutput(NS(id, "pefm_analytics_kable"))
}

#' portfolio_nearby_table Server Functions
#'
#' @noRd
mod_portfolio_nearby_table_server <- function(id, selected_analytics){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$pefm_analytics_kable <- function() {
      format_property_pefm_info(
        selected_analytics()
      )
    }

  })
}

## To be copied in the UI
# mod_portfolio_nearby_table_ui("portfolio_nearby_table_1")

## To be copied in the server
# mod_portfolio_nearby_table_server("portfolio_nearby_table_1")
