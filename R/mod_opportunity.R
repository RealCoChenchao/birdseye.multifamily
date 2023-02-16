#' opportunity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_opportunity_ui <- function(id){
  ns <- NS(id)
  Pivot(
    PivotItem(headerText = "Opportunity Map",  mod_opportunity_map_ui(NS(id, "map"))),
    PivotItem(headerText = "Opportunity Table", mod_opportunity_table_ui(NS(id, "table")))
  )
}

#' opportunity Server Functions
#'
#' @noRd
mod_opportunity_server <- function(id, selected_opportunity){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_opportunity_map_server("map", reactive({selected_opportunity()}))
    mod_opportunity_table_server("table", reactive({selected_opportunity()}))
  })
}

## To be copied in the UI
# mod_opportunity_ui("opportunity_1")

## To be copied in the server
# mod_opportunity_server("opportunity_1")
