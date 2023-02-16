#' opportunity_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_opportunity_table_ui <- function(id){
  ns <- NS(id)
  ns <- NS(id)
  DT::dataTableOutput(NS(id, "opportunity_table"))
}

#' opportunity_table Server Functions
#'
#' @noRd
mod_opportunity_table_server <- function(id, pefm_table){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$opportunity_table <-
      DT::renderDataTable(datatable(pefm_table()) %>%
                            DT::formatPercentage(c("Rent Growth %",
                                                   "Occupancy Change %",
                                                   "Revenue Per Unit Growth %"),
                                                 2),
                          options = list(scrollX = TRUE),
                          rownames = FALSE)
  })
}

## To be copied in the UI
# mod_opportunity_table_ui("opportunity_table_1")

## To be copied in the server
# mod_opportunity_table_server("opportunity_table_1")
