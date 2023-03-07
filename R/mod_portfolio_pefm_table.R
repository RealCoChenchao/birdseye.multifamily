#' portfolio_pefm_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_portfolio_pefm_table_ui <- function(id){
  ns <- NS(id)
  DT::dataTableOutput(NS(id, "pefm_table"))
}

#' portfolio_pefm_table Server Functions
#'
#' @noRd
mod_portfolio_pefm_table_server <- function(id, portfolio_table){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$pefm_table <-
      DT::renderDataTable(datatable(portfolio_table(),
                                    extensions = 'Buttons',
                                    selection = 'single',
                                    options = list(
                                      dom = 'Blfrtip',
                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                    )) %>%
                            DT::formatPercentage(c("Rent 3 Month Growth %",
                                                   "Occupancy 3 Month Change %",
                                                   "Revenue Per Unit 3 Month Growth %"),
                                                 2),
                          options = list(scrollX = TRUE),
                          rownames = FALSE)
  })
}

## To be copied in the UI
# mod_portfolio_pefm_table_ui("portfolio_pefm_table_1")

## To be copied in the server
# mod_portfolio_pefm_table_server("portfolio_pefm_table_1")
