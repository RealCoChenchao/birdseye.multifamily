#' rank_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT dataTableOutput
mod_rank_table_ui <- function(id){
  ns <- NS(id)
  DT::dataTableOutput(NS(id, "rank_table"))
}

#' rank_table Server Functions
#'
#' @noRd
#' @importFrom DT datatable
mod_rank_table_server <- function(id, pefm_table){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$rank_table <-
      DT::renderDataTable(datatable(pefm_table()) %>%
                            DT::formatPercentage(2:4,
                                                 2),
                          options = list(scrollX = TRUE),
                          rownames = FALSE)
  })
}

## To be copied in the UI
# mod_rank_table_ui("rank_table_1")

## To be copied in the server
# mod_rank_table_server("rank_table_1")
