#' multi_barchart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_multi_barchart_ui <- function(id){
  ns <- NS(id)
  highchartOutput(NS(id, "barchart"))
}

#' multi_barchart Server Functions
#'
#' @noRd
mod_multi_barchart_server <- function(id, pefm_cut){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$barchart <- renderHighchart({
      hchart(pefm_cut(), "column",
             hcaes(x = `Data Cut`,
                   y = Performance,
                   group = Market))
    })
  })
}

## To be copied in the UI
# mod_multi_barchart_ui("multi_barchart_1")

## To be copied in the server
# mod_multi_barchart_server("multi_barchart_1")