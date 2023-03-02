#' multi_barchart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom highcharter highchartOutput
#' @importFrom highcharter renderHighchart
#' @importFrom highcharter hchart
#' @importFrom highcharter hcaes
#' @importFrom highcharter hc_xAxis
#' @importFrom highcharter hc_yAxis
#' @importFrom highcharter hc_tooltip
mod_multi_barchart_ui <- function(id){
  ns <- NS(id)
  highchartOutput(NS(id, "barchart"))
}

#' multi_barchart Server Functions
#'
#' @noRd
mod_multi_barchart_server <- function(id, pefm_cut, by_group = TRUE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    if(by_group){
      output$barchart <- renderHighchart({
        hchart(pefm_cut()$chart %>%
                 dplyr::mutate(Performance = round(Performance * 100, 2)), "column",
               hcaes(x = Market,
                     y = Performance,
                     group = `Data Cut`)) %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = pefm_cut()$metric),
                   labels = list(format = "{value}%")) %>%
          hc_tooltip(valueSuffix = "%")
      })
    }else{
      output$barchart <- renderHighchart({
        hchart(pefm_cut()$chart %>%
                 dplyr::mutate(Performance = round(Performance * 100, 2)), "column",
               hcaes(x = Market,
                     y = Performance),
               tooltip = list(pointFormat = paste0("{point.Performance}%"))) %>%
          hc_xAxis(title = list(text = "")) %>%
          hc_yAxis(title = list(text = pefm_cut()$metric),
                   labels = list(format = "{value}%"))
      })
    }
  })
}

## To be copied in the UI
# mod_multi_barchart_ui("multi_barchart_1")

## To be copied in the server
# mod_multi_barchart_server("multi_barchart_1")
