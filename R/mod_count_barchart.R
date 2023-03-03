#' count_barchart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_count_barchart_ui <- function(id){
  ns <- NS(id)
  highchartOutput(NS(id, "barchart"))
}

#' count_barchart Server Functions
#'
#' @noRd
mod_count_barchart_server <- function(id, opportunity_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$barchart <- renderHighchart({

      hchart(opportunity_summary() %>%
               mutate(Region = fct_reorder(Region, Count, .desc = TRUE)) %>%
               arrange(Region),
             "column",
             hcaes(x = Region,
                   y = Count),
             tooltip = list(pointFormat = paste0("Count: ", "{point.Count}")))

    })
  })
}

## To be copied in the UI
# mod_count_barchart_ui("count_barchart_1")

## To be copied in the server
# mod_count_barchart_server("count_barchart_1")
