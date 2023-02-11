#' pct_infobox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard renderInfoBox
#' @importFrom shinydashboard infoBox
mod_pct_infobox_ui <- function(id){
  ns <- NS(id)
  tagList(
    infoBoxOutput(NS(id, "pct_metric"))
  )
}

#' pct_infobox Server Functions
#'
#' @noRd
mod_pct_infobox_server <- function(id, box_title, pct_value){
  moduleServer( id, function(input, output, session){
    output$pct_metric <- renderInfoBox({
      infoBox(
        box_title,
        paste0(round(pct_value*100, 2),"%"),
        icon = shiny::icon("building"),
        color = "aqua",
        fill = TRUE
      )
    })
  })
}

## To be copied in the UI
# mod_pct_infobox_ui("pct_infobox_1")

## To be copied in the server
# mod_pct_infobox_server("pct_infobox_1")
