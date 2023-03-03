#' dollar_infobox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard infoBoxOutput
mod_dollar_infobox_ui <- function(id){
  infoBoxOutput(NS(id, "dollar_metric"))
}

#' dollar_infobox Server Functions
#'
#' @noRd
mod_dollar_infobox_server <- function(id, box_title, dollar_value){
  moduleServer( id, function(input, output, session){
    output$dollar_metric <- renderInfoBox({
      infoBox(
        box_title,
        paste0(scales::dollar(dollar_value())),
        icon = shiny::icon("dollar-sign"),
        color = "light-blue",
        fill = TRUE
      )
    })
  })
}

## To be copied in the UI
# mod_dollar_infobox_ui("dollar_infobox_1")

## To be copied in the server
# mod_dollar_infobox_server("dollar_infobox_1")
