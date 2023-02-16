#' opportunity_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_opportunity_map_ui <- function(id){
  ns <- NS(id)
  leafletOutput(NS(id, "property_map"),
                height = 750,
                width = "100%")
}

#' opportunity_map Server Functions
#'
#' @noRd
mod_opportunity_map_server <- function(id, selected_opportunity){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    transport_layer <- "https://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png?apikey=eee136582101482dbbc1977656b7ce3f"
    hybrid_layer <- "http://mt0.google.com/vt/lyrs=y&hl=en&x={x}&y={y}&z={z}"

    output$property_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(provider = "Stamen.TonerLite", group = "Contrast Map") %>%
        addProviderTiles(provider = "OpenStreetMap", group = "Street Map") %>%
        addProviderTiles(provider = "Esri.WorldImagery", group = "Satellite Map") %>%
        addTiles(urlTemplate = transport_layer, group = "Transit Map") %>%
        addTiles(urlTemplate = hybrid_layer, group = "Hybrid Map") %>%
        addNegativeRentPropertyMarker(sf_data = selected_opportunity()) %>%
        addTiles(urlTemplate = "", attribution = "") %>%
        addLayersControl(baseGroups = c("Contrast Map",
                                        "Street Map",
                                        "Satellite Map",
                                        "Transit Map",
                                        "Hybrid Map"),
                         position = "topright",
                         options = layersControlOptions(collapsed = FALSE))
    })
  })
}

## To be copied in the UI
# mod_opportunity_map_ui("opportunity_map_1")

## To be copied in the server
# mod_opportunity_map_server("opportunity_map_1")
