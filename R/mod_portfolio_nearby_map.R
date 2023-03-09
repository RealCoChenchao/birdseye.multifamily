#' portfolio_nearby_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_portfolio_nearby_map_ui <- function(id){
  ns <- NS(id)
  leafletOutput(NS(id, "map"))
}

#' portfolio_nearby_map Server Functions
#'
#' @noRd
mod_portfolio_nearby_map_server <- function(id, property, nearby_boundary, nearby_property){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    transport_layer <- "https://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png?apikey=eee136582101482dbbc1977656b7ce3f"
    hybrid_layer <- "http://mt0.google.com/vt/lyrs=y&hl=en&x={x}&y={y}&z={z}"

    output$map <- renderLeaflet({
      bounds <- nearby_boundary() %>%
        st_bbox() %>%
        as.character()

      leaflet() %>%
        addProviderTiles(provider = "Stamen.TonerLite", group = "Contrast Map") %>%
        addProviderTiles(provider = "OpenStreetMap", group = "Street Map") %>%
        addProviderTiles(provider = "Esri.WorldImagery", group = "Satellite Map") %>%
        addTiles(urlTemplate = transport_layer, group = "Transit Map") %>%
        addTiles(urlTemplate = hybrid_layer, group = "Hybrid Map") %>%
        addAwesomeMarkers(data = property,
                          icon = makeAwesomeIcon(icon = "star",
                                                 library = "fa",
                                                 markerColor = "orange"),
                          label = ~name,
                          popup = ~popup_text) %>%
        addAwesomeMarkers(data = nearby_property(),
                          icon = makeAwesomeIcon(icon = 'building',
                                                library = 'fa',
                                                markerColor = "blue"),
                          label = ~name,
                          popup = ~popup_text) %>%
        addPolygons(data = nearby_boundary(),
                    label = "drive time boundary",
                    color = "#ac080e",
                    weight = 0.5) %>%
        flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
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
# mod_portfolio_nearby_map_ui("portfolio_nearby_map_1")

## To be copied in the server
# mod_portfolio_nearby_map_server("portfolio_nearby_map_1")
