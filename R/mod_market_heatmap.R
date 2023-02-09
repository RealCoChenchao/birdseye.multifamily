#' market_heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_market_heatmap_ui <- function(id){
  ns <- NS(id)
  leafletOutput(NS(id, "metro_map"),
                width = 1200,
                height = 800)
}

#' market_heatmap Server Functions
#'
#' @noRd
mod_market_heatmap_server <- function(id, pefm_sf, selected_metric){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    transport_layer <- "https://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png?apikey=eee136582101482dbbc1977656b7ce3f"
    hybrid_layer <- "http://mt0.google.com/vt/lyrs=y&hl=en&x={x}&y={y}&z={z}"

    output$metro_map <- renderLeaflet({
      scale_max <- max(max(pefm_sf()[[selected_metric]], na.rm=TRUE),
                       -min(pefm_sf()[[selected_metric]], na.rm=TRUE))
      pal <- colorNumeric(palette = "Spectral",
                          na.color = NA,
                          domain = c(-scale_max, scale_max))
      leaflet() %>%
        addProviderTiles(provider = "Stamen.TonerLite", group = "Contrast Map") %>%
        addProviderTiles(provider = "OpenStreetMap", group = "Street Map") %>%
        addProviderTiles(provider = "Esri.WorldImagery", group = "Satellite Map") %>%
        addTiles(urlTemplate = transport_layer, group = "Transit Map") %>%
        addTiles(urlTemplate = hybrid_layer, group = "Hybrid Map") %>%
        addPolygons(data = pefm_sf(),
                    fillColor = ~pal(pefm_sf()[[selected_metric]]),
                    weight = 0.6,
                    opacity = 1,
                    color = "grey",
                    fillOpacity = 0.8
                    # ,popup = ~popup
                    ) %>%
        addLegend(data = pefm_sf(),
                  pal = pal,
                  title = str_to_title(str_replace_all(selected_metric, "_", " ")),
                  values = ~pefm_sf()[[selected_metric]],
                  opacity = 0.8,
                  position = "bottomright",
                  labFormat = ifelse(str_detect(selected_metric, "occupancy|growth"),
                                     labelFormat(suffix = "%", transform = function(x) 100 * x),
                                     labelFormat(prefix = "$"))) %>%
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
# mod_market_heatmap_ui("market_heatmap_1")

## To be copied in the server
# mod_market_heatmap_server("market_heatmap_1")
