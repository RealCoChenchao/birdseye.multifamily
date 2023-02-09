#' market_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_market_info_ui <- function(id){
  Stack(
    tokens = list(childrenGap = 10),
    Stack(horizontal = TRUE,
          tokens = list(childrenGap = 10),
          Dropdown.shinyInput(NS(id, "metro"),
                              placeHolder = "Metro",
                              multiSelect = FALSE,
                              value = "176",
                              dropdownWidth = 'auto',
                              styles = list(
                                dropdownItemsWrapper = list(
                                  root = list(width = "10vw"),
                                  maxHeight = "200px",
                                  overflow = "auto"
                                )),
                              options = metro_options),
          Dropdown.shinyInput(NS(id, "metric"),
                              placeHolder = "Metric",
                              multiSelect = FALSE,
                              value = "mean_revenue_per_unit",
                              dropdownWidth = 'auto',
                              styles = list(
                                dropdownItemsWrapper = list(
                                  root = list(width = "10vw"),
                                  maxHeight = "200px",
                                  overflow = "auto"
                                )),
                              options = metric_options)),
    makesimpleCard(mod_pefm_infoboxes_ui(NS(id, "market"))),
    makeCard("Market Performance Heatmap",
             mod_market_heatmap_ui(NS(id, "market")),
             size = 8,
             style = "max-height: 1000px")
  )
}

#' market_info Server Functions
#'
#' @noRd
mod_market_info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    national_pefm
    market_pefm # this should already by a reactive
    submarket_pefm
    pefm_sf # this must be a sf object
    mod_pefm_infoboxes_server("market", reactive({market_pefm}))
    mod_market_heatmap_server("market", reactive({pefm_sf}), input$metric)

  })
}

## To be copied in the UI
# mod_market_info_ui("market_info_1")

## To be copied in the server
# mod_market_info_server("market_info_1")
