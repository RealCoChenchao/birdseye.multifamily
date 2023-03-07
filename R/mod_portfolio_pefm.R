#' portfolio_pefm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_portfolio_pefm_ui <- function(id){
  ns <- NS(id)
  fluentPage(
    makeCard("Portfolio Performance Tracking",
             mod_portfolio_pefm_table_ui(NS(id, "portfolio_pefm")),
             size = 11.5,
             style = "max-height: 400px; overflow: auto"),
    Stack(
      tokens = list(childrenGap = 10),
      horizontal = TRUE,
      makeCard("Portfolio and Surrounding Property",
               Stack(
                 tokens = list(childrenGap = 10),
                 Slider.shinyInput(NS(id, "distance"),
                                   value = 15, min = 1, max = 30, step = 1,
                                   label = "Comparable Area",
                                   snapToStep = TRUE),
                 mod_portfolio_nearby_map_ui(NS(id, "portfolio_nearby"))
               ),
               size = 4,
               style = "max-height: 750px;"),
      makeCard("Performance Comparison",
               mod_portfolio_nearby_table_ui(NS(id, "portfolio_nearby")),
               size = 8,
               style = "max-height: 750px; overflow: auto")
    )
  )
}

#' portfolio_pefm Server Functions
#'
#' @noRd
mod_portfolio_pefm_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    real_estate_db <- make_pool()

    googlesheets4::gs4_deauth()
    googlesheets4::gs4_auth(cache = ".secrets",
                            email = TRUE)
    realco_operating_property <- googlesheets4::read_sheet("1lnP1ERj3pjtMtiArTfn4YD-eSKNPkHCxjibW6l4jvrg")

    realco_property_pefm <- select_property_pefm(real_estate_db = real_estate_db,
                                                 selected_projid = realco_operating_property$projid)
    mod_portfolio_pefm_table_server("portfolio_pefm", reactive({realco_property_pefm}))

    observe({
      req(input$portfolio_pefm_rows_selected)
      selected_property <- realco_property_pefm[input$portfolio_pefm_rows_selected,]
      print(selected_property[[1]])
      nearby_boundary <- reactive({
        get_surrounding_sf
      })
      mod_portfolio_nearby_map_server("portfolio_nearby",
                                      selected_property,
                                      nearby_boundary,
                                      nearby_property)
      mod_portfolio_nearby_table_server("portfolio_nearby")
    })


  })
}

## To be copied in the UI
# mod_portfolio_pefm_ui("portfolio_pefm_1")

## To be copied in the server
# mod_portfolio_pefm_server("portfolio_pefm_1")
