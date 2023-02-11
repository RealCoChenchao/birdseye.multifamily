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
                              # value = "Tucson, AZ",
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
    makesimpleCard(mod_pefm_infoboxes_ui(NS(id, "market_box"))),
    makesimpleCard(mod_market_heatmap_ui(NS(id, "market_map")),
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

    real_estate_db <- rcAppTools::rc_connect_db(
      database = c("cre_fundamentals"),
      type = c("pool"))
    national_pefm <- dplyr::collect(tbl(real_estate_db, "axio_national_stable_pefm"))
    market_pefm <- tbl(real_estate_db, "axio_mkt_stable_pefm") %>%
      dplyr::filter(month == max(month)) %>%
      dplyr::collect()
    mkt_geometry <- sf::read_sf(real_estate_db, "axio_markets")
    market_pefm_sf <- market_pefm %>%
      dplyr::left_join(mkt_geometry,
                       by = c("marketname")) %>%
      sf::st_as_sf()

    pefm_table <- reactive({
      if(length(input$metro) == 0){
        pefm_table <- national_pefm
      }else{
        pefm_table <- dplyr::filter(market_pefm, marketname == input$metro)
      }
    })

    pefm_boundary <- reactive({
      if(length(input$metro) == 0){
        pefm_boundary <- market_pefm_sf
      }else{
        submkt_geometry <- sf::read_sf(real_estate_db,
                                    query = glue("SELECT * FROM axio_submarkets WHERE marketname = '{input$metro}'"))
        submarket_pefm <- tbl(real_estate_db, "axio_submkt_stable_pefm") %>%
          dplyr::filter(marketname == !!input$metro,
                        month == max(month)) %>%
          dplyr::collect()
        pefm_boundary <- submarket_pefm %>%
          dplyr::left_join(submkt_geometry,
                           by = c("marketname" = "marketname",
                                  "submarket" = "submarketn")) %>%
          sf::st_as_sf()
      }
    })

    mod_pefm_infoboxes_server("market_box", pefm_table)
    mod_market_heatmap_server("market_map", pefm_boundary, input$metric)
  })
}

## To be copied in the UI
# mod_market_info_ui("market_info_1")

## To be copied in the server
# mod_market_info_server("market_info_1")
