#' market_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom scales dollar
#' @importFrom scales percent
#' @importFrom tidyr replace_na
mod_market_info_ui <- function(id){
  Stack(
    tokens = list(childrenGap = 10),
    mod_pefm_infoboxes_ui(NS(id, "market_box")),
    Stack(horizontal = TRUE,
          tokens = list(childrenGap = 500),
          makesimpleCard(Dropdown.shinyInput(NS(id, "metro"),
                              # placeHolder = "National",
                              multiSelect = FALSE,
                              value = "National",
                              dropdownWidth = 'auto',
                              styles = list(
                                dropdownItemsWrapper = list(
                                  root = list(width = "10vw"),
                                  maxHeight = "200px",
                                  overflow = "auto"
                                )),
                              options = metro_options)),
          makesimpleCard(Dropdown.shinyInput(NS(id, "metric"),
                              # placeHolder = "Metric",
                              multiSelect = FALSE,
                              value = "mean_effective_rent_per_sq_ft",
                              dropdownWidth = 'auto',
                              styles = list(
                                dropdownItemsWrapper = list(
                                  root = list(width = "10vw"),
                                  maxHeight = "200px",
                                  overflow = "auto"
                                )),
                              options = metric_options))),
    makesimpleCard(div(style = "max-height: 750px; overflow: auto",
                   mod_market_heatmap_ui(NS(id, "market_map"))))
  )
}

#' market_info Server Functions
#'
#' @noRd
mod_market_info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    real_estate_db <- make_pool()

    googlesheets4::gs4_deauth()
    googlesheets4::gs4_auth(cache = ".secrets",
                            email = TRUE)
    realco_housing_mkt <- googlesheets4::read_sheet("1jUt0Gs2AWdSBdFJl9FFd3mLEKjenPTbYAhkplQMc39c") %>%
      dplyr::select(marketname) %>%
      dplyr::distinct()

    national_pefm <- tbl(real_estate_db, "axio_national_stable_pefm") %>%
      dplyr::filter(month == max(month)) %>%
      dplyr::collect()
    market_pefm <- tbl(real_estate_db, "axio_mkt_stable_pefm") %>%
      dplyr::filter(month == max(month)) %>%
      dplyr::collect()
    mkt_geometry <- sf::read_sf(real_estate_db, "axio_markets")
    market_pefm_sf <- market_pefm %>%
      dplyr::left_join(mkt_geometry,
                       by = c("marketname")) %>%
      sf::st_as_sf()

    pefm_table <- reactive({
      if(input$metro == "National"){
        pefm_table <- national_pefm
      }else if(input$metro == "Realco"){
        pefm_table <- market_pefm %>%
          dplyr::filter(marketname %in% realco_housing_mkt$marketname) %>%
          dplyr::summarise(
            across(where(is.numeric),
                   mean)
          )
      }else{
        pefm_table <- dplyr::filter(market_pefm, marketname == input$metro)
      }
      pefm_table
    })

    pefm_boundary <- reactive({
      if(input$metro == "National"){
        pefm_boundary <- addPopupTexts(market_pefm_sf)
      }else if(input$metro == "Realco"){
        pefm_boundary <- addPopupTexts(dplyr::filter(market_pefm_sf, marketname %in% realco_housing_mkt$marketname))
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
          sf::st_as_sf() %>%
          addPopupTexts()
      }
    })

    selected_metric <- reactive({
      input$metric
    })

    mod_pefm_infoboxes_server("market_box", pefm_table)
    mod_market_heatmap_server("market_map", pefm_boundary, selected_metric)
  })
}

## To be copied in the UI
# mod_market_info_ui("market_info_1")

## To be copied in the server
# mod_market_info_server("market_info_1")
