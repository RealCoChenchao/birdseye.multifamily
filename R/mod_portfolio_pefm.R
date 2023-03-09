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
    realco_operating_property <- googlesheets4::read_sheet("1lnP1ERj3pjtMtiArTfn4YD-eSKNPkHCxjibW6l4jvrg") %>%
      dplyr::select(projid)

    axio_property_comps <- sf::read_sf(real_estate_db,
                                       query = "SELECT DISTINCT projid, geometry FROM axio_property_comps")
    realco_property_pefm <- select_property_pefm(real_estate_db = real_estate_db,
                                                 selected_projid = realco_operating_property$projid)
    mod_portfolio_pefm_table_server("portfolio_pefm", reactive({realco_property_pefm}))

    observe({
      req(input$`portfolio_pefm-pefm_table_rows_selected`)

      selected_property <- realco_property_pefm[input$`portfolio_pefm-pefm_table_rows_selected`,] %>%
        dplyr::inner_join(axio_property_comps,
                          by = c("projid")) %>%
        sf::st_as_sf() %>%
        addPropertyPefmPopupTexts()

      nearby_boundary <- reactive({
        rcUtility::get_surrounding_sf(selected_property)
      })

      nearby_property <- reactive({
        comps_surrounding <- sf::st_join(axio_property_comps,
                                         nearby_boundary(),
                                         join = st_covered_by,
                                         left = FALSE) %>%
          dplyr::filter(projid != selected_property$projid)

        comps_surrounding_pefm <- select_property_pefm(real_estate_db = real_estate_db,
                                                       selected_projid = comps_surrounding$projid)

        comps_surrounding %>%
          dplyr::inner_join(comps_surrounding_pefm,
                            by = c("projid")) %>%
          sf::st_as_sf() %>%
          addPropertyPefmPopupTexts()

      })

      mod_portfolio_nearby_map_server("portfolio_nearby",
                                      selected_property,
                                      nearby_boundary,
                                      nearby_property)

      selected_analytics_all <- reactive({

          comps_and_property <- tbl(real_estate_db, "axio_property_pefm") %>%
            dplyr::filter(projid %in% nearby_property()$projid) %>%
            dplyr::filter(month == max(month)) %>%
            dplyr::collect()

          nearby_pefm <- comps_and_property %>%
            dplyr_summarize_property_pefm() %>%
            dplyr::rename_all(~stringr::str_replace(., "^mean_", "")) %>%
            dplyr::mutate(property_group = "Properties Nearby")

          property_pefm <- comps_and_property %>%
            dplyr::filter(projid == selected_property$projid)%>%
            dplyr::mutate(property_group = selected_property$name_axio) %>%
            dplyr::select(names(nearby_pefm))

          submarket_pefm <- tbl(real_estate_db, "axio_submkt_stable_pefm") %>%
            dplyr::filter(marketname == !!selected_property$marketname,
                          submarket == !!selected_property$submarket) %>%
            dplyr::filter(month == max(month)) %>%
            dplyr::collect() %>%
            dplyr::rename_all(~stringr::str_replace(., "^mean_", "")) %>%
            dplyr::mutate(property_group = "Submarket") %>%
            dplyr::select(names(nearby_pefm))

          market_pefm <- tbl(real_estate_db, "axio_mkt_stable_pefm") %>%
            dplyr::filter(marketname == !!selected_property$marketname) %>%
            dplyr::filter(month == max(month)) %>%
            dplyr::collect() %>%
            dplyr::rename_all(~stringr::str_replace(., "^mean_", "")) %>%
            dplyr::mutate(property_group = "Market") %>%
            dplyr::select(names(nearby_pefm))

          national_pefm <- tbl(real_estate_db, "axio_national_stable_pefm") %>%
            dplyr::filter(month == max(month)) %>%
            dplyr::collect() %>%
            dplyr::rename_all(~stringr::str_replace(., "^mean_", "")) %>%
            dplyr::mutate(property_group = "National") %>%
            dplyr::select(names(nearby_pefm))

          selected_analytics_all <- dplyr::bind_rows(
            property_pefm,
            nearby_pefm,
            submarket_pefm,
            market_pefm,
            national_pefm)

          selected_analytics_all
      })

      mod_portfolio_nearby_table_server("portfolio_nearby", selected_analytics_all)
    })


  })
}

## To be copied in the UI
# mod_portfolio_pefm_ui("portfolio_pefm_1")

## To be copied in the server
# mod_portfolio_pefm_server("portfolio_pefm_1")
