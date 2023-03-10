#' portfolio_pefm_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_portfolio_pefm_table_ui <- function(id){
  ns <- NS(id)
  DT::dataTableOutput(NS(id, "pefm_table"))
}

#' portfolio_pefm_table Server Functions
#'
#' @noRd
mod_portfolio_pefm_table_server <- function(id, portfolio_table){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$pefm_table <-
      DT::renderDataTable(datatable(realco_property_pefm %>%
                                      sf::st_drop_geometry() %>%
                                      dplyr::select(-contains("covid"), -projid) %>%
                                      tidyr::pivot_longer(cols = c(effective_rent_per_sq_ft,
                                                                   contains("effective_rent_per_sf"),
                                                                   contains("revenue_per_unit"),
                                                                   contains("occupancy")),
                                                          names_to = "performance_metric") %>%
                                      dplyr::mutate(performance_metric = format_metric_name(performance_metric)) %>%
                                      tidyr::pivot_wider(names_from = "performance_metric",
                                                         values_from = "value") %>%
                                      dplyr::select(-popup_text) %>%
                                      dplyr::select(Name = name,
                                                    Address = address,
                                                    City = city,
                                                    State = state,
                                                    Zip = zip,
                                                    `Year Built` = yearbuilt,
                                                    Level = level,
                                                    Status = status,
                                                    Units = quantity,
                                                    Areaperunit = areaperunit,
                                                    Market = marketname,
                                                    Submarket = submarket,
                                                    `Market Grade` = property_market_grade,
                                                    `Submarket Grade` = property_submarket_grade,
                                                    everything()),
                                    extensions = 'Buttons',
                                    selection = 'single',
                                    options = list(
                                      dom = 'Blfrtip',
                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                    )) %>%
                            DT::formatPercentage(c(16:21,
                                                   23:35),
                                                 2) %>%
                            DT::formatCurrency(c(15, 22)),
                          options = list(scrollX = TRUE),
                          rownames = FALSE)
  })
}

## To be copied in the UI
# mod_portfolio_pefm_table_ui("portfolio_pefm_table_1")

## To be copied in the server
# mod_portfolio_pefm_table_server("portfolio_pefm_table_1")
