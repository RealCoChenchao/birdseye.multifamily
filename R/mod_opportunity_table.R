#' opportunity_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_opportunity_table_ui <- function(id){
  ns <- NS(id)
  ns <- NS(id)
  DT::dataTableOutput(NS(id, "opportunity_table"))
}

#' opportunity_table Server Functions
#'
#' @noRd
mod_opportunity_table_server <- function(id, pefm_table){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$opportunity_table <-
      DT::renderDataTable(pefm_table() %>%
                            sf::st_drop_geometry() %>%
                            dplyr::select(
                              name,
                              address,
                              city,
                              state,
                              zip,
                              quantity,
                              `Year Delivered` = delivered_year,
                              `Lease-Up Month` = leaseup_month,
                              `Rent Growth From Lease-Up to Current` = effective_rent_pct_change,
                              `Rent Growth Diff VS Market From Lease-Up to Current` = diff_to_market_effective_rent
                            ) %>%
                            datatable() %>%
                            DT::formatPercentage(c("Rent Growth From Lease-Up to Current",
                                                   "Rent Growth Diff VS Market From Lease-Up to Current"),
                                                 2),
                          options = list(scrollX = TRUE),
                          rownames = FALSE)
  })
}

## To be copied in the UI
# mod_opportunity_table_ui("opportunity_table_1")

## To be copied in the server
# mod_opportunity_table_server("opportunity_table_1")
