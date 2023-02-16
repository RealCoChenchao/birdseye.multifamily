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
                              `Rent Change From Lease-Up to Current` = effective_rent_pct_change_leaseup_current,
                              `Rent Change During Lease-Up Against Market` = diff_to_market_effective_rent_leaseup_current
                            ) %>%
                            datatable() %>%
                            DT::formatPercentage(c("Rent Change From Lease-Up to Current",
                                                   "Rent Change During Lease-Up Against Market"),
                                                 2),
                          options = list(scrollX = TRUE),
                          rownames = FALSE)
  })
}

## To be copied in the UI
# mod_opportunity_table_ui("opportunity_table_1")

## To be copied in the server
# mod_opportunity_table_server("opportunity_table_1")
