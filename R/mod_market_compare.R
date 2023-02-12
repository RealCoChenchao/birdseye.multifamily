#' market_compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_market_compare_ui <- function(id){
  ns <- NS(id)
  filters <- Stack(
    tokens = list(childrenGap = 10),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      DatePicker.shinyInput(NS(id, "fromDate"), value = as.Date('2020/01/01'), label = "From date"),
      DatePicker.shinyInput(NS(id, "toDate"), value = as.Date('2020/12/31'), label = "To date")
    ),
    Label("Add Metro to the right line chart", className = "my_class"),
    Dropdown.shinyInput(NS(id, "metro"),
                        placeHolder = "Metro",
                        multiSelect = TRUE,
                        # value = "Tucson, AZ",
                        dropdownWidth = 'auto',
                        styles = list(
                          dropdownItemsWrapper = list(
                            root = list(width = "10vw"),
                            maxHeight = "200px",
                            overflow = "auto"
                          )),
                        options = metro_options)
  )

  fluentPage(
    Stack(
      tokens = list(childrenGap = 10),
      horizontal = TRUE,
      makeCard("Rank Filters",
               filters,
               size = 4,
               style = "max-height: 320px;"),
      makeCard("Market Comparison",
               mod_multi_linechart_ui(NS(id, "linechart")),
               size = 8,
               style = "max-height: 320px")
    ),
    makeCard("Market Performance Rank (Stablized Property)",
             mod_rank_table_ui(NS(id, "rank_table")),
             size = 8,
             style = "max-height: 400px; overflow: auto"),
    # Label("Add Metro to the right line chart", className = "my_class"),
    makeCard("Market Performance by % of 2+ Bedroom Units",
             mod_multi_barchart_ui(NS(id, "unit_dist")),
             size = 8,
             style = "max-height: 400px; overflow: auto"),
    makeCard("Market Performance by Urban vs Suburban",
             mod_multi_barchart_ui(NS(id, "urban_dist")),
             size = 8,
             style = "max-height: 400px; overflow: auto"),

  )
}

#' market_compare Server Functions
#'
#' @noRd
mod_market_compare_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_market_compare_ui("market_compare_1")

## To be copied in the server
# mod_market_compare_server("market_compare_1")
