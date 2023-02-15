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
    Label("Pick the Metric; Add metro to the right chart", className = "my_class"),
    Label("Maximum 5 selected metro for user experience purpose", className = "my_class"),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      Dropdown.shinyInput(NS(id, "metric"),
                          placeHolder = "Metric",
                          multiSelect = FALSE,
                          value = "mean_effective_rent_per_sf_1_month_growth",
                          dropdownWidth = 'auto',
                          styles = list(
                            dropdownItemsWrapper = list(
                              root = list(width = "10vw"),
                              root = list(width = "10vw"),
                              maxHeight = "200px",
                              overflow = "auto"
                            )),
                          options = metric_options),
      Dropdown.shinyInput(NS(id, "metro"),
                          placeHolder = "Metro",
                          multiSelect = TRUE,
                          # value = "Tucson, AZ",
                          styles = list(
                            root = list(width = "10vw"),
                            dropdownItemsWrapper = list(
                              root = list(width = "10vw"),
                              maxHeight = "200px",
                              overflow = "auto"
                            )),
                          options = metro_options)
    )
  )

  fluentPage(
    Stack(
      tokens = list(childrenGap = 10),
      horizontal = TRUE,
      makeCard("Rank Filters",
               filters,
               size = 4,
               style = "max-height: 400px;"),
      makeCard("Market Comparison",
               mod_multi_linechart_ui(NS(id, "linechart")),
               size = 8,
               style = "max-height: 400px; overflow: auto")
    ),
    makeCard("Market Performance Rank (Stablized Property)",
             Pivot(
               PivotItem(headerText = "Select Market Chart",  Label("Hello 2")),
               PivotItem(headerText = "All Market Table", mod_rank_table_ui(NS(id, "rank_table")))
             ),
             size = 11.5,
             style = "max-height: 400px; overflow: auto"),
    # Label("Add Metro to the right line chart", className = "my_class"),
    makeCard("Market Performance by % of 2+ Bedroom Units",
             Pivot(
               PivotItem(headerText = "Select Market Chart", mod_multi_barchart_ui(NS(id, "unit_dist"))),
               PivotItem(headerText = "All Market Table", Label("Hello 2"))
             ),
             size = 11.5,
             style = "max-height: 400px; overflow: auto"),
    makeCard("Market Performance by Urban vs Suburban",
             Pivot(
               PivotItem(headerText = "Select Market Chart", mod_multi_barchart_ui(NS(id, "urban_dist"))),
               PivotItem(headerText = "All Market Table", Label("Hello 2"))
             ),
             size = 11.5,
             style = "max-height: 400px; overflow: auto"),

  )
}

#' market_compare Server Functions
#'
#' @noRd
#' @importFrom rcAnalyticalMF calc_axio_mkt_metric
mod_market_compare_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    real_estate_db <- rcAppTools::rc_connect_db(
      database = c("cre_fundamentals"),
      type = c("pool"))

    # work on the rank DT table
    pefm_rank <- reactive({
      calc_axio_mkt_metric(start_month = input$fromDate,
                           end_month = input$toDate,
                           groupby = "axio_market") %>%
        format_axio_mkt_metric_tbl()
    })
    mod_rank_table_server("rank_table", pefm_rank)

    # work on the line chart
    pefm_line <- reactive({
      selectedMetro <- (
        if (length(input$metro) > 0) input$metro[1:5]
        else c("Tucson, AZ")
      )
      tbl(real_estate_db, "axio_mkt_stable_pefm") %>%
        dplyr::filter(
          month >= !!input$fromDate,
          month <= !!input$toDate,
          marketname %in% !!selectedMetro
        ) %>%
        dplyr::collect()
    })

    mod_multi_linechart_server("linechart",
                               pefm_line,
                               month,
                               mean_revenue_per_unit_1_month_growth,
                               marketname)
    # input$metric
  })
}

## To be copied in the UI
# mod_market_compare_ui("market_compare_1")

## To be copied in the server
# mod_market_compare_server("market_compare_1")
