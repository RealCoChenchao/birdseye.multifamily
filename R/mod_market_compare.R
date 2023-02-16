#' market_compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom rlang sym
mod_market_compare_ui <- function(id){
  ns <- NS(id)
  filters <- Stack(
    tokens = list(childrenGap = 10),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      DatePicker.shinyInput(NS(id, "fromDate"), value = as.Date('2020/03/01'), label = "From date"),
      DatePicker.shinyInput(NS(id, "toDate"), value = as.Date('2023/01/01'), label = "To date")
    ),
    Label("Pick the Metric; Add metro to the right chart", className = "my_class"),
    Label("Maximum 5 selected metro for user experience purpose", className = "my_class"),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      Dropdown.shinyInput(NS(id, "metric"),
                          # placeHolder = "Metric",
                          multiSelect = FALSE,
                          value = "mean_revenue_per_unit_1_month_growth",
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
                          styles = list(
                            root = list(width = "10vw"),
                            dropdownItemsWrapper = list(
                              root = list(width = "10vw"),
                              maxHeight = "200px",
                              overflow = "auto"
                            )),
                          options = metro_options)
    ),
    Label("Pick the Metric for Charts below", className = "my_class"),
    Dropdown.shinyInput(NS(id, "calc_metric"),
                        # placeHolder = "Metric",
                        multiSelect = FALSE,
                        value = "mean_effective_rent_per_sf_period_growth",
                        dropdownWidth = 'auto',
                        styles = list(
                          dropdownItemsWrapper = list(
                            root = list(width = "10vw"),
                            root = list(width = "10vw"),
                            maxHeight = "200px",
                            overflow = "auto"
                          )),
                        options = calc_metric_options)
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
               PivotItem(headerText = "Select Market Chart",  mod_multi_barchart_ui(NS(id, "overall_table"))),
               PivotItem(headerText = "All Market Table", mod_rank_table_ui(NS(id, "overall_table")))
             ),
             size = 11.5,
             style = "max-height: 400px; overflow: auto"),
    makeCard("Market Performance by % of 2+ Bedroom Units",
             Pivot(
               PivotItem(headerText = "Select Market Chart", mod_multi_barchart_ui(NS(id, "unit_market"))),
               PivotItem(headerText = "All Market Table", mod_rank_table_ui(NS(id, "unit_market")))
             ),
             size = 11.5,
             style = "max-height: 400px; overflow: auto"),
    makeCard("Market Performance by Grade",
             Pivot(
               PivotItem(headerText = "Select Market Chart", mod_multi_barchart_ui(NS(id, "market_grade"))),
               PivotItem(headerText = "All Market Table", mod_rank_table_ui(NS(id, "market_grade")))
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

    # work on the overall pefm table
    pefm_overall <- reactive({
      selectedMetro <- (
        if (length(input$metro) > 0) input$metro[1:5]
        else metro_options$text[1:5]
      )

      selectedMetric <- (
        if (length(input$calc_metric) > 0) input$calc_metric
        else c("mean_effective_rent_per_sf_period_growth")
      )

      pefm_tbl <- calc_axio_mkt_metric(start_month = input$fromDate,
                                       end_month = input$toDate,
                                       groupby = "axio_market")
      chart <- pefm_tbl %>%
        dplyr::filter(marketname %in% !!selectedMetro) %>%
        dplyr::select(
          c("Data Cut" = "marketname",
            "Performance" = all_of(selectedMetric),
            "Market" = "marketname"))

      return(list(table = format_axio_mkt_metric_tbl(pefm_tbl),
                  chart = chart))
    }) %>%
      bindCache(input$fromDate,
                input$toDate,
                input$metro)
    mod_rank_table_server("overall_table", pefm_overall)
    mod_multi_barchart_server("overall_table", pefm_overall)

    # work on the unit pefm table
    pefm_unit_market <- reactive({
      selectedMetro <- (
        if (length(input$metro) > 0) input$metro[1:5]
        else metro_options$text[1:5]
      )

      selectedMetric <- (
        if (length(input$calc_metric) > 0) input$calc_metric
        else c("mean_effective_rent_per_sf_period_growth")
      )

      pefm_tbl <- calc_axio_mkt_metric(start_month = input$fromDate,
                           end_month = input$toDate,
                           groupby = "combo_unit_market")

      chart <- pefm_tbl %>%
        dplyr::filter(marketname %in% !!selectedMetro) %>%
        dplyr::select(
          c("Data Cut" = "property_unit_dist",
            "Performance" = all_of(selectedMetric),
            "Market" = "marketname"))

      return(list(table = format_axio_mkt_metric_tbl(pefm_tbl),
                  chart = chart))
    }) %>%
      bindCache(input$fromDate,
                input$toDate,
                input$metro)
    mod_rank_table_server("unit_market", pefm_unit_market)
    mod_multi_barchart_server("unit_market", pefm_unit_market)

    # work on the grade pefm table
    pefm_market_grade <- reactive({
      selectedMetro <- (
        if (length(input$metro) > 0) input$metro[1:5]
        else metro_options$text[1:5]
      )

      selectedMetric <- (
        if (length(input$calc_metric) > 0) input$calc_metric
        else c("mean_effective_rent_per_sf_period_growth")
      )

      pefm_tbl <- calc_axio_mkt_metric(start_month = input$fromDate,
                           end_month = input$toDate,
                           groupby = "combo_market_grade")

      chart <- pefm_tbl %>%
        dplyr::filter(marketname %in% !!selectedMetro) %>%
        dplyr::select(
          c("Data Cut" = "property_market_grade_new",
            "Performance" = all_of(selectedMetric),
            "Market" = "marketname"))

      return(list(table = format_axio_mkt_metric_tbl(pefm_tbl),
                  chart = chart))
    }) %>%
      bindCache(input$fromDate,
                input$toDate,
                input$metro)
    mod_rank_table_server("market_grade", pefm_market_grade)
    mod_multi_barchart_server("market_grade", pefm_market_grade)

    # work on the line chart
    pefm_line <- reactive({
      selectedMetro <- (
        if (length(input$metro) > 0) input$metro[1:5]
        else metro_options$text[1:5]
      )
      tbl(real_estate_db, "axio_mkt_stable_pefm") %>%
        dplyr::filter(
          month >= !!input$fromDate,
          month <= !!input$toDate,
          marketname %in% !!selectedMetro
        ) %>%
        dplyr::select(month, marketname, pefm_line_metric = rlang::sym(!!input$metric)) %>%
        dplyr::collect()
    })

    y_axis_format <- reactive({
      ifelse(str_detect(input$metric, "occupancy|growth|change"),
             scales::percent,
             scales::dollar)
    })

    mod_multi_linechart_server("linechart",
                               pefm_line,
                               month,
                               pefm_line_metric,
                               marketname,
                               y_axis_format)
    # input$metric
  })
}

## To be copied in the UI
# mod_market_compare_ui("market_compare_1")

## To be copied in the server
# mod_market_compare_server("market_compare_1")
