#' recap_opportunity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_recap_opportunity_ui <- function(id){
  ns <- NS(id)
  filters <- Stack(
    tokens = list(childrenGap = 10),
    DatePicker.shinyInput(NS(id, "fromDate"), value = as.Date('2020/03/01'), label = "Deliver Date"),
    Dropdown.shinyInput(NS(id, "metro"),
                        placeHolder = "Metro",
                        value = "National",
                        multiSelect = FALSE,
                        styles = list(
                          root = list(width = "10vw"),
                          dropdownItemsWrapper = list(
                            root = list(width = "10vw"),
                            maxHeight = "200px",
                            overflow = "auto"
                          )),
                        options = metro_options),
    # Toggle.shinyInput(NS(id, "render"),
    #                   value = FALSE,
    #                   label = "Toggle Heatmap (5-mins driving)"),
    Slider.shinyInput(NS(id, "rent_growth"),
                      value = 0, min = -1, max = 1, step = 0.05,
                      label = "Rent Growth",
                      valueFormat = JS("function(x) { return Intl.NumberFormat('en-GB', { style: 'percent'}).format(x)}"),
                      snapToStep = TRUE)
  )

  fluentPage(
    Stack(
      tokens = list(childrenGap = 10),
      horizontal = TRUE,
      makeCard("Recap Opportunity Filters",
               filters,
               size = 4,
               style = "max-height: 400px;"),
      makeCard("Opportunity Count",
               mod_count_barchart_ui(NS(id, "opportunity_count")),
               size = 8,
               style = "max-height: 400px; overflow: auto")
    ),
    makeCard("Potential Recapitalization Opportunity",
             mod_opportunity_ui(NS(id, "recap_opportunity")),
             size = 11.5,
             style = "max-height: 750px; overflow: auto")
  )
}

#' recap_opportunity Server Functions
#'
#' @noRd
mod_recap_opportunity_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    real_estate_db <- make_pool()

    stablized_dev_property <- sf::read_sf(real_estate_db,
                                          query = "SELECT * FROM stablized_dev_property")
    selected_opportunity <- reactive({

      selectedMetro <- (
        if (length(input$metro) > 0) input$metro
        else c("National")
      )

      selectedRentGrowth <- (
        if (length(input$rent_growth) > 0) input$rent_growth
        else c(0)
      )

      filtered_property <- stablized_dev_property %>%
        dplyr::filter(delivered_date >= input$fromDate,
                      effective_rent_pct_change_leaseup_current <= selectedRentGrowth)

      if(selectedMetro != "National"){
        filtered_property <- filtered_property %>%
          dplyr::filter(marketname %in% selectedMetro)
      }

      return(filtered_property)
    })

    mod_opportunity_server("recap_opportunity", selected_opportunity)

    opportunity_summary <- reactive({
      req(!is.null(selected_opportunity()))
      if(input$metro == "National"){
        opportunity_summary <- selected_opportunity() %>%
          sf::st_drop_geometry() %>%
          dplyr::count(marketname) %>%
          dplyr::rename(Region = marketname,
                        Count = n)
      }else{
        opportunity_summary <- selected_opportunity() %>%
          sf::st_drop_geometry() %>%
          dplyr::count(submarket) %>%
          dplyr::rename(Region = submarket,
                        Count = n)
      }
      opportunity_summary
    })

    mod_count_barchart_server("opportunity_count", opportunity_summary)
  })
}

## To be copied in the UI
# mod_recap_opportunity_ui("recap_opportunity_1")

## To be copied in the server
# mod_recap_opportunity_server("recap_opportunity_1")
