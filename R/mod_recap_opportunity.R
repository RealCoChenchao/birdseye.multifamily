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
                        multiSelect = TRUE,
                        styles = list(
                          root = list(width = "10vw"),
                          dropdownItemsWrapper = list(
                            root = list(width = "10vw"),
                            maxHeight = "200px",
                            overflow = "auto"
                          )),
                        options = metro_options),
    Toggle.shinyInput(NS(id, "render"),
                      value = FALSE,
                      label = "Toggle Heatmap (5-mins driving)"),
    Slider.shinyInput(NS(id, "sqft"),
                      value = 100000, min = 10000, max = 5e6, step = 5000,
                      label = "Sqft",
                      valueFormat = JS("function(x) { return Intl.NumberFormat().format(x)}"),
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
      makeCard("Market Comparison",
               mod_multi_linechart_ui(NS(id, "linechart")),
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
    real_estate_db <- rcAppTools::rc_connect_db(
      database = c("cre_fundamentals"),
      type = c("pool"))

    stablized_dev_property <- sf::read_sf(real_estate_db,
                                          query = "SELECT * FROM stablized_dev_property")
    selected_opportunity <- reactive({
      result <- stablized_dev_property %>%
        dplyr::filter(GEOID %in% c("408"),
                      delivered_date >= "2014-05-01")
      return(result)
    })

    mod_opportunity_server("recap_opportunity", selected_opportunity)
  })
}

## To be copied in the UI
# mod_recap_opportunity_ui("recap_opportunity_1")

## To be copied in the server
# mod_recap_opportunity_server("recap_opportunity_1")
