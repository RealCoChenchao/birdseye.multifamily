#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  real_estate_db <- make_pool()
  mod_market_info_server("market_intelligence")
  mod_market_compare_server("market_compare")
  mod_recap_opportunity_server("opportunity_lens")
  router$server(input, output, session)
}
