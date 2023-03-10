#' pefm_infoboxes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pefm_infoboxes_ui <- function(id){
  ns <- NS(id)
  makesimpleCard(
    Stack(
    tokens = list(childrenGap = 5),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 20),
      makesimpleCard(mod_dollar_infobox_ui(NS(id, "rent")), size = 6),
      makesimpleCard(mod_pct_infobox_ui(NS(id, "occupancy")), size = 6),
      makesimpleCard(mod_dollar_infobox_ui(NS(id, "revenue")), size = 6)
    ),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 20),
      makesimpleCard(mod_pct_infobox_ui(NS(id, "rent_growth_3month")), size = 6),
      makesimpleCard(mod_pct_infobox_ui(NS(id, "rent_growth_6month")), size = 6),
      makesimpleCard(mod_pct_infobox_ui(NS(id, "rent_growth_12month")), size = 6)
    ),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 20),
      makesimpleCard(mod_pct_infobox_ui(NS(id, "revenue_growth_3month")), size = 6),
      makesimpleCard(mod_pct_infobox_ui(NS(id, "revenue_growth_6month")), size = 6),
      makesimpleCard(mod_pct_infobox_ui(NS(id, "revenue_growth_12month")), size = 6)
    )
  )
  )

}

#' pefm_infoboxes Server Functions
#'
#' @noRd
mod_pefm_infoboxes_server <- function(id, pefm_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_pct_infobox_server("occupancy", "Occupancy", reactive({pefm_df()$mean_occupancy}))
    mod_dollar_infobox_server("rent", "Effective Rent", reactive({pefm_df()$mean_effective_rent_per_sq_ft}))
    mod_dollar_infobox_server("revenue", "Revenue Per Unit", reactive({pefm_df()$mean_revenue_per_unit}))
    mod_pct_infobox_server("rent_growth_3month", "3-Month Rent Growth", reactive({pefm_df()$mean_effective_rent_per_sf_3_month_growth}))
    mod_pct_infobox_server("rent_growth_6month", "6-Month Rent Growth", reactive({pefm_df()$mean_effective_rent_per_sf_6_month_growth}))
    mod_pct_infobox_server("rent_growth_12month", "12-Month Rent Growth", reactive({pefm_df()$mean_effective_rent_per_sf_12_month_growth}))
    mod_pct_infobox_server("revenue_growth_3month", "3-Month RPU Growth", reactive({pefm_df()$mean_revenue_per_unit_3_month_growth}))
    mod_pct_infobox_server("revenue_growth_6month", "6-Month RPU Growth", reactive({pefm_df()$mean_revenue_per_unit_6_month_growth}))
    mod_pct_infobox_server("revenue_growth_12month", "12-Month RPU Growth", reactive({pefm_df()$mean_revenue_per_unit_12_month_growth}))
  })
}

## To be copied in the UI
# mod_pefm_infoboxes_ui("pefm_infoboxes_1")

## To be copied in the server
# mod_pefm_infoboxes_server("pefm_infoboxes_1")
