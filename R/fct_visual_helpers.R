#' visual_helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
format_axio_mkt_metric_tbl <- function(metric_tbl){
  metric_tbl %>%
    dplyr::rename(
      Market = marketname,
      `Rent Growth %` = mean_effective_rent_per_sf_period_growth,
      `Occupancy Change %` = mean_occupancy_period_change,
      `Revenue Per Unit Growth %` = mean_revenue_per_unit_period_growth
    )
}
