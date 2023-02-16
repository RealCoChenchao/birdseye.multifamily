#' visual_helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
format_axio_mkt_metric_tbl <- function(metric_tbl){
  rename_tbl <- metric_tbl %>%
    dplyr::rename(
      Market = marketname,
      `Rent Growth %` = mean_effective_rent_per_sf_period_growth,
      `Occupancy Change %` = mean_occupancy_period_change,
      `Revenue Per Unit Growth %` = mean_revenue_per_unit_period_growth
    )

  if("property_unit_dist" %in% names(metric_tbl)){
    rename_tbl <- rename_tbl %>%
      dplyr::rename(`Unit Dist` = property_unit_dist)
  }

  if("property_market_grade_new" %in% names(metric_tbl)){
    rename_tbl <- rename_tbl %>%
      dplyr::rename(`Grade` = property_market_grade_new)
  }

  rename_tbl
}

icons <- awesomeIconList(
  axio_pipeline = makeAwesomeIcon(icon = 'building',
                                  library = 'fa',
                                  markerColor = "lightred")
)

addNegativeRentPropertyMarker <- function(map, sf_data){
  if (dim(sf_data)[1] != 0) {
    addAwesomeMarkers(map,
                      data = sf_data,
                      icon = ~icons["axio_pipeline"],
                      label = ~name,
                      popup = ~paste(name, "<br>",
                                     address, "<br>",
                                     city, state, zip, "<br>",
                                     scales::comma(quantity), "units", "<br>",
                                     "Year Delivered: ", delivered_year, "<br>",
                                     "Lease-Up Month: ", leaseup_month, "<br>",
                                     scales::percent(effective_rent_pct_change_leaseup_current, accuracy = 0.01), " Rent Change From Lease-Up to Current",
                                     scales::percent(diff_to_market_effective_rent_leaseup_current, accuracy = 0.01), " Rent Change Against Market"))
  } else {
    map
  }
}
