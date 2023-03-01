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
                                     scales::percent(effective_rent_pct_change, accuracy = 0.01), " Rent Growth From Lease-Up to Current",
                                     scales::percent(diff_to_market_effective_rent, accuracy = 0.01), " Rent Growth Diff VS Market From Lease-Up to Current"))
  } else {
    map
  }
}

addPopupTexts <- function(sf_df){
  if(!"submarket" %in% names(sf_df)){
    sf_df %>%
      dplyr:: mutate(popup_text = paste0("Market Name: ", tidyr::replace_na(marketname, "N/A"), "<br>",
                                         "Average Effective Rent/SqFt: ", tidyr::replace_na(scales::dollar(mean_effective_rent_per_sq_ft, accuracy = 0.01), "N/A"), "<br>",
                                         "Average Occupancy: ", tidyr::replace_na(percent(mean_occupancy, accuracy = 0.01), "N/A"), "<br>",
                                         "Average Revenue/Unit: ", tidyr::replace_na(scales::dollar(mean_revenue_per_unit, accuracy = 1), "N/A"), "<br>",
                                         "Average Revenue/Unit 1-Month Growth: ", tidyr::replace_na(percent(mean_revenue_per_unit_1_month_growth, accuracy = 0.01), "N/A")))
  }else{
    sf_df %>%
      dplyr:: mutate(popup_text = paste0("Market Name: ", tidyr::replace_na(marketname, "N/A"), "<br>",
                                         "Submarket Name: ", tidyr::replace_na(submarket, "N/A"), "<br>",
                                         "Average Effective Rent/SqFt: ", tidyr::replace_na(scales::dollar(mean_effective_rent_per_sq_ft, accuracy = 0.01), "N/A"), "<br>",
                                         "Average Occupancy: ", tidyr::replace_na(percent(mean_occupancy, accuracy = 0.01), "N/A"), "<br>",
                                         "Average Revenue/Unit: ", tidyr::replace_na(scales::dollar(mean_revenue_per_unit, accuracy = 1), "N/A"), "<br>",
                                         "Average Revenue/Unit 1-Month Growth: ", tidyr::replace_na(percent(mean_revenue_per_unit_1_month_growth, accuracy = 0.01), "N/A")))
  }
}
