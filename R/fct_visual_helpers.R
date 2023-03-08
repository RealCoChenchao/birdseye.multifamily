#' visual_helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom leaflet awesomeIconList
#' @noRd

dplyr_summarize_property_pefm <- function(selected_property_pefm){
  selected_property_pefm %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      mean_occupancy = weighted.mean(occupancy, quantity, na.rm = TRUE),
      mean_occupancy_3_month_change = weighted.mean(occupancy_3_month_change, quantity, na.rm = TRUE),
      mean_occupancy_6_month_change = weighted.mean(occupancy_6_month_change, quantity, na.rm = TRUE),
      mean_occupancy_12_month_change = weighted.mean(occupancy_12_month_change, quantity, na.rm = TRUE),
      mean_occupancy_24_month_change = weighted.mean(occupancy_24_month_change, quantity, na.rm = TRUE),
      mean_occupancy_36_month_change = weighted.mean(occupancy_36_month_change, quantity, na.rm = TRUE),

      mean_effective_rent_per_sq_ft = weighted.mean(effective_rent_per_sq_ft, quantity * areaperunit, na.rm = TRUE),
      mean_effective_rent_per_sf_3_month_growth = weighted.mean(effective_rent_per_sf_3_month_growth, quantity * areaperunit, na.rm = TRUE),
      mean_effective_rent_per_sf_6_month_growth = weighted.mean(effective_rent_per_sf_6_month_growth, quantity * areaperunit, na.rm = TRUE),
      mean_effective_rent_per_sf_12_month_growth = weighted.mean(effective_rent_per_sf_12_month_growth, quantity * areaperunit, na.rm = TRUE),
      mean_effective_rent_per_sf_24_month_growth = weighted.mean(effective_rent_per_sf_24_month_growth, quantity * areaperunit, na.rm = TRUE),
      mean_effective_rent_per_sf_36_month_growth = weighted.mean(effective_rent_per_sf_36_month_growth, quantity * areaperunit, na.rm = TRUE),

      mean_revenue_per_unit = weighted.mean(revenue_per_unit, quantity, na.rm = TRUE),
      mean_revenue_per_unit_3_month_growth = weighted.mean(revenue_per_unit_3_month_growth, quantity, na.rm = TRUE),
      mean_revenue_per_unit_6_month_growth = weighted.mean(revenue_per_unit_6_month_growth, quantity, na.rm = TRUE),
      mean_revenue_per_unit_12_month_growth = weighted.mean(revenue_per_unit_12_month_growth, quantity, na.rm = TRUE),
      mean_revenue_per_unit_24_month_growth = weighted.mean(revenue_per_unit_24_month_growth, quantity, na.rm = TRUE),
      mean_revenue_per_unit_36_month_growth = weighted.mean(revenue_per_unit_36_month_growth, quantity, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

format_property_pefm_info <- function(selected_roperty_pefm_info){
  subset_selected_roperty_pefm_info <- selected_roperty_pefm_info %>%
    rename(effective_rent_per_sf = effective_rent_per_sq_ft) %>%
    tidyr::pivot_longer(cols = c(contains("effective_rent_per_sf"),
                                 contains("revenue_per_unit"),
                                 contains("occupancy")),
                        names_to = "performance_metric") %>%
    tidyr::pivot_wider(names_from = property_group,
                       values_from = "value") %>%
    dplyr::mutate(id = as.character(row_number()))
  subset_selected_roperty_pefm_info %>%
    dplyr::filter(!performance_metric %in% c("effective_rent_per_sf", "revenue_per_unit")) %>%
    dplyr::mutate(performance_metric = format_metric_name(performance_metric)) %>%
    dplyr::mutate_if(is.numeric,
                     funs(
                       ifelse(is.na(.),
                              "",
                              scales::percent(., accuracy = .1)))) %>%
    dplyr::bind_rows(subset_selected_roperty_pefm_info %>%
                       dplyr::filter(performance_metric %in% c("effective_rent_per_sf", "revenue_per_unit")) %>%
                       dplyr::mutate(performance_metric = format_metric_name(performance_metric)) %>%
                       dplyr::mutate_if(is.numeric,
                                        funs(
                                          ifelse(is.na(.),
                                                 "",
                                                 scales::dollar(., accuracy = .01))))) %>%
    dplyr::arrange(as.numeric(id)) %>%
    dplyr::rename(`Performance Metric` = performance_metric) %>%
    dplyr::select(-id) %>%
    knitr::kable() %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                              font_size = 13) %>%
    kableExtra::pack_rows("MF Performance Metrics", 1, 21,
                          label_row_css = "background-color: #12395b; color: #fff;")  %>%
    kableExtra::column_spec(column = 1, bold = TRUE)
}

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

icons <- leaflet::awesomeIconList(
  axio_pipeline = leaflet::makeAwesomeIcon(icon = 'building',
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
