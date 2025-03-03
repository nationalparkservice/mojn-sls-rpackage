#' Return median well depth to water measurements
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns Tibble
#' @export
#'
#' @examples
wellDepth <- function(park, field.season, site) {
  depthImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "WellDepth")
  
  depthCalculated <- depthImport |>
    dplyr::group_by(Park, SiteCode, DateTime) |>
    dplyr::summarize(WLBelowLSD_ft = median(WLBelowLSD_ft)) |>
    dplyr::ungroup() |>
    dplyr::filter(lubridate::year(DateTime) > 2010) |>
    dplyr::mutate(Gap = paste0(cumsum(c(0, diff(DateTime, units = "days") > 120)), "_", SiteCode))
  
  return(depthCalculated)
}

#' Return well depth to water measurements that are outside of the expected range of values
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns Tibble
#' @export
#'
#' @examples
wellExpected <- function(park, field.season, site) {
  depthImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "WellDepth")
  
  depthExpected <- depthImport |>
    dplyr::filter((SiteCode == "LAKE_W_ROGE0" & (270 > WLBelowLSD_ft | WLBelowLSD_ft > 278)) |
                  (SiteCode == "GRBA_W_DP1" & (10 > WLBelowLSD_ft | WLBelowLSD_ft > 70)) |
                  (SiteCode == "GRBA_W_OC2" & (30 > WLBelowLSD_ft | WLBelowLSD_ft > 80)) |
                  (SiteCode == "GRBA_W_SH3" & (30 > WLBelowLSD_ft | WLBelowLSD_ft > 80)))
  
}

#' Plot well depth to water over time for sites within a park
#'
#' @param park Mandatory, e.g., "GRBA"
#' @param field.season Optional, e.g., c("2017", "2019", "2020")
#' @param site Optional, e.g., "LAKE_W_ROGE0"
#'
#' @returns ggplot object
#' @export
#'
#' @examples
wellDepthPlot <- function(park, field.season, site) {
  depthCalculated <- wellDepth(park = park, field.season = field.season, site = site)
  
  plot <- ggplot2::ggplot(data = depthCalculated,
                          ggplot2::aes(x = DateTime,
                                       y = WLBelowLSD_ft,
                                       color = SiteCode,
                                       shape = SiteCode
                                       , group = Gap
                          )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Water Level Below Land Surface Datum (ft)") +
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_y_reverse()
  
  return(plot)
}
