#' Return well depth to water measurements that are outside of the expected range of values
#'
#' @param park Optional, e.g., "GRBA"
#' @param field.season Optional, e.g., c("2017", "2019", "2020")
#' @param site Optional, e.g., "LAKE_W_ROGE0"
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
  
  return(depthExpected)
}

#' Plot well depth to water over time for sites within a park
#'
#' @param park Optional, e.g., "GRBA"
#' @param field.season Optional, e.g., c("2017", "2019", "2020")
#' @param site Optional, e.g., "LAKE_W_ROGE0"
#'
#' @returns ggplot object
#' @export
#'
#' @examples
discreteWellDepthPlot <- function(park, field.season, site) {
  depthImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "VisitWell")
  
  depthGaps <- depthImport |>
    dplyr::select(Park, SiteCode, DateTime, WLBelowLSD_ft) |>
    dplyr::filter(lubridate::year(DateTime) > 2010) |>
    dplyr::mutate(Gap = paste0(cumsum(c(0, diff(DateTime, units = "days") > 120)), "_", SiteCode))
    
  plot <- ggplot2::ggplot(data = depthGaps,
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
    ggplot2::scale_y_reverse() +
    ggplot2::facet_grid(Park~.,
                        scale = "free_y")
  
  return(plot)
}

#' Plot continuous well depth values from Aquarius
#'
#' @param park Mandatory, e.g., "GRBA"
#' @param field.season Optional, e.g., c("2017", "2019", "2020")
#' @param site Optional, e.g., "LAKE_W_ROGE0"
#'
#' @returns ggplot object
#' @export
#'
#' @examples
continuousWellDepthPlot <- function(park, field.season, site) {
  depthImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "TimeseriesDepth")
  
  depthGaps <- depthImport |>
    dplyr::group_by(Park, SiteCode) |>
    dplyr::mutate(Gap = paste0(cumsum(c(0, diff(DateTime, units = "hours") > 1)), "_", SiteCode)) |>
    dplyr::ungroup() |>
    dplyr::select(Park, SiteCode, DateTime, FieldSeason, Value, Grade, Approval, Gap)
  
  plot <- ggplot2::ggplot(data = depthGaps,
                          ggplot2::aes(x = DateTime,
                                       y = Value,
                                       color = SiteCode,
                                       group = Gap
                          )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Depth to water from ground surface (ft)") +
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(.~Park)
  
  return(plot)
}

#' Return discrete well depth readings for Aquarius and Survey123 entries
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_W_BAKR0_DP1".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
wellComparison <- function(park, field.season, site) {
  wellDepth <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "VisitWell")
  fieldVisit <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "TimeseriesFieldVisit") |>
    dplyr::relocate(Unit, .after = MonitoringMethod)

  aquarius <- fieldVisit |>
    dplyr::select(Park, SiteCode, DateTime, Parameter, Unit, Value) |>
    dplyr::mutate(Source = "Aquarius") |>
    dplyr::filter(Parameter %in% c("DepthToWaterFromGround", "Water Level"))
  
  survey <- wellDepth |>
    dplyr::rename(Value = WLBelowLSD_ft) |>
    dplyr::mutate(Unit = "ft",
                  Source = "Survey",
                  Parameter = "DepthToWaterFromGround") |>
    dplyr::select(Park, SiteCode, DateTime, Parameter, Unit, Value, Source) |>
    dplyr::filter(lubridate::year(DateTime) >= 2010)
  
  discrete <- rbind(survey, aquarius)
  
  return(discrete)
}

#' Plot discrete well depth values for Aquarius and Survey123 entries
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_W_BAKR0_DP1".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
wellComparisonPlot <- function(park, field.season, site) {
  wellData <- wellComparison(park = park, field.season = field.season, site = site) |>
    dplyr::filter(Parameter %in% c("DepthToWaterFromGround"))
  
  plot <- ggplot2::ggplot(wellData,
                          ggplot2::aes(x = DateTime,
                                       y = Value,
                                       color = Source,
                                       shape = Source)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_grid(SiteCode~Parameter,
                        scales = "free_y") +
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggplot2::labs(title = "Comparison of Aquarius and Survey123 well depth measurements",
                  x = "Year",
                  y = "Depth to Water from Ground (ft)",
                  color = "Source") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_datetime(date_breaks = "1 year",
                              date_labels = "%Y") +
    khroma::scale_color_muted() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  
  return(plot)
}