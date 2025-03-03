#' Calculate total surface water area for all pools
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns Tibble
#' @export
#'
#' @examples
poolArea <- function(park, field.season, site) {
  areaImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "Pool")
  
  areaCalculated <- areaImport |>
    dplyr::group_by(Park, SiteCode, DateTime) |>
    dplyr::summarize(TotalArea_m_sq = sum(PoolArea_m_sq)) |>
    dplyr::ungroup()
  
  return(areaCalculated)
}

#' Calculate total channel length for all outlets
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns Tibble
#' @export
#'
#' @examples
outletLength <- function(park, field.season, site) {
  lengthImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "Outlet")
  
  lengthCalculated <- lengthImport |>
    dplyr::group_by(Park, SiteCode, DateTime) |>
    dplyr::summarize(TotalLength_m = sum(SpringbrookLength_m)) |>
    dplyr::ungroup()
  
  return(lengthCalculated)
}

#' Plot total surface water area for all pools
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns ggplot object
#' @export
#'
#' @examples
poolAreaPlot <- function(park, field.season, site) {
  areaCalculated <- poolArea(park = park, field.season = field.season, site = site)

  plot <- ggplot2::ggplot(data = areaCalculated,
                          ggplot2::aes(x = DateTime,
                                       y = TotalArea_m_sq,
                                       color = SiteCode,
                                       group = cumsum(c(0, diff(DateTime) > 120)))) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Total Area (m2)")+
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  
  return(plot)
}

#' Plot total channel length for all outlets
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns ggplot object
#' @export
#'
#' @examples
outletLengthPlot <- function(park, field.season, site) {
  lengthCalculated <- outletLength(park = park, field.season = field.season, site = site)
  
  plot <- ggplot2::ggplot(data = lengthCalculated,
                          ggplot2::aes(x = DateTime,
                                       y = TotalLength_m,
                                       color = SiteCode,
                                       group = cumsum(c(0, diff(DateTime) > 120)))) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Total Length (m)")+
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  
  return(plot)
}

#' Return observed staff gage heights
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns Tibble
#' @export
#'
#' @examples
gageReading <- function(park, field.season, site) {
  gageImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "Gage")
  visitImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "VisitQuarterly")
  
  readingSorted <- gageImport |>
    dplyr::arrange(SiteCode, DateTime, GageTime) |>
    dplyr::left_join(visitImport, by = c("Park", "SiteCode", "DateTime")) |>
    dplyr::select(Park, SiteCode, DateTime, GageTime, GageHeight_ft, FlumeWeirNotes, OverallNotes)
  
  return(readingSorted)
}

#' Plot gage readings over time
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns
#' @export
#'
#' @examples
gageReadingPlot <- function(park, field.season, site) {
  readingSorted <- gageReading(park = park, field.season = field.season, site = site)
  
  plot <- ggplot2::ggplot(data = readingSorted,
                          ggplot2::aes(x = DateTime,
                                       y = GageHeight_ft,
                                       color = SiteCode,
                                       shape = SiteCode
                                       #, group = cumsum(c(0, diff(DateTime) > 120))
                                       )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Gage Height (ft)")+
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}

#' Calculate volumetric discharge based on container volume and fill time
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns Tibble
#' @export
#'
#' @examples
volumetricDischarge <- function(park, field.season, site) {
  volumetricImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "FillTime")
  visitImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "VisitQuarterly")
  
  dischargeCalculated <- volumetricImport |>
    dplyr::group_by(Park, SiteCode, DateTime) |>
    dplyr::summarize(MedianFillTime_sec = median(FillTime_sec)) |>
    dplyr::ungroup() |>
    dplyr::left_join(visitImport, by = c("Park", "SiteCode", "DateTime")) |>
    dplyr::select(Park, SiteCode, DateTime, ContainerVolume_L, PercentFlowCaptured, MedianFillTime_sec, FlumeWeirNotes, OverallNotes) |>
    dplyr::mutate(Discharge_L_per_sec = (ContainerVolume_L/MedianFillTime_sec)*(PercentFlowCaptured/100)) |>
    dplyr::mutate(Discharge_cfs = Discharge_L_per_sec * 0.035315) |>
    dplyr::relocate(Discharge_L_per_sec, .after = "MedianFillTime_sec") |>
    dplyr::relocate(Discharge_cfs, .after = "Discharge_L_per_sec")
    
  return(dischargeCalculated)
}

#' Plot discharge readings over time
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns ggplot object
#' @export
#'
#' @examples
volumetricDischargePlot <- function(park, field.season, site) {
  dischargeCalculated <- volumetricDischarge(park = park, field.season = field.season, site = site)
  
  plot <- ggplot2::ggplot(data = dischargeCalculated,
                          ggplot2::aes(x = DateTime,
                                       y = Discharge_cfs,
                                       color = SiteCode,
                                       shape = SiteCode
                                       #, group = cumsum(c(0, diff(DateTime) > 120))
                          )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Discharge (cfs)")+
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}