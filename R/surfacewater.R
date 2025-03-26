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

#' Plot staff gage readings over time
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

#' Plot volumetric discharge readings over time
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

#' Return continuous discharge values and grades from Aquarius for all sites
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns
#' @export
#'
#' @examples
dischargeContinuous <- function(park, field.season, site) {
  dischargeImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "TimeseriesDischarge")

  dischargeDaily <- dischargeImport

  return(dischargeDaily)
}

#' Plot continuous discharge values from Aquarius for a single site
#'
#' @param park 
#' @param field.season 
#' @param site Mandatory.
#'
#' @returns
#' @export
#'
#' @examples
dischargeContinuousPlot <- function(park, field.season, site) {
  dischargeDaily <- dischargeContinuous(park = park, field.season = field.season, site = site) # |>
    # dplyr::filter(SiteCode == site)
  
  # dischargeVolumetric <- volumetricDischarge(park = park, field.season = field.season, site = site) |>
  #   dplyr::filter(SiteCode == site)
  
  plot <- ggplot2::ggplot(data = dischargeDaily,
                          ggplot2::aes(x = DateTime,
                                       y = Value,
                                       color = SiteCode,
                                       group = cumsum(c(0, diff(DateTime) > 60)) # show gaps greater than 60 minutes
                          )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
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

#' Return continuous stage values and grades from Aquarius for all sites
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns
#' @export
#'
#' @examples
stageContinuous <- function(park, field.season, site) {
  stageImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "TimeseriesStage")

  stageDaily <- stageImport
  
  return(stageDaily)
}

#' Plot continuous stage values from Aquarius for a single site
#'
#' @param park 
#' @param field.season 
#' @param site Mandatory.
#'
#' @returns
#' @export
#'
#' @examples
stageContinuousPlot <- function(park, field.season, site) {
  stageDaily <- stageContinuous(park = park, field.season = field.season, site = site) |>
    dplyr::filter(SiteCode == site)
  
  readingSorted <- gageReading(park = park, field.season = field.season, site = site) |>
    dplyr::filter(SiteCode == site)
  
  plot <- ggplot2::ggplot(data = stageDaily,
                          ggplot2::aes(x = DateTime,
                                       y = Value,
                                       color = SiteCode,
                                       group = cumsum(c(0, diff(DateTime) > 3600)) # show gaps greater than 3600 seconds (1 hour)
                          )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Stage (ft)") +
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}