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

#' Return observed staff gage heights with gage and overall notes
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
    dplyr::select(Park, SiteCode, DateTime, GageTime, GageHeight_ft, GageNotes, OverallNotes)
  
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
  readingImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "Gage") |>
    dplyr::filter(!(Park %in% c("DEVA")))
  
  plot <- ggplot2::ggplot(data = readingImport,
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
  dischargeCalculated <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "VolumetricDischarge")
  
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
#' @param park Optional. Four-letter park code to filter on, e.g. "LAKE".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
continuousDischarge <- function(park, field.season, site) {
  dischargeImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "TimeseriesDischarge")

  dischargeDaily <- dischargeImport

  return(dischargeDaily)
}

#' Plot continuous discharge values from Aquarius for a single site
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "LAKE".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Mandatory. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
continuousDischargePlot <- function(park, field.season, site) {
  dischargeDaily <- continuousDischarge(park = park, field.season = field.season, site = site) |>
    dplyr::filter(SiteCode == site)
  
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
#' @param park Optional. Four-letter park code to filter on, e.g. "LAKE".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
continuousStage <- function(park, field.season, site) {
  stageImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "TimeseriesStage")

  stageDaily <- stageImport
  
  return(stageDaily)
}

#' Plot continuous stage values from Aquarius for a single site
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "LAKE".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Mandatory. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
continuousStagePlot <- function(park, field.season, site) {
  stageDaily <- continuousStage(park = park, field.season = field.season, site = site) |>
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

#' Return discrete stage and discharge readings for Aquarius and Survey123 entries
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "LAKE".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
discreteComparison <- function(park, field.season, site) {
  gageReading <- gageReading(park = park, field.season = field.season, site = site)
  volumetricDischarge <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "VolumetricDischarge")
  fieldVisit <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "TimeseriesFieldVisit") |>
    dplyr::relocate(Unit, .after = MonitoringMethod)
  aquariusVolumetric <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "TimeseriesVolumetric")
  
  aquarius <- rbind(fieldVisit, aquariusVolumetric) |>
    dplyr::select(Park, SiteCode, DateTime, Parameter, Unit, Value) |>
    dplyr::mutate(Source = "Aquarius")
  
  gageForm <- gageReading |>
    dplyr::rename(Value = GageHeight_ft) |>
    dplyr::mutate(Unit = "ft",
                  Source = "Survey",
                  Parameter = "Stage") |>
    dplyr::select(Park, SiteCode, DateTime, Parameter, Unit, Value, Source) |>
    dplyr::filter(Park %in% c("LAKE", "MOJA", "PARA")) |>
    dplyr::mutate(Value = dplyr::case_when(SiteCode == "LAKE_P_BLUE0" ~ Value + 0.02,
                                           TRUE ~ Value))
  
  volumetricForm <- volumetricDischarge |>
    dplyr::rename(Value = Discharge_cfs) |>
    dplyr::mutate(Unit = "ft^3/s",
                  Source = "Survey",
                  Parameter = "Discharge") |>
    dplyr::select(Park, SiteCode, DateTime, Parameter, Unit, Value, Source) |>
    dplyr::filter(Park %in% c("LAKE", "MOJA", "PARA"))
  
  survey <- rbind(gageForm, volumetricForm)
  
  discrete <- rbind(survey, aquarius)
  
  return(discrete)
}

#' Plot discrete stage values for Aquarius and Survey123 entries
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "LAKE".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
discreteStageComparisonPlot <- function(park, field.season, site) {
  discreteData <- discreteComparison(park = park, field.season = field.season, site = site) |>
    dplyr::filter(Parameter %in% c("Stage"))
  
  plot <- ggplot2::ggplot(discreteData,
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
    ggplot2::labs(title = "Comparison of Aquarius and Survey123 stage measurements",
                  x = "Year",
                  y = "Stage (ft)",
                  color = "Source") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_x_datetime(date_breaks = "1 year",
                              date_labels = "%Y") +
    khroma::scale_color_muted() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  
  return(plot)
}

#' Plot discrete discharge values for Aquarius and Survey123 entries
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "LAKE".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
discreteDischargeComparisonPlot <- function(park, field.season, site) {
  discreteData <- discreteComparison(park = park, field.season = field.season, site = site) |>
    dplyr::filter(Parameter %in% c("Discharge"))
  
  plot <- ggplot2::ggplot(discreteData,
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
    ggplot2::labs(title = "Comparison of Aquarius and Survey123 discharge measurements",
                  x = "Year",
                  y = "Discharge (ft)",
                  color = "Source") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_x_datetime(date_breaks = "1 year",
                              date_labels = "%Y") +
    khroma::scale_color_muted() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  
  return(plot)
}