#' Return median surface water quality measurements
#'
#' @param park Optional, e.g., "DEVA"
#' @param field.season Optional, e.g., c("2017", "2019", "2020")
#' @param site Optional, e.g., "JOTR_P_FORT0"
#'
#' @returns Tibble
#' @export
#'
#' @examples
waterQuality <- function(park, field.season, site) {
  qualityImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "WaterQuality")
  
  qualitySurface <- qualityImport |>
    dplyr::group_by(Park, SiteCode, SubsiteCode, DateTime) |>
    dplyr::filter(MeasurementDepth_ft == min(MeasurementDepth_ft) | is.na(MeasurementDepth_ft)) |>
    dplyr::ungroup() |>
    dplyr::select(-c("IsDepthProfile", "MeasurementDepth_ft", "DepthToBottom_ft")) |>
    dplyr::arrange(SubsiteCode, DateTime)
  
  return(qualitySurface)
}

#' Plot water temperature over time for sites within a park
#'
#' @param park Mandatory, e.g., "DEVA"
#' @param field.season Optional, e.g., c("2017", "2019", "2020")
#' @param site Optional, e.g., "JOTR_P_FORT0"
#'
#' @returns ggplot object
#' @export
#'
#' @examples
temperaturePlot <- function(park, field.season, site) {
  qualitySurface <- waterQuality(park = park, field.season = field.season, site = site) |>
    dplyr::group_by(Park, SiteCode, SubsiteCode) |>
    dplyr::mutate(Gap = paste0(cumsum(c(0, diff(DateTime, units = "days") > 120)), "_", SubsiteCode)) |>
    dplyr::ungroup()
  
  plot <- ggplot2::ggplot(data = qualitySurface,
                          ggplot2::aes(x = DateTime,
                                       y = Temperature_C,
                                       color = SubsiteCode,
                                       shape = SubsiteCode,
                                       group = Gap
                           )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Temperature (C)") +
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  return(plot)
}

#' Plot pH over time for sites within a park
#'
#' @param park Mandatory, e.g., "DEVA"
#' @param field.season Optional, e.g., c("2017", "2019", "2020")
#' @param site Optional, e.g., "JOTR_P_FORT0"
#'
#' @returns ggplot object
#' @export
#'
#' @examples
phPlot <- function(park, field.season, site) {
  qualitySurface <- waterQuality(park = park, field.season = field.season, site = site)
  
  plot <- ggplot2::ggplot(data = qualitySurface,
                          ggplot2::aes(x = DateTime,
                                       y = pH,
                                       color = SubsiteCode,
                                       shape = SubsiteCode,
                                       group = Gap
                          )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "pH") +
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}

#' Plot specific conductance over time for sites within a park
#'
#' @param park Mandatory, e.g., "DEVA"
#' @param field.season Optional, e.g., c("2017", "2019", "2020")
#' @param site Optional, e.g., "JOTR_P_FORT0"
#'
#' @returns ggplot object
#' @export
#'
#' @examples
spcondPlot <- function(park, field.season, site) {
  qualitySurface <- waterQuality(park = park, field.season = field.season, site = site)
  
  plot <- ggplot2::ggplot(data = qualitySurface,
                          ggplot2::aes(x = DateTime,
                                       y = SpCond_uS_per_cm,
                                       color = SubsiteCode,
                                       shape = SubsiteCode,
                                       group = Gap
                          )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Specific Conductance (uS/cm)") +
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}

#' Plot dissolved oxygen concentration over time for sites within a park
#'
#' @param park Mandatory, e.g., "DEVA"
#' @param field.season Optional, e.g., c("2017", "2019", "2020")
#' @param site Optional, e.g., "JOTR_P_FORT0"
#'
#' @returns ggplot object
#' @export
#'
#' @examples
doPlot <- function(park, field.season, site) {
  qualitySurface <- waterQuality(park = park, field.season = field.season, site = site)
  
  plot <- ggplot2::ggplot(data = qualitySurface,
                          ggplot2::aes(x = DateTime,
                                       y = DO_mg_per_L,
                                       color = SubsiteCode,
                                       shape = SubsiteCode,
                                       group = Gap
                          )) + # add gaps to line in plot when visits were missed
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_bright() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Dissolved Oxygen (mg/L)") +
    ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}