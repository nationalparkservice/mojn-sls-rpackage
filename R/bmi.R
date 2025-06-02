#' Return list of sample metrics determined by laboratory
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @exportfe
#'
#' @examples
bmiMetrics <- function(park, site, field.season) {
  metricsImport <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "BMIMetrics")
  
  metricsList <- metricsImport |>
    dplyr::filter(AnalysisType == "Routine") |>
    dplyr::select(-c("SampleID", "SiteName", "AnalysisType", "InvasiveSpeciesList")) |>
    dplyr::mutate(Metric = dplyr::case_when(grepl("Richness", Attribute) ~ "Richness",
                                            grepl("Density", Attribute) ~ "Density",
                                            grepl("Hilsenhoff|Shannons|Simpsons|Evenness", Attribute) ~ "Index",
                                            grepl("DominantFamily", Attribute) ~ "Fraction",
                                            grepl("DominantTaxon", Attribute) ~ "Fraction",
                                            TRUE ~ as.character(Attribute))) |>
    dplyr::mutate(Category = dplyr::case_when(Attribute %in% c("UniqueRichness") ~ "Overall",
                                              Attribute %in% c("Density") ~ "Overall",
                                              grepl("Feed", Attribute) ~ "Functional Feeding Group",
                                              grepl("Habit", Attribute) ~ "Habit",
                                              grepl("LongLived|Intolerant|Tolerant", Attribute) ~ "Sensitivity",
                                              grepl("Insecta|Ephemeroptera|Plecoptera|Trichoptera|Coleoptera|Elmidae|Diptera|Chironomidae|Megaloptera|Crustacea|NonInsects|Oligochaeta|Mollusca", Attribute) ~ "Taxa Group",
                                              grepl("DominantFamily", Attribute) ~ "Dominant Family",
                                              grepl("DominantTaxon", Attribute) ~ "Dominant Taxon",
                                              grepl("Hilsenhoff", Attribute) ~ "Hilsenhoff",
                                              grepl("Shannons", Attribute) ~ "Shannons",
                                              grepl("Simpsons", Attribute) ~ "Simpsons",
                                              grepl("Evenness", Attribute) ~ "Evenness",
                                              TRUE ~ as.character(Attribute))) |>
    dplyr::mutate(Type = dplyr::case_when(grepl("CollectorFilterer", Attribute) ~ "Collector Filterer",
                                          grepl("CollectorGatherer", Attribute) ~ "Collector Gatherer",
                                          grepl("Scraper", Attribute) ~ "Scraper",
                                          grepl("Shredder", Attribute) ~ "Shredder",
                                          grepl("Parasite", Attribute) ~ "Parasite",
                                          grepl("Predator", Attribute) ~ "Predator",
                                          grepl("PiercerHerbivore", Attribute) ~ "Piercer Herbivore",
                                          grepl("Clinger", Attribute) ~ "Clinger",
                                          grepl("Planktonic", Attribute) ~ "Planktonic",
                                          grepl("Skater", Attribute) ~ "Skater",
                                          grepl("Climber", Attribute) ~ "Climber",
                                          grepl("Crawler", Attribute) ~ "Crawler",
                                          grepl("Swimmer", Attribute) ~ "Swimmer",
                                          grepl("Burrower", Attribute) ~ "Burrower",
                                          grepl("Sprawler", Attribute) ~ "Sprawler",
                                          grepl("LongLived", Attribute) ~ "Long Lived",
                                          grepl("Intolerant", Attribute) ~ "Intolerant",
                                          grepl("Tolerant", Attribute) ~ "Tolerant",
                                          grepl("Insecta", Attribute) ~ "Insecta",
                                          grepl("Ephemeroptera", Attribute) ~ "Ephemeroptera",
                                          grepl("Plecoptera", Attribute) ~ "Plecoptera",
                                          grepl("Trichoptera", Attribute) ~ "Trichoptera",
                                          grepl("Coleoptera", Attribute) ~ "Coleoptera",
                                          grepl("Elmidae", Attribute) ~ "Elmidae",
                                          grepl("Diptera", Attribute) ~ "Diptera",
                                          grepl("Chironomidae", Attribute) ~ "Chironomidae",
                                          grepl("Megaloptera", Attribute) ~ "Megaloptera",
                                          grepl("Crustacea", Attribute) ~ "Crustacea",
                                          grepl("NonInsects", Attribute) ~ "NonInsects",
                                          grepl("Oligochaeta", Attribute) ~ "Oligochaeta",
                                          grepl("Mollusca", Attribute) ~ "Mollusca",
                                          TRUE ~ NA_character_)) |>
    dplyr::mutate(Label = dplyr::case_when(Category %in% c("Functional Feeding Group", "Habit", "Sensitivity") ~ paste0(Category, ": ", Type),
                                           Category %in% c("Taxa Group") ~ paste0(Category, ": ", Type),
                                           Category %in% c("Overall") ~ paste0(Category, " ", Metric),
                                           Metric %in% c("Index") ~ paste0(Metric, ": ", Category),
                                           Metric %in% c("Fraction") ~ paste0(Metric, ": ", Category),
                                           TRUE ~ NA_character_))
    
  return(metricsList)
} 

#' Return list of aquatic invertebrate taxa identified in samples
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
bmiSpecies <- function(park, site, field.season) {
  speciesImport <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "BMISpecies")
  
  invertList <- speciesImport |>
    dplyr::select(-c("SampleID", "SiteName"))
    
  return(invertList)
}

#' Return list of invasive aquatic invertebrates identified in samples
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @exportfe
#'
#' @examples
bmiInvasives <- function(park, site, field.season) {
  metricsImport <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "BMIMetrics")
  
  invasivesList <- metricsImport |>
    dplyr::select(Park, SiteCode, SubsiteCode, FieldSeason, CollectionDate, InvasiveSpeciesList) |>
    dplyr::filter(InvasiveSpeciesList != "Absent") |>
    unique() |>
    dplyr::arrange(SubsiteCode, FieldSeason, CollectionDate)
  
  return(invasivesList)
}

#' Plot overall density and richness metrics
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
bmiOverallPlot <- function(park, site, field.season) {
  metricsList <- bmiMetrics(park = park, site = site, field.season = field.season)
  
  overall <- metricsList |>
    dplyr::mutate(Year = as.Date(FieldSeason, format = "%Y")) |>
    dplyr::filter(Category == "Overall")
 
  plot <- ggplot2::ggplot(data = overall,
                          ggplot2::aes(x = Year,
                                       y = Value,
                                       # color = Type,
                                       # shape = Type,
                                       group = Type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_discreterainbow() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Count") +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(Metric~SubsiteCode,
                        scales = "free_y")
  
  return(plot) 
}

#' Plot taxonomic dominance metrics
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
bmiDominancePlot <- function(park, site, field.season) {
  metricsList <- bmiMetrics(park = park, site = site, field.season = field.season)
  
  overall <- metricsList |>
    dplyr::mutate(Year = as.Date(FieldSeason, format = "%Y")) |>
    dplyr::filter(Category %in% c("Dominant Family", "Dominant Taxon"))
  
  plot <- ggplot2::ggplot(data = overall,
                          ggplot2::aes(x = Year,
                                       y = Value,
                                       color = Category,
                                       # shape = Type,
                                       group = Category)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_discreterainbow() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Count") +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(limits = c(0, NA),
                                labels = function(x) paste0(x*100, "%")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(Metric~SubsiteCode,
                        scales = "free_y")
  
  return(plot) 
}

#' Plot taxonomic density and richness metrics
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
bmiTaxaPlot <- function(park, site, field.season) {
  metricsList <- bmiMetrics(park = park, site = site, field.season = field.season)
  
  taxa <- metricsList |>
    dplyr::filter(Category == "Taxa Group")
  
  plot <- ggplot2::ggplot(data = taxa,
                          ggplot2::aes(x = FieldSeason,
                                       y = Value,
                                       color = Type,
                                       # shape = Type,
                                       group = Type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_discreterainbow() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Count") +
    # ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(Metric~SubsiteCode,
                        scales = "free_y")
    
  return(plot)
}

#' Plot function feeding group density and richness metrics
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
bmiFunctionalPlot <- function(park, site, field.season) {
  metricsList <- bmiMetrics(park = park, site = site, field.season = field.season)

  functional <- metricsList |>
    dplyr::filter(Category == "Functional Feeding Group")
  
  plot <- ggplot2::ggplot(data = functional,
                          ggplot2::aes(x = FieldSeason,
                                       y = Value,
                                       color = Type,
                                       # shape = Type,
                                       group = Type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_discreterainbow() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Count") +
    # ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(Metric~SubsiteCode,
                        scales = "free_y")
  
  return(plot)
}

#' Plot habit density and richness metrics
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
bmiHabitPlot <- function(park, site, field.season) {
  metricsList <- bmiMetrics(park = park, site = site, field.season = field.season)

  habit <- metricsList |>
    dplyr::filter(Category == "Habit")
  
  plot <- ggplot2::ggplot(data = habit,
                          ggplot2::aes(x = FieldSeason,
                                       y = Value,
                                       color = Type,
                                       # shape = Type,
                                       group = Type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_discreterainbow() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Count") +
    # ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(Metric~SubsiteCode,
                        scales = "free_y")
  
  return(plot)
}

#' Plot tolerance density and richness metrics
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
bmiTolerancePlot <- function(park, site, field.season) {
  metricsList <- bmiMetrics(park = park, site = site, field.season = field.season)
  
  tolerance <- metricsList |>
    dplyr::filter(Category == "Sensitivity")
  
  plot <- ggplot2::ggplot(data = tolerance,
                          ggplot2::aes(x = FieldSeason,
                                       y = Value,
                                       color = Type,
                                       # shape = Type,
                                       group = Type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_discreterainbow() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Count") +
    # ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(Metric~SubsiteCode,
                        scales = "free_y")
  
  return(plot)
}

#' Plot diversity indices
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns
#' @export
#'
#' @examples
bmiIndexPlot <- function(park, site, field.season) {
  metricsList <- bmiMetrics(park = park, site = site, field.season = field.season)

  index <- metricsList |>
    dplyr::filter(Metric == "Index")
  
  plot <- ggplot2::ggplot(data = index,
                          ggplot2::aes(x = FieldSeason,
                                       y = Value,
                                       # color = Category,
                                       # shape = Type,
                                       group = Category
                                       )) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    khroma::scale_color_discreterainbow() +
    ggplot2::labs(title = "",
                  x = "Year",
                  y = "Value") +
    # ggplot2::scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    # ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(Category~SubsiteCode,
                        scales = "free_y")
  
  return(plot)
}