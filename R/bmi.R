#' Return list of sample metrics determined by laboratory
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns Tibble
#' @exportfe
#'
#' @examples
bmiMetrics <- function(park, field.season, site) {
  metricsImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "BMIMetrics")
  
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

#' Return taxonomic list of aquatic invertebrates identified in samples
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns Tibble
#' @export
#'
#' @examples
bmiSpecies <- function(park, field.season, site) {
  speciesImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "BMISpecies")
  
  invertList <- speciesImport |>
    dplyr::mutate(Phylum = dplyr::case_when(ScientificName == "Oligochaeta" ~ "Annelida",
                                            ScientificName == "Nematoda" ~ "Nematoda",
                                            ScientificName == "Lumbriculata" ~ "Annelida",
                                            ScientificName == "Platyhelminthes" ~ "Platyhelminthes",
                                            ScientificName == "Xenacoelomorpha" ~ "Xenacoelomorpha",
                                            TRUE ~ Phylum)) |>
    dplyr::mutate(Class = dplyr::case_when(ScientificName == "Oligochaeta" ~ "Clitellata",
                                           ScientificName == "Lumbriculata" ~ "Clitellata",
                                           ScientificName == "Branchiobdellidae" ~ "Clitellata",
                                           ScientificName == "Erpobdellidae" ~ "Clitellata",
                                           ScientificName == "Collembola" ~ "Collembola",
                                           TRUE ~ Class)) |>
    dplyr::filter(!(ScientificName %in% c("Actinopterygii", "Anura"))) |>
    dplyr::select(-c("SampleID", "SiteName"))
    
  return(invertList)
}

#' Return list of invasive aquatic invertebrates identified in samples
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns Tibble
#' @exportfe
#'
#' @examples
bmiInvasives <- function(park, field.season, site) {
  metricsImport <- ReadAndFilterData(park = park, field.season = field.season, site = site, data.name = "BMIMetrics")
  
  invasivesList <- metricsImport |>
    dplyr::select(Park, SiteCode, SubsiteCode, FieldSeason, CollectionDate, InvasiveSpeciesList) |>
    dplyr::filter(InvasiveSpeciesList != "Absent") |>
    unique() |>
    dplyr::arrange(SubsiteCode, FieldSeason, CollectionDate)
  
  return(invasivesList)
}

#' Title
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns
#' @export
#'
#' @examples
bmiOverallPlot <- function(park, field.season, site) {
  metricsList <- bmiMetrics(park = park, field.season = field.season, site = site)
  
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

#' Title
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns
#' @export
#'
#' @examples
bmiTaxaPlot <- function(park, field.season, site) {
  metricsList <- bmiMetrics(park = park, field.season = field.season, site = site)
  
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

#' Title
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns
#' @export
#'
#' @examples
bmiFunctionalPlot <- function(park, field.season, site) {
  metricsList <- bmiMetrics(park = park, field.season = field.season, site = site)

  functional <- metricsList |>
    dplyr::filter(Category == "Functional Feeding Group")
  
  plot <- ggplot2::ggplot(data = functional |> dplyr::filter(Park == "LAKE"),
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

#' Title
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns
#' @export
#'
#' @examples
bmiHabitPlot <- function(park, field.season, site) {
  metricsList <- bmiMetrics(park = park, field.season = field.season, site = site)

  habit <- metricsList |>
    dplyr::filter(Category == "Habit")
  
  plot <- ggplot2::ggplot(data = habit |> dplyr::filter(Park == "LAKE"),
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

#' Title
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns
#' @export
#'
#' @examples
bmiTolerancePlot <- function(park, field.season, site) {
  metricsList <- bmiMetrics(park = park, field.season = field.season, site = site)
  
  tolerance <- metricsList |>
    dplyr::filter(Category == "Sensitivity")
  
  plot <- ggplot2::ggplot(data = tolerance |> dplyr::filter(Park == "LAKE"),
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

#' Title
#'
#' @param park 
#' @param field.season 
#' @param site 
#'
#' @returns
#' @export
#'
#' @examples
bmiIndexPlot <- function(park, field.season, site) {
  metricsList <- bmiMetrics(park = park, field.season = field.season, site = site)

  index <- metricsList |>
    dplyr::filter(Metric == "Index")
  
  plot <- ggplot2::ggplot(data = index |> dplyr::filter(Park == "LAKE"),
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