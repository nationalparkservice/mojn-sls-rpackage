#' Create tibble with MDL and ML values for each characteristic.
#'
#' @returns A tibble with columns Characteristic, Unit, StartYear, EndYear, MDL, ML.
#' @export
#'
getMDLLookup <- function() {
  lookup <- tibble::tibble(Characteristic = c("ALK2", "Ca", "DOC", "Cl", "Mg", "NO3-N+NO2-N", "NO3-N", "UTN", "UTP", "K", "Na", "SO4-S"),
                           Unit = c("mg CaCO3/L", "mg/L", "mg C/L", "mg/L", "mg/L", "mg N/L", "mg N/L", "mg N/L", "mg P/L", "mg/L", "mg/L", "mg S/L"),
                           StartYear = c(2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009),
                           EndYear = c(2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025),
                           MDL = c(0.2, 0.06, 0.05, 0.01, 0.02, 0.001, 0.001, 0.01, 0.002, 0.03, 0.01, 0.01),
                           ML = c(0.6, 0.19, 0.16, 0.03, 0.06, 0.003, 0.003, 0.03, 0.006, 0.10, 0.03, 0.03))
  
  return(lookup)
}

#' List all routine laboratory values that are less than or equal to the minimum detection level (MDL) for that analyte.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
chemMDL <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  lookup <- getMDLLookup()
  
  chemData <- chem |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value) |>
    dplyr::mutate(FieldSeason = as.double(FieldSeason))
  
  chemDataMerged <- fuzzyjoin::fuzzy_inner_join(x = chemData,
                                                y = lookup,
                                                by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                                match_fun = list(`==`, `==`, `>=`, `<=`))
  
  chemMDL <- chemDataMerged |>
    dplyr::filter(FieldSeason >= StartYear & FieldSeason <= EndYear) |>
    dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) |>
    dplyr::mutate(FieldSeason = as.character(FieldSeason)) |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value, MDL) |>
    dplyr::filter(Value <= MDL)
  
  return(chemMDL)
}

#' List all routine laboratory values that are less than or equal to the minimum level of quantitation (ML) for that analyte.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
chemML <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  lookup <- getMDLLookup()
  
  chemData <- chem |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value) |>
    dplyr::mutate(FieldSeason = as.double(FieldSeason))
  
  chemDataMerged <- fuzzyjoin::fuzzy_inner_join(x = chemData,
                                                y = lookup,
                                                by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                                match_fun = list(`==`, `==`, `>=`, `<=`))
  
  chemML <- chemDataMerged |>
    dplyr::filter(FieldSeason >= StartYear & FieldSeason <= EndYear) |>
    dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) |>
    dplyr::mutate(FieldSeason = as.character(FieldSeason)) |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value, ML) |>
    dplyr::filter(Value <= ML)
  
  return(chemML)
}

#' Calculate the relative percent difference (RPD) for laboratory duplicates and triplicates, flag results that exceed the 30% MQO threshold, and list all RPD values and flags.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
#' \dontrun{
#' chemLabDuplicates()
#' chemLabDuplicates(site = c("PARA_P_PAKO0", "PARA_P_TASS0"), field.season = c("2020", "2024"))
#' }
chemLabDuplicates <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  
  labDupes <- chem |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, SampleType, AnalysisType, Characteristic, Unit, Value)
  
  labDupesWide <- tidyr::pivot_wider(data = labDupes, names_from = AnalysisType, values_from = Value)
  
  labDupesList <- labDupesWide |>
    dplyr::filter(!is.na(Duplicate)) |>
    dplyr::mutate(RPD = round(((pmax(Routine, Duplicate) - pmin(Routine, Duplicate))/((pmax(Routine, Duplicate) + pmin(Routine, Duplicate))/2))*100, 2)) |>
    dplyr::mutate(RPD2 = round(((pmax(Routine, Triplicate) - pmin(Routine, Triplicate))/((pmax(Routine, Triplicate) + pmin(Routine, Triplicate))/2))*100, 2)) |>
    dplyr::mutate(RPDFlag = ifelse(RPD > 30 | RPD2 > 30, "RPD above laboratory precision MQO of 30%", NA)) |>
    dplyr::arrange(desc(RPD))
  
  return(labDupesList)
}

#' Calculate the relative percent difference (RPD) for field replicates, flag results that exceed the 30% MQO threshold, and list all RPD values and flags.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
#' \dontrun{
#' chemFieldReplicates()
#' chemFieldReplicates(site = c("PARA_P_PAKO0", "PARA_P_TASS0"), field.season = c("2020", "2024"))
#' }
chemFieldReplicates <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  
  fieldReps <- chem |>
   dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, SampleType, AnalysisType, Characteristic, Unit, Value) |>
   dplyr::filter(SampleType %in% c("Routine", "Replicate"))
  
  fieldRepsWide <- tidyr::pivot_wider(data = fieldReps, names_from = SampleType, values_from = Value)

  fieldRepsList <- fieldRepsWide |>
    dplyr::filter(!is.na(Replicate)) |>
    dplyr::mutate(RPD = round(((pmax(Routine, Replicate) - pmin(Routine, Replicate))/((pmax(Routine, Replicate) + pmin(Routine, Replicate))/2))*100, 2)) |>
    dplyr::mutate(RPDFlag = ifelse(RPD > 30, "RPD above laboratory precision MQO of 30%", NA)) |>
    dplyr::arrange(desc(RPD))
  
  return(fieldRepsList)
}

#' List all laboratory values from field blanks that exceed the minimum detection level (MDL) for that analyte.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
#' \dontrun{
#' chemFieldBlanks()
#' chemFieldBlanks(site = c("PARA_P_PAKO0", "PARA_P_TASS0"), field.season = c("2020", "2024"))
#' }
chemFieldBlanks <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  lookup <- getMDLLookup()
    
  fieldBlanks <- chem |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value) |>
    dplyr::filter(SampleType %in% c("Blank")) |>
    dplyr::mutate(FieldSeason = as.double(FieldSeason))
  
  fieldBlanksMerged <- fuzzyjoin::fuzzy_inner_join(x = fieldBlanks,
                                                   y = lookup,
                                                   by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                                   match_fun = list(`==`, `==`, `>=`, `<=`))
  
  fieldBlanksList <- fieldBlanksMerged |>
    dplyr::filter(FieldSeason >= StartYear & FieldSeason <= EndYear) |>
    dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) |>
    dplyr::mutate(FieldSeason = as.character(FieldSeason)) |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value, MDL) |>
    dplyr::filter(Value > MDL)
  
  return(fieldBlanksList)
}

#' List all samples where total dissolved nitrogen (TDN) values exceeded total nitrogen (UTN) values, and flag whether the discrepancy was within precision limits or outside of the expected error.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
chemTDNCheck <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  
  TDN <- chem |>
    dplyr::filter(Characteristic %in% c("UTN", "TDN")) |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value, WithinPrecision, PrecisionNotes)
  
  TDNWide <- tidyr::pivot_wider(data = TDN, names_from = Characteristic, values_from = c("Value", "WithinPrecision", "PrecisionNotes")) |>
    dplyr::rename(TDN = Value_TDN,
                  UTN = Value_UTN,
                  WithinPrecision = WithinPrecision_TDN,
                  PrecisionNotes = PrecisionNotes_TDN) |>
    dplyr::select(-c("WithinPrecision_UTN", "PrecisionNotes_UTN"))
  
  TDNList <- TDNWide |>
    dplyr::mutate(Difference = ifelse(TDN > UTN, round(TDN - UTN, 2), NA)) |>
    #dplyr::mutate(TDNFlag = ifelse(TDNvUTN > 0.01, "TDN is greater than UTN outside the normal limits of variability", "TDN is greater than UTN within precision limits")) |>
    dplyr::filter(!is.na(Difference) | !is.na(WithinPrecision)) |>
    dplyr::relocate(Difference, .after = UTN)
  
  return(TDNList)
}

#' List all samples where nitrate and nitrite (NO3-N and NO2-N) values exceeded either total dissolved nitrogen (TDN) values or total nitrogen (UTN) values, and flag whether the discrepancy was within precision limits or outside of the expected error.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
chemNO3NO2Check <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  
  NO3NO2 <- chem |>
    dplyr::filter(Characteristic %in% c("UTN", "TDN", "NO3-N+NO2-N")) |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value, WithinPrecision, PrecisionNotes) |>
    dplyr::mutate(Characteristic = dplyr::case_when(Characteristic == "NO3-N+NO2-N" ~ "NO3NO2",
                                                    TRUE ~ Characteristic))
  
  NO3NO2Wide <- tidyr::pivot_wider(data = NO3NO2, names_from = Characteristic, values_from = c("Value", "WithinPrecision", "PrecisionNotes")) |>
    dplyr::rename(TDN = Value_TDN,
                  UTN = Value_UTN,
                  NO3NO2 = Value_NO3NO2,
                  WithinPrecision = WithinPrecision_NO3NO2,
                  PrecisionNotes = PrecisionNotes_NO3NO2) |>
    dplyr::select(-c("WithinPrecision_UTN", "WithinPrecision_TDN", "PrecisionNotes_UTN", "PrecisionNotes_TDN"))
  
  NO3NO2List <- NO3NO2Wide |>
    dplyr::mutate(Difference_TDN = ifelse(NO3NO2 > TDN, round(NO3NO2 - TDN, 3), NA)) |>
    dplyr::mutate(Difference_UTN = ifelse(NO3NO2 > UTN, round(NO3NO2 - UTN, 3), NA)) |>
    #dplyr::mutate(TDNFlag = ifelse(TDNvUTN > 0.01, "TDN is greater than UTN outside the normal limits of variability", "TDN is greater than UTN within precision limits")) |>
    dplyr::filter(!is.na(Difference_TDN) | !is.na(Difference_UTN) | !is.na(WithinPrecision)) |>
    dplyr::relocate(Difference_TDN, .after = UTN) |>
    dplyr::relocate(Difference_UTN, .after = Difference_TDN)
      
  return(NO3NO2List)
}

#' List all samples where total dissolved phosphorous (TDP) values exceeded total phosphorus (UTP) values, and flag whether the discrepancy was within precision limits or outside of the expected error.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
chemTDPCheck <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  
  TDP <- chem |>
    dplyr::filter(Characteristic %in% c("UTP", "TDP")) |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value, WithinPrecision, PrecisionNotes)
  
  TDPWide <- tidyr::pivot_wider(data = TDP, names_from = Characteristic, values_from = c("Value", "WithinPrecision", "PrecisionNotes")) |>
    dplyr::rename(TDP = Value_TDP,
                  UTP = Value_UTP,
                  WithinPrecision = WithinPrecision_TDP,
                  PrecisionNotes = PrecisionNotes_TDP) |>
    dplyr::select(-c("WithinPrecision_UTP", "PrecisionNotes_UTP"))
  
  TDPList <- TDPWide |>
    dplyr::mutate(Difference = ifelse(TDP > UTP, round(TDP - UTP, 3), NA)) |>
    #dplyr::mutate(TDNFlag = ifelse(TDNvUTN > 0.01, "TDP is greater than UTP outside the normal limits of variability", "TDP is greater than UTP within precision limits")) |>
    dplyr::filter(!is.na(Difference) | !is.na(WithinPrecision)) |>
    dplyr::relocate(Difference, .after = UTP)
  
  return(TDPList)
}

#' OBSOLETE FUNCTION. ANC CALCULATION NOW DONE IN AGOL PRIOR TO EXPORT: Calculate acid neutralizing capacity (ANC) from alkalinity
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
chemANC <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")

  ANC <- chem  |>
    dplyr::filter(Characteristic == "Alkalinity") |>
    dplyr::rename(Characteristic_OLD = Characteristic,
                  Unit_OLD = Unit,
                  Value_OLD = Value) |>
    dplyr::mutate(Characteristic = "ANC",
                  Unit = "ueq/L",
                  Value = Value_OLD*19.983) |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, DateProcessed, SampleType, AnalysisType, Characteristic, Unit, Value, BelowDL, WithinPrecision, PrecisionNotes, Flag)
  
  joinedANC <- rbind(chem, ANC)
  
  return(joinedANC)
}

#' Calculate ratio of total nitrogen (TN) and dissolved inorganic nitrogen (DIN) to total phosphorus (TP)
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
chemRatios <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  
  ratios <- chem |>
    dplyr::filter(Characteristic %in% c("UTN", "TDN", "NO3-N+NO2-N", "NO3-N", "UTP", "TDP", "DOC"),
                  SampleType == "Routine",
                  AnalysisType == "Routine") |>
    dplyr::mutate(Characteristic = dplyr::case_when(Characteristic == "NO3-N+NO2-N" ~ "DIN",
                                                    Characteristic == "NO3-N" ~ "DIN",
                                                    TRUE ~ Characteristic)) |>
    dplyr::select(-c("SampleType", "AnalysisType", "DateProcessed", "Unit", "BelowDL", "WithinPrecision", "PrecisionNotes", "Flag")) |>
    tidyr::pivot_wider(names_from = Characteristic, values_from = Value) |>
    dplyr::mutate(`DIN:TN` = DIN/UTN,
                  `DIN:TP` = DIN/UTP,
                  `TN:TP` = UTN/UTP)
    
}

#' Format missing years with NAs for ease of plotting
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns Tibble
#' @export
#'
#' @examples
chemFormatted <- function(park, site, field.season) {
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "ChemResults")
  
  chemYear <- chem |>
    dplyr::mutate(Year = as.integer(FieldSeason))
  
  min.year <- min(chemYear$Year)
  max.year <- max(chemYear$Year)
  
  chemFormatted <- chemYear |>
    dplyr::filter(SampleType == "Routine",
                  AnalysisType == "Routine") |>
    dplyr::select(-c("DateProcessed", "BelowDL", "WithinPrecision", "PrecisionNotes", "Flag")) |>
    dplyr::select(Park, SiteCode, SubsiteCode_Chem, VisitDate, FieldSeason, SampleType, AnalysisType, Characteristic, Unit, Value) |>
    dplyr::arrange(SubsiteCode_Chem, FieldSeason, Characteristic)

  return(chemFormatted)  
}

#' Plot acid neutralizing capacity (ANC) and include EPA thresholds
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
chemANCPlot <- function(park, site, field.season) {
  chemFormatted <- chemFormatted(park = park, site = site, field.season = field.season)
  
  chemANC <- chemFormatted |>
    dplyr::filter(Characteristic == "ANC")
  
  thresholds <- data.frame(yintercept = c(20, 50, 100, 200), Lines = c("Acute", "Severe", "Elevated", "Moderately Acidic"))
  
  plotANC <- ggplot2::ggplot(chemANC,
                             ggplot2::aes(x = SiteCode,
                                          y = Value,
                                          color = Park,
                                          fill = Park)) +
    ggplot2::geom_boxplot(alpha = 0.2) +
    ggplot2::geom_point() +
    #ggplot2::geom_line(linewidth = 1) +
    #ggplot2::facet_grid(~Park, scales = "free_x") +
    ggplot2::ylab(label = "Acid Neutralizing Capacity (ueq/L)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggplot2::labs(title = "Acid neutralizing capacity (ANC)") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::geom_hline(yintercept = c(20, 50, 100, 200), linetype = "dashed", color = "gray 20") +
    ggplot2::annotate("text", x = 1, y = 200, label = "Moderate", vjust = 1) +
    ggplot2::annotate("text", x = 1, y = 100, label = "Elevated", vjust = 1) +
    ggplot2::annotate("text", x = 1, y = 50, label = "Severe", vjust = 1) +
    ggplot2::annotate("text", x = 1, y = 20, label = "Acute", vjust = 1) +
    #ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
    khroma::scale_color_muted() +
    khroma::scale_fill_muted()
  
  return(plotANC)
}

#' Plot common ion concentrations (Na, Mg, K, Ca, SO4, Cl)
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
chemIonsPlot <- function(park, site, field.season) {
  chemFormatted <- chemFormatted(park = park, site = site, field.season = field.season)
  
  ion <- chemFormatted |>
    dplyr::filter(Characteristic %in% c("Na", "K", "Ca", "Mg", "SO4-S", "Cl")) |>
    dplyr::mutate(Characteristic = dplyr::case_when(Characteristic == "SO4-S" ~ "SO4",
                                                    TRUE ~ Characteristic)) |>
    dplyr::mutate(Ion = ifelse(Characteristic %in% c("Na", "K", "Ca", "Mg"), "Cation",
                               ifelse(Characteristic %in% c("SO4", "Cl"), "Anion", NA)))
  
  ion$Ion_f = factor(ion$Ion, levels = c("Cation", "Anion"))
  ion$Characteristic_f = factor(ion$Characteristic, levels = c("Na", "K", "Ca", "Mg", "Cl", "SO4"))
  
  ionPlot <- ggplot2::ggplot(ion,
                             ggplot2::aes(x = as.integer(FieldSeason),
                                          y = Value,
                                          color = Characteristic_f,
                                          group = Characteristic_f,
                                          text = paste0("Site Code: ", SiteCode, "<br>",
                                                        "Year: ", FieldSeason, "<br>",
                                                        "Parameter: ", Characteristic_f, "<br>",
                                                        "Value: ", Value))) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 3,
                        shape = 18) +
    ggplot2::facet_grid(#rows = ggplot2::vars(Characteristic_f),
                        cols = ggplot2::vars(SiteCode),
                        scales = "free_y",
                        space = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggplot2::labs(title = "Common ion concentrations",
                  x = "Year",
                  y = "Concentration (mg/L)",
                  color = "Ion") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    khroma::scale_color_muted() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  
  return(ionPlot)
}

#' Plot common ion concentrations (Na, Mg, K, Ca, SO4, Cl) on separate plots
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
chemIonsSplitPlot <- function(park, site, field.season) {
  chemFormatted <- chemFormatted(park = park, site = site, field.season = field.season)
  
  ion <- chemFormatted |>
    dplyr::filter(Characteristic %in% c("Na", "K", "Ca", "Mg", "SO4-S", "Cl")) |>
    dplyr::mutate(Characteristic = dplyr::case_when(Characteristic == "SO4-S" ~ "SO4",
                                                    TRUE ~ Characteristic)) |>
    dplyr::mutate(Ion = ifelse(Characteristic %in% c("Na", "K", "Ca", "Mg"), "Cation",
                               ifelse(Characteristic %in% c("SO4", "Cl"), "Anion", NA)))
  
  ion$Ion_f = factor(ion$Ion, levels = c("Cation", "Anion"))
  ion$Characteristic_f = factor(ion$Characteristic, levels = c("Na", "K", "Ca", "Mg", "Cl", "SO4"))
  
  ionPlot <- ggplot2::ggplot(ion,
                             ggplot2::aes(x = as.integer(FieldSeason),
                                          y = Value,
                                          color = Characteristic_f,
                                          group = Characteristic_f,
                                          text = paste0("Site Code: ", SiteCode, "<br>",
                                                        "Year: ", FieldSeason, "<br>",
                                                        "Parameter: ", Characteristic_f, "<br>",
                                                        "Value: ", Value))) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 3,
                        shape = 18) +
    ggplot2::facet_grid(rows = ggplot2::vars(Characteristic_f),
      cols = ggplot2::vars(SiteCode),
      scales = "free_y",
      space = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggplot2::labs(title = "Common ion concentrations",
                  x = "Year",
                  y = "Concentration (mg/L)",
                  color = "Ion") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    khroma::scale_color_muted() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  
  return(ionPlot)
}

#' Plot nutrient concentrations (UTN, TDN, NO3, UTP, TDP, DOC) as line plot
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
chemNutrientsLinePlot <- function(park, site, field.season) {
  chemFormatted <- chemFormatted(park = park, site = site, field.season = field.season)
  
  nutrient <- chemFormatted |>
    dplyr::filter(Characteristic %in% c("UTN", "TDN", "NO3-N+NO2-N", "NO3-N", "UTP", "TDP", "DOC")) |>
    dplyr::mutate(Characteristic = dplyr::case_when(Characteristic == "NO3-N+NO2-N" ~ "NO3",
                                                    Characteristic == "NO3-N" ~ "NO3",
                                                    TRUE ~ Characteristic)) |>
    dplyr::mutate(Nutrient = ifelse(Characteristic %in% c("UTN", "TDN", "NO3"), "Nitrogen",
                                    ifelse(Characteristic %in% c("UTP", "TDP"), "Phosphorus",
                                           ifelse(Characteristic %in% c("DOC"), "Carbon", NA))))
  
  nutrient$Nutrient_f = factor(nutrient$Nutrient, levels = c("Nitrogen", "Phosphorus", "Carbon"))
  nutrient$Characteristic_f = factor(nutrient$Characteristic, levels = c("UTN", "TDN", "NO3", "UTP", "TDP", "DOC"))
  
  nutrientLinePlot <- ggplot2::ggplot(nutrient,
                                  ggplot2::aes(x = as.integer(FieldSeason),
                                               y = Value,
                                               color = Characteristic_f,
                                               group = Characteristic_f,
                                               text = paste0("Site Code: ", SiteCode, "<br>",
                                                             "Year: ", FieldSeason, "<br>",
                                                             "Parameter: ", Characteristic_f, "<br>",
                                                             "Value: ", Value))) +
     ggplot2::geom_line(linewidth = 1) +
     ggplot2::geom_point(size = 3,
                         shape = 18) +
     ggplot2::facet_grid(rows = ggplot2::vars(Nutrient_f),
                        cols = ggplot2::vars(SiteCode),
                        scales = "free_y",
                        space = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggplot2::labs(title = "Nutrient concentrations",
                  x = "Year",
                  y = "Concentration (mg/L)",
                  color = "Nutrient") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_color_manual(values = c("midnightblue", "royalblue1", "lightskyblue", "firebrick4", "lightpink2", "goldenrod")) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  
  return(nutrientLinePlot)
}

#' Plot nutrient concentrations (UTN, TDN, NO3, UTP, TDP, DOC) as bar plot
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "PARA".
#' @param site Optional. Site code to filter on, e.g. "PARA_P_TASS0".
#' @param field.season Optional. Field season name to filter on, e.g. "2022".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
chemNutrientsBarPlot <- function(park, site, field.season) {
  chemFormatted <- chemFormatted(park = park, site = site, field.season = field.season)
  
  nutrient <- chemFormatted |>
    dplyr::filter(Characteristic %in% c("UTN", "TDN", "NO3-N+NO2-N", "NO3-N", "UTP", "TDP", "DOC")) |>
    dplyr::mutate(Characteristic = dplyr::case_when(Characteristic == "NO3-N+NO2-N" ~ "NO3",
                                                    Characteristic == "NO3-N" ~ "NO3",
                                                    TRUE ~ Characteristic)) |>
    dplyr::mutate(Nutrient = ifelse(Characteristic %in% c("UTN", "TDN", "NO3"), "Nitrogen",
                                    ifelse(Characteristic %in% c("UTP", "TDP"), "Phosphorus",
                                           ifelse(Characteristic %in% c("DOC"), "Carbon", NA))))
  
  nutrient$Nutrient_f = factor(nutrient$Nutrient, levels = c("Nitrogen", "Phosphorus", "Carbon"))
  nutrient$Characteristic_f = factor(nutrient$Characteristic, levels = c("UTN", "TDN", "NO3", "UTP", "TDP", "DOC"))
 
  nutrient <- nutrient |>
    dplyr::arrange(match(Characteristic_f, c("UTN", "TDN", "NO3", "UTP", "TDP", "DOC"), dplyr::desc(Characteristic_f)))
   
  nutrientBarPlot <- ggplot2::ggplot(nutrient,
                                  ggplot2::aes(x = as.integer(FieldSeason),
                                               y = Value,
                                               fill = Characteristic_f,
                                               text = paste0("Site Code: ", SiteCode, "<br>",
                                                             "Year: ", FieldSeason, "<br>",
                                                             "Parameter: ", Characteristic_f, "<br>",
                                                             "Value: ", Value))) +
    ggplot2::geom_bar(stat = "identity",
                      position = "identity") +
    ggplot2::facet_grid(rows = ggplot2::vars(Nutrient_f),
                        cols = ggplot2::vars(SiteCode),
                        scales = "free_y",
                        space = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggplot2::labs(title = "Nutrient concentrations",
                  x = "Year",
                  y = "Concentration (mg/L)",
                  fill = "Nutrient") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_fill_manual(values = c("midnightblue", "royalblue1", "lightskyblue", "firebrick4", "lightpink2", "goldenrod")) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  
  return(nutrientBarPlot)
}