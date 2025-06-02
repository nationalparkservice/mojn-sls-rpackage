#' @importFrom magrittr %>% %<>%

pkg_globals <- new.env(parent = emptyenv())

#' Intermediate function between reading package and returning data
#'
#' @param data.name Name of tibble to read from environment (e.g., "Pool")
#'
#' @returns Tibble
#'
get_data <- function(data.name) {
  
  tibble_names <- c("Sites",
                    "Subsites",
                    "VisitQuarterly",
                    "PhotoQuarterly",
                    "WaterQuality",
                    "Outlet",
                    "Pool",
                    "Gage",
                    "FillTime",
                    "VolumetricDischarge",
                    "VisitBiennial",
                    "BMISample",
                    "BMIQuadrat",
                    "ChemSample",
                    "VisitWell",
                    "PhotoWell",
                    "WellDepth",
                    "WellSecondary",
                    "BMISpecies",
                    "BMIMetrics",
                    "BMIVisit",
                    "ChemResults",
                    "CalibrationSpCond",
                    "CalibrationpH",
                    "CalibrationDO",
                    "TimeseriesDischarge",
                    "TimeseriesStage",
                    "TimeseriesDepth",
                    "TimeseriesFieldVisit",
                    "TimeseriesVolumetric")
  
  if (!missing(data.name)) {
    if (!(data.name %in% tibble_names)) {
      stop("Invalid data table name.")
    }
    tryCatch({data <- get(data.name, pkg_globals)},
             error = function(e) {
               if (grepl(".*object.* not found.*", e$message, ignore.case = TRUE)) {
                 stop(paste0("Could not find data. Did you remember to call LoadSelectedLargeSprings?\n\tOriginal error: ", e$message))
               }
               else {e}
             })
  }
  return(data)
}

#' Read data from AGOL
#'
#' @param agol_username 
#' @param agol_password 
#'
#' @returns List of tibbles
#'
ReadAGOL <- function(agol_username = "mojn_data", agol_password = keyring::key_get(service = "AGOL", username = agol_username)) {
  
  sites_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_SLS_Sites/FeatureServer"
  subsites_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_SLS_Subsites/FeatureServer"
  quarterly_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/service_991cb0ba53db499a9958e7e12d5c3f5f/FeatureServer"
  biennial_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/service_3b813173a71c4f41b5a3a02064bf361c/FeatureServer"
  well_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/service_2e96791d903b4698925bd961b3e65b8f/FeatureServer"
  bmi_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_HYDRO_BMI_Database/FeatureServer"
  chem_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_SLS_Chemistry_Database/FeatureServer"
  calibration_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Calibration_Database/FeatureServer"
  lookup_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Lookup_Database/FeatureServer"
  
  agol_layers <- list()
  
  #Import sites feature service
  sites <- fetchagol::fetchRawData(sites_url, agol_username, agol_password)

  names(sites$data) <- c("Sites")
  
  # Import subsites feature service
  subsites <- fetchagol::fetchRawData(subsites_url, agol_username, agol_password)

  names(subsites$data) <- c("Subsites")
  
  # Import quarterly visit feature service
  quarterly <- fetchagol::fetchRawData(quarterly_url, agol_username, agol_password)
  quarterly <- fetchagol::cleanData(quarterly)
  
  quarterly$data[['FieldCrew']] <- NULL
  names(quarterly$data) <- c("VisitQuarterly", "PhotoQuarterly", "WaterQuality", "Outlet", "Pool", "Gage", "FillTime")
  
  # Import biennial visit feature service
  biennial <- fetchagol::fetchRawData(biennial_url, agol_username, agol_password)
  biennial <- fetchagol::cleanData(biennial)
  
  biennial$data[['FieldCrew']] <- NULL
  names(biennial$data) <- c("VisitBiennial", "BMISample", "BMIQuadrat", "ChemSample")
  
  # Import well depth feature service
  well <- fetchagol::fetchRawData(well_url, agol_username, agol_password)
  well <- fetchagol::cleanData(well)
  
  well$data[['FieldCrew']] <- NULL
  names(well$data) <- c("VisitWell", "PhotoWell", "WellDepth", "WellSecondary")
  
  # Import bmi feature service
  bmi <- fetchagol::fetchRawData(bmi_url, agol_username, agol_password)
  bmi <- fetchagol::cleanData(bmi)
  
  bmi$data[['BMI_Metadata']] <- NULL
  names(bmi$data) <- c("BMISpecies", "BMIMetrics", "BMIVisit")
  
  # Import chem feature service
  chem <- fetchagol::fetchRawData(chem_url, agol_username, agol_password)
  chem <- fetchagol::cleanData(chem)

  chem$data[['DummySpatialLayer']] <- NULL
  names(chem$data) <- c("ChemResults")
  
  # Import lookup feature service - DEPRECATED
  # lookup <- fetchagol::fetchRawData(lookup_url, agol_username, agol_password)
  # lookup <- fetchagol::cleanData(lookup)
  
  # Tidy bmi import data
  
  bmi$data <- lapply(bmi$data, function(df) {
    df |>
      dplyr::filter(Project %in% c("SLS", "STLK")) |>
      dplyr::mutate(CollectionDate = as.Date(CollectionDateText, tz = "America/Los_Angeles"))
  })
  
  # Import water quality instrument calibration feature service
  calibration <- fetchagol::fetchRawData(calibration_url, agol_username, agol_password)
  calibration <- fetchagol::cleanData(calibration)
  
  agol_layers <- c(sites$data, subsites$data, quarterly$data, biennial$data, well$data, bmi$data, chem$data, calibration$data)
  
  return(agol_layers)
}

#' Wrangle data from AGOL
#'
#' @returns List of tibbles
#'
WrangleAGOL <- function(...) {
  agol_layers <- ReadAGOL(...)

  # Get join information from visit tables
  
  visit_q_info <- agol_layers$VisitQuarterly |>
    dplyr::mutate(StartTime = as.POSIXct(StartTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) |>
    dplyr::rename(DateTime = StartTime,
                  Park = ParkCode,
                  SiteCode = SpringCode) |>
    dplyr::select(Park, SiteCode, DateTime, globalid)
  
  visit_b_info <- agol_layers$VisitBiennial |>
    dplyr::mutate(StartTime = as.POSIXct(StartTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) |>
    dplyr::rename(DateTime = StartTime,
                  Park = ParkCode,
                  SiteCode = SpringCode) |>
    dplyr::select(Park, SiteCode, DateTime, globalid)
  
  visit_b_info_chem <- agol_layers$VisitBiennial |>
    dplyr::mutate(StartTime = as.POSIXct(StartTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) |>
    dplyr::rename(DateTime = StartTime,
                  Park = ParkCode,
                  SiteCode = SpringCode) |>
    dplyr::select(Park, SiteCode, ChemLocation, DateTime, ChemMethod, ChemNotes, globalid)
  
  visit_b_info_chem_intermediate <- agol_layers$ChemSample |>
    dplyr::select(globalid, parentglobalid, SampleType)
  
  visit_w_info <- agol_layers$VisitWell |>
    dplyr::mutate(StartTime = as.POSIXct(StartTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) |>
    dplyr::rename(DateTime = StartTime,
                  Park = ParkCode,
                  SiteCode = WellCode,
                  SubsiteCode = SubWellCode) |>
    dplyr::select(Park, SiteCode, DateTime, globalid)
  
  vol_discharge <- agol_layers$VisitQuarterly |>
    dplyr::mutate(StartTime = as.POSIXct(StartTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) |>
    dplyr::rename(DateTime = StartTime,
                  Park = ParkCode,
                  SiteCode = SpringCode) |>
    dplyr::select(Park, SiteCode, DateTime, ContainerVolume_L, PercentFlowCaptured)
    
  filltime_median <- agol_layers$FillTime |>
    dplyr::left_join(visit_q_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, FillTime_sec) |>
    dplyr::mutate_if(is.numeric, dplyr::na_if, -9999) |>  # Replace -9999 or -999 with NA
    dplyr::mutate_if(is.numeric, dplyr::na_if, -999) |>
    dplyr::group_by(Park, SiteCode, DateTime) |>
    dplyr::mutate(FillTime_sec = median(FillTime_sec, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    unique() |>
    dplyr::arrange(SiteCode, DateTime) |>
    dplyr::mutate(FillTime_sec = round(FillTime_sec, 2))
  
  welldepth_median <- agol_layers$WellDepth |>
    dplyr::left_join(visit_w_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, WLBelowLSD_ft) |>
    dplyr::mutate_if(is.numeric, dplyr::na_if, -9999) |>  # Replace -9999 or -999 with NA
    dplyr::mutate_if(is.numeric, dplyr::na_if, -999) |>
    dplyr::group_by(Park, SiteCode, DateTime) |>
    dplyr::mutate(WLBelowLSD_ft = median(WLBelowLSD_ft, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    unique() |>
    dplyr::arrange(SiteCode, DateTime) |>
    dplyr::mutate(WLBelowLSD_ft = round(WLBelowLSD_ft, 3))
  
  wellsecondary_median <- agol_layers$WellSecondary |>
    dplyr::left_join(visit_w_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, WLBelowLSD_Secondary_ft) |>
    dplyr::mutate_if(is.numeric, dplyr::na_if, -9999) |>  # Replace -9999 or -999 with NA
    dplyr::mutate_if(is.numeric, dplyr::na_if, -999) |>
    dplyr::group_by(Park, SiteCode, DateTime) |>
    dplyr::mutate(WLBelowLSD_Secondary_ft = median(WLBelowLSD_Secondary_ft)) |>
    dplyr::ungroup() |>
    unique() |>
    dplyr::arrange(SiteCode, DateTime) |>
    dplyr::mutate(WLBelowLSD_Secondary_ft = round(WLBelowLSD_Secondary_ft, 3))
  
  # ----- Sites -----
  
  agol_layers$Sites <- agol_layers$Sites |>
    dplyr::select("Park",
                  "SiteCode",
                  "SiteCodeShort",
                  "SiteName",
                  "NHD_Permanent_ID",
                  "NHD_Permanent_ID_Type",
                  "Status",
                  "SiteProtectionStatus",
                  "Latitude",
                  "Longitude",
                  "Elevation_m") |>
    dplyr::mutate(NHD_Permanent_ID = as.double(NHD_Permanent_ID),
                  Elevation_m = as.double(Elevation_m))
  
  # ----- Subsites -----
  
  agol_layers$Subsites <- agol_layers$Subsites |>
    dplyr::select("Park",
                  "SiteCode",
                  "SiteCodeShort",
                  "SubsiteCode",
                  "SubsiteCodeShort",
                  "SiteName",
                  "SubsiteName",
                  "NHD_Permanent_ID",
                  "NHD_Permanent_ID_Type",
                  "Status",
                  "SiteProtectionStatus",
                  "Latitude",
                  "Longitude",
                  "Elevation_m") |>
    dplyr::mutate(NHD_Permanent_ID = as.double(NHD_Permanent_ID),
                  Elevation_m = as.double(Elevation_m))
  
  # ----- VisitQuarterly -----
  
  agol_layers$VisitQuarterly <- agol_layers$VisitQuarterly |>
    dplyr::mutate(StartTime = as.POSIXct(StartTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) |>
    dplyr::rename(DateTime = StartTime,
                  Park = ParkCode,
                  SiteCode = SpringCode,
                  GageNotes = FlumeWeirNotes,
                  WaterQualityNotes = WQNotes) |>
    dplyr::select(Park, SiteCode, DateTime, FlowCondition, GageNotes, WaterQualityNotes, OverallNotes) |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- PhotoQuarterly -----
  
  agol_layers$PhotoQuarterly <- agol_layers$PhotoQuarterly |>
    dplyr::left_join(visit_q_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, PhotoType, IsLibrary, PhotoNotes) |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- WaterQuality -----
  
  agol_layers$WaterQuality <- agol_layers$WaterQuality |>
    dplyr::left_join(visit_q_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::rename(SubsiteCode = WQSubsite,
                  MeasurementDepth_ft = MeasurementDepth,
                  SpCond_Flag = SpCondmicroS_Flag,
                  Temperature_Flag = Temp_C_Flag) |>
    dplyr::filter(ifelse(parentglobalid %in% "22269cb4-c908-4c82-a5a8-6758ac76d525", globalid == "ffc5a62c-b53e-40c7-8fe3-1dc95d27a95a", TRUE)) |>
    dplyr::filter(!is.na(SubsiteCode)) |>
    dplyr::mutate_if(is.numeric, dplyr::na_if, -9999) |>  # Replace -9999 or -999 with NA
    dplyr::mutate_if(is.numeric, dplyr::na_if, -999) |>
    dplyr::rowwise() |>
    dplyr::mutate(Temperature_C = median(c(Temp_C_1, Temp_C_2, Temp_C_3), na.rm = TRUE),
                  pH = median(c(pH_1, pH_2, pH_3), na.rm = TRUE),
                  SpCond_uS_per_cm = median(c(SpCond_microS_1, SpCond_microS_2, SpCond_microS_3), na.rm = TRUE),
                  DO_mg_per_L = median(c(DO_mg_per_L_1, DO_mg_per_L_2, DO_mg_per_L_3), na.rm = TRUE),
                  DO_percent = median(c(DO_percent_1, DO_percent_2, DO_percent_3), na.rm = TRUE)) |>
    dplyr::mutate(Temperature_Flag = dplyr::case_when(Temperature_Flag == 1 ~ "No Flag",
                                                      Temperature_Flag == 2 ~ "Information",
                                                      Temperature_Flag == 3 ~ "Warning",
                                                      Temperature_Flag == 4 ~ "Critical",
                                                      TRUE ~ NA_character_),
                  pH_Flag = dplyr::case_when(pH_Flag == 1 ~ "No Flag",
                                             pH_Flag == 2 ~ "Information",
                                             pH_Flag == 3 ~ "Warning",
                                             pH_Flag == 4 ~ "Critical",
                                             TRUE ~ NA_character_),
                  SpCond_Flag = dplyr::case_when(SpCond_Flag == 1 ~ "No Flag",
                                                 SpCond_Flag == 2 ~ "Information",
                                                 SpCond_Flag == 3 ~ "Warning",
                                                 SpCond_Flag == 4 ~ "Critical",
                                                 TRUE ~ NA_character_),
                  DO_Flag = dplyr::case_when(DO_Flag == 1 ~ "No Flag",
                                             DO_Flag == 2 ~ "Information",
                                             DO_Flag == 3 ~ "Warning",
                                             DO_Flag == 4 ~ "Critical",
                                             TRUE ~ NA_character_)) |>
    dplyr::select(Park, SiteCode, SubsiteCode, DateTime, IsDepthProfile, MeasurementDepth_ft, DepthToBottom_ft, Temperature_C, Temperature_Flag, pH, pH_Flag, SpCond_uS_per_cm, SpCond_Flag, DO_mg_per_L, DO_percent, DO_Flag) |>
    dplyr::arrange(SiteCode, DateTime) |>
    dplyr::mutate(Temperature_C = round(Temperature_C, 2),
                  pH = round(pH, 2),
                  SpCond_uS_per_cm = dplyr::case_when(SpCond_uS_per_cm > 600 ~ round(SpCond_uS_per_cm, 0),
                                                      TRUE ~ round(SpCond_uS_per_cm, 1)),
                  DO_mg_per_L = round(DO_mg_per_L, 2),
                  DO_percent = round(DO_percent, 1))
  
  # ----- Outlet -----
  
  agol_layers$Outlet <- agol_layers$Outlet |>
    dplyr::left_join(visit_q_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, OutletNumber, OutletWaterPresent, FlowsOverCliff, SpringbrookLength_m, OutletNotes) |>
    dplyr::filter(SiteCode %in% "JOTR_P_SMIT0") |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- Pool -----
  
  agol_layers$Pool <- agol_layers$Pool |>
    dplyr::left_join(visit_q_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, PoolNumber, PoolArea_m_sq, FrogCount, PoolNotes) |>
    dplyr::filter(SiteCode %in% "JOTR_P_FORT0") |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- Gage -----
  
  agol_layers$Gage <- agol_layers$Gage |>
    dplyr::left_join(visit_q_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, GageTime, GageHeight_ft) |>
    dplyr::mutate_if(is.numeric, dplyr::na_if, -9999) |>  # Replace -9999 or -999 with NA
    dplyr::mutate_if(is.numeric, dplyr::na_if, -999) |>
    dplyr::filter(!is.na(GageHeight_ft)) |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- FillTime -----
  
  agol_layers$FillTime <- agol_layers$FillTime |>
    dplyr::left_join(visit_q_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, FillTime_sec) |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- VolumetricDischarge -----
  
  agol_layers$VolumetricDischarge <- vol_discharge |>
    dplyr::left_join(filltime_median, by = c("Park", "SiteCode", "DateTime")) |>
    dplyr::mutate(Discharge_L_per_sec = (ContainerVolume_L/FillTime_sec)*(100/PercentFlowCaptured),
                  Discharge_cfs = (ContainerVolume_L/FillTime_sec)*(100/PercentFlowCaptured)*0.035315) |>
    dplyr::select(Park, SiteCode, DateTime, ContainerVolume_L, PercentFlowCaptured, FillTime_sec, Discharge_L_per_sec, Discharge_cfs) |>
    dplyr::arrange(SiteCode, DateTime) |>
    dplyr::filter(!(is.na(FillTime_sec))) |>
    dplyr::mutate(Discharge_L_per_sec = round(Discharge_L_per_sec, 3),
                  Discharge_cfs = round(Discharge_cfs, 3))
  
  # ----- VisitBiennial -----
  
  agol_layers$VisitBiennial <- agol_layers$VisitBiennial |>
    dplyr::mutate(StartTime = as.POSIXct(StartTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) |>
    dplyr::rename(DateTime = StartTime,
                  Park = ParkCode,
                  SiteCode = SpringCode,
                  SubsiteCode_Chem = ChemLocation) |>
    dplyr::select(Park, SiteCode, DateTime, BMINotes) |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- BMISample -----
  
  bmi_b_info <- agol_layers$BMISample |> # Leave this code here. Must be run before agol_layers$BMISample gets modified.
    dplyr::select(SpringSubsite, globalid, parentglobalid) |>
    dplyr::rename(SubsiteCode_BMI = SpringSubsite)
    
  agol_layers$BMISample <- agol_layers$BMISample |>
    dplyr::left_join(visit_b_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::rename(SubsiteCode_BMI = SpringSubsite) |>
    dplyr::mutate(QuadratSize_cm = dplyr::case_when(QuadratSize == 1010 ~ 10,
                                                    QuadratSize == 3030 ~ 30,
                                                    TRUE ~ NA_integer_)) |>
    dplyr::filter(BMICollected == "Y") |>
    dplyr::select(Park, SiteCode, SubsiteCode_BMI, DateTime, BMIMethod, QuadratSize_cm) |>
    dplyr::arrange(SiteCode, DateTime, SubsiteCode_BMI)
  
  # ----- BMIQuadrat -----
  
  agol_layers$BMIQuadrat <- agol_layers$BMIQuadrat |>
    dplyr::left_join(bmi_b_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::left_join(visit_b_info, by = c("parentglobalid.y" = "globalid")) |>
    dplyr::rename(TransectDistance_m = TransectDistance,
                  ChannelSide = SideOfSpringbrook,
                  ChannelSubstrate = Substrate,
                  ChannelFlow = Channel,
                  PoolQuadrats = NumQuadrats,
                  QuadratNum = QuadrateNum) |>
    dplyr::select(Park, SiteCode, SubsiteCode_BMI, DateTime, TransectDistance_m, ChannelSide, ChannelSubstrate, ChannelFlow, QuadratNum, PoolNum, PoolQuadrats, QuadratNote) |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- ChemSample -----
  
  agol_layers$ChemSample <- agol_layers$ChemSample |>
    dplyr::left_join(visit_b_info_chem, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, ChemLocation, DateTime, SampleType, ChemMethod, bottleCount_Unfiltered, bottleCount_Filtered, laboratory, lab_number, ChemNotes) |>
    dplyr::rename(UnfilteredBottles = bottleCount_Unfiltered,
                  FilteredBottles = bottleCount_Filtered,
                  LabName = laboratory,
                  LabSampleID = lab_number,
                  SubsiteCode_Chem = ChemLocation) |>
    dplyr::arrange(SiteCode, DateTime) |>
    dplyr::filter(!is.na(LabSampleID))
  
  # ----- VisitWell -----
  
  agol_layers$VisitWell <- agol_layers$VisitWell |>
    dplyr::mutate(StartTime = as.POSIXct(StartTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) |>
    dplyr::rename(DateTime = StartTime,
                  Park = ParkCode,
                  SiteCode = WellCode,
                  SubsiteCode = SubWellCode,
                  WellNotes = OverallNotes) |>
    dplyr::select(Park, SiteCode, DateTime, TapeType, TapeID, HoldMethod, TapeType_Secondary, TapeID_Secondary, HoldMethod_Secondary, WellNotes) |>
    dplyr::left_join(welldepth_median, by = c("Park", "SiteCode", "DateTime")) |>
    dplyr::left_join(wellsecondary_median, by = c("Park", "SiteCode", "DateTime")) |>
    dplyr::relocate(WLBelowLSD_ft, .after = HoldMethod) |>
    dplyr::relocate(WLBelowLSD_Secondary_ft, .after = HoldMethod_Secondary) |>
    dplyr::arrange(SiteCode, DateTime) |>
    dplyr::filter(!(is.na(WLBelowLSD_ft)))
  
  # ----- PhotoWell -----
  
  agol_layers$PhotoWell <- agol_layers$PhotoWell |>
    dplyr::left_join(visit_w_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, PhotoType, IsLibrary, PhotoNotes) |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- WellDepth -----
  
  agol_layers$WellDepth <- agol_layers$WellDepth |>
    dplyr::left_join(visit_w_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, DepthTime, Hold_ft, Cut_ft, TapeCorrection_ft, MPCorrection_ft, WLBelowLSD_ft) |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- WellSecondary -----
  
  agol_layers$WellSecondary <- agol_layers$WellSecondary |>
    dplyr::left_join(visit_w_info, by = c("parentglobalid" = "globalid")) |>
    dplyr::select(Park, SiteCode, DateTime, DepthTime_Secondary, Hold_Secondary_ft, Cut_Secondary_ft, TapeCorrection_Secondary_ft, MPCorrection_Secondary_ft, WLBelowLSD_Secondary_ft) |>
    dplyr::arrange(SiteCode, DateTime)
  
  # ----- BMISpecies -----
  
  agol_layers$BMISpecies <- agol_layers$BMISpecies |>
    dplyr::rename(SubsiteCode = SiteCode) |>
    dplyr::rename(SiteCode = SiteGroup,
                  Order = Order_) |>
    dplyr::mutate(SiteCode = dplyr::case_when(Project == "STLK" ~ SubsiteCode,
                                              TRUE ~ SiteCode)) |>
    dplyr::select(SampleID, Laboratory, Project, Park, SiteCode, SubsiteCode, SiteName, FieldSeason, CollectionDate,
                  Phylum, Class, Order, Family, SubFamily, Genus, Species,
                  ScientificName, OTUName, LifeStage, Notes, LabCount, BigRareCount) |>
    dplyr::arrange(SiteCode, CollectionDate) |>
    dplyr::filter(!(ScientificName %in% c("Actinopterygii", "Anura"))) |>
    dplyr::mutate(Phylum = dplyr::case_when(ScientificName == "Oligochaeta" & (is.na(Phylum) | Phylum == "NA") ~ "Annelida",
                                            ScientificName == "Nematoda" & (is.na(Phylum) | Phylum == "NA") ~ "Nematoda",
                                            ScientificName == "Lumbriculata" & (is.na(Phylum) | Phylum == "NA") ~ "Annelida",
                                            ScientificName == "Platyhelminthes" & (is.na(Phylum) | Phylum == "NA") ~ "Platyhelminthes",
                                            ScientificName == "Xenacoelomorpha" & (is.na(Phylum) | Phylum == "NA") ~ "Xenacoelomorpha",
                                            TRUE ~ Phylum)) |>
    dplyr::mutate(Class = dplyr::case_when(ScientificName == "Oligochaeta" & (is.na(Class) | Class == "NA") ~ "Clitellata",
                                           ScientificName == "Lumbriculata" & (is.na(Class) | Class == "NA") ~ "Clitellata",
                                           ScientificName == "Branchiobdellidae" & (is.na(Class) | Class == "NA") ~ "Clitellata",
                                           ScientificName == "Erpobdellidae"  & (is.na(Class) | Class == "NA") ~ "Clitellata",
                                           ScientificName == "Collembola" & (is.na(Class) | Class == "NA") ~ "Collembola",
                                           TRUE ~ Class)) |>
    dplyr::mutate(SiteName = dplyr::case_when(grepl("Baker Creek|BAKR2|BAKR3", SiteName) ~ "Baker Creek",
                                              grepl("Lehman|LHMN2", SiteName) ~ "Lehman Creek",
                                              grepl("Mill|MILL1", SiteName) ~ "Mill Creek",
                                              grepl("Pine|PINE1", SiteName) ~ "Pine Creek",
                                              grepl("Ridge|RDGE1", SiteName) ~ "Ridge Creek",
                                              grepl("Shingle|SHNG1", SiteName) ~ "Shingle Creek",
                                              grepl("South Fork|SFBW1", SiteName) ~ "South Fork Big Wash",
                                              grepl("Snake|SNKE4", SiteName) ~ "Snake Creek",
                                              grepl("Strawberry Creek|STRW2", SiteName) ~ "Strawberry Creek",
                                              grepl("Mound|MOUN0|MOUND", SiteName) ~ "Mound Spring",
                                              grepl("Nevares|NEVA0", SiteName) ~ "Nevares Spring",
                                              grepl("Saratoga|SARA0", SiteName) ~ "Saratoga Spring",
                                              grepl("Texas|TEXA0", SiteName) ~ "Texas Spring",
                                              grepl("Travertine|TRAV0", SiteName) ~ "Travertine Spring",
                                              grepl("Boiler|BOIL0", SiteName) ~ "Boiler Spring",
                                              grepl("Marmot|MARM0", SiteName) ~ "Marmot Spring",
                                              grepl("Strawberry Spring|STRW0", SiteName) ~ "Strawberry Spring",
                                              grepl("Fortynine|FORT0", SiteName) ~ "Fortynine Palms Oasis",
                                              grepl("Smithwater|SMIT0", SiteName) ~ "Smithwater Canyon Spring",
                                              grepl("Blue Point|BLUE0", SiteName) ~ "Blue Point Spring",
                                              grepl("Pakoon|PAKO0", SiteName) ~ "Pakoon Spring",
                                              grepl("Tassi|TASS0", SiteName) ~ "Tassi Spring",
                                              TRUE ~ SiteName))
  
  # ----- BMIMetrics -----
  
  agol_layers$BMIMetrics <- agol_layers$BMIMetrics |>
    dplyr::rename(SubsiteCode = SiteCode) |>
    dplyr::rename(SiteCode = SiteGroup) |>
    dplyr::mutate(SiteCode = dplyr::case_when(Project == "STLK" ~ SubsiteCode,
                                              TRUE ~ SiteCode)) |>
    dplyr::select(SampleID, Project, Park, SiteCode, SubsiteCode, SiteName, FieldSeason, CollectionDate, AnalysisType, InvasiveSpeciesList, Attribute, Value) |>
    dplyr::arrange(SiteCode, CollectionDate) |>
    dplyr::mutate(Value = dplyr::case_when(grepl("SEV|DSP", SubsiteCode) & grepl("Density", Attribute) ~ NA,
                                           TRUE ~ Value)) |>
    dplyr::mutate(SiteName = dplyr::case_when(grepl("Baker Creek|BAKR2|BAKR3", SiteName) ~ "Baker Creek",
                                              grepl("Lehman|LHMN2", SiteName) ~ "Lehman Creek",
                                              grepl("Mill|MILL1", SiteName) ~ "Mill Creek",
                                              grepl("Pine|PINE1", SiteName) ~ "Pine Creek",
                                              grepl("Ridge|RDGE1", SiteName) ~ "Ridge Creek",
                                              grepl("Shingle|SHNG1", SiteName) ~ "Shingle Creek",
                                              grepl("South Fork|SFBW1", SiteName) ~ "South Fork Big Wash",
                                              grepl("Snake|SNKE4", SiteName) ~ "Snake Creek",
                                              grepl("Strawberry Creek|STRW2", SiteName) ~ "Strawberry Creek",
                                              grepl("Mound|MOUN0|MOUND", SiteName) ~ "Mound Spring",
                                              grepl("Nevares|NEVA0", SiteName) ~ "Nevares Spring",
                                              grepl("Saratoga|SARA0", SiteName) ~ "Saratoga Spring",
                                              grepl("Texas|TEXA0", SiteName) ~ "Texas Spring",
                                              grepl("Travertine|TRAV0", SiteName) ~ "Travertine Spring",
                                              grepl("Boiler|BOIL0", SiteName) ~ "Boiler Spring",
                                              grepl("Marmot|MARM0", SiteName) ~ "Marmot Spring",
                                              grepl("Strawberry Spring|STRW0", SiteName) ~ "Strawberry Spring",
                                              grepl("Fortynine|FORT0", SiteName) ~ "Fortynine Palms Oasis",
                                              grepl("Smithwater|SMIT0", SiteName) ~ "Smithwater Canyon Spring",
                                              grepl("Blue Point|BLUE0", SiteName) ~ "Blue Point Spring",
                                              grepl("Pakoon|PAKO0", SiteName) ~ "Pakoon Spring",
                                              grepl("Tassi|TASS0", SiteName) ~ "Tassi Spring",
                                              TRUE ~ SiteName))
  
  # ----- BMIVisit -----
  
  agol_layers$BMIVisit <- agol_layers$BMIVisit |>
    dplyr::rename(SubsiteCode = SiteCode) |>
    dplyr::rename(SiteCode = SiteGroup) |>
    dplyr::mutate(SiteCode = dplyr::case_when(Project == "STLK" ~ SubsiteCode,
                                              TRUE ~ SiteCode)) |>
    dplyr::select(SampleID, Project, Laboratory, Park, SiteCode, SubsiteCode, SiteName, FieldSeason, CollectionDate,
                  NAMC_Latitude, NAMC_Longitude, Customer_Latitude, Customer_Longitude,
                  VisitType, AnalysisType, SampleType, SamplerType, Habitat, Ecosystem,
                  Area, FieldSplit, LabSplit, SplitCount, FieldNotes, LabNotes) |>
    dplyr::arrange(SiteCode, CollectionDate) |>
    dplyr::mutate(SiteName = dplyr::case_when(grepl("Baker Creek|BAKR2|BAKR3", SiteName) ~ "Baker Creek",
                                              grepl("Lehman|LHMN2", SiteName) ~ "Lehman Creek",
                                              grepl("Mill|MILL1", SiteName) ~ "Mill Creek",
                                              grepl("Pine|PINE1", SiteName) ~ "Pine Creek",
                                              grepl("Ridge|RDGE1", SiteName) ~ "Ridge Creek",
                                              grepl("Shingle|SHNG1", SiteName) ~ "Shingle Creek",
                                              grepl("South Fork|SFBW1", SiteName) ~ "South Fork Big Wash",
                                              grepl("Snake|SNKE4", SiteName) ~ "Snake Creek",
                                              grepl("Strawberry Creek|STRW2", SiteName) ~ "Strawberry Creek",
                                              grepl("Mound|MOUN0|MOUND", SiteName) ~ "Mound Spring",
                                              grepl("Nevares|NEVA0", SiteName) ~ "Nevares Spring",
                                              grepl("Saratoga|SARA0", SiteName) ~ "Saratoga Spring",
                                              grepl("Texas|TEXA0", SiteName) ~ "Texas Spring",
                                              grepl("Travertine|TRAV0", SiteName) ~ "Travertine Spring",
                                              grepl("Boiler|BOIL0", SiteName) ~ "Boiler Spring",
                                              grepl("Marmot|MARM0", SiteName) ~ "Marmot Spring",
                                              grepl("Strawberry Spring|STRW0", SiteName) ~ "Strawberry Spring",
                                              grepl("Fortynine|FORT0", SiteName) ~ "Fortynine Palms Oasis",
                                              grepl("Smithwater|SMIT0", SiteName) ~ "Smithwater Canyon Spring",
                                              grepl("Blue Point|BLUE0", SiteName) ~ "Blue Point Spring",
                                              grepl("Pakoon|PAKO0", SiteName) ~ "Pakoon Spring",
                                              grepl("Tassi|TASS0", SiteName) ~ "Tassi Spring",
                                              TRUE ~ SiteName))
  
  # ----- ChemResults -----
  
  agol_layers$ChemResults <- agol_layers$ChemResults |>
    dplyr::mutate(dateCollection = as.POSIXct(dateCollection/1000, origin = "1970-01-01", tz = "UTC"),
                  dateDelivery = as.POSIXct(dateDelivery/1000, origin = "1970-01-01", tz = "UTC"),
                  dateProcessing = as.POSIXct(dateProcessing/1000, origin = "1970-01-01", tz = "UTC")) |>
    dplyr::mutate(dateCollection = as.Date(dateCollection),
                  dateDelivery = as.Date(dateDelivery),
                  dateProcessing = as.Date(dateProcessing)) |>
    dplyr::left_join(visit_b_info_chem_intermediate, by = c("parentglobalid" = "globalid")) |>
    dplyr::left_join(visit_b_info_chem, by = c("parentglobalid.y" = "globalid")) |>
    dplyr::mutate(DateTime = as.Date(DateTime, tz = "America/Los_Angeles")) |>
    dplyr::mutate(FieldSeason = format(DateTime, "%Y")) |>
    dplyr::select(Park, SiteCode, ChemLocation, DateTime, FieldSeason, dateProcessing, SampleType, analysisType, characteristic, unit, value, belowDetectionLimit, qa_within_precision_limits, qa_description, qualifications) |>
    dplyr::rename(AnalysisType = analysisType,
                  Characteristic = characteristic,
                  Unit = unit,
                  Value = value,
                  BelowDL = belowDetectionLimit,
                  WithinPrecision = qa_within_precision_limits,
                  PrecisionNotes = qa_description,
                  Flag = qualifications,
                  SubsiteCode_Chem = ChemLocation,
                  DateProcessed = dateProcessing,
                  VisitDate = DateTime) |>
    dplyr::mutate(AnalysisType = dplyr::case_when(AnalysisType == "routine" ~ "Routine",
                                                  AnalysisType == "duplicate" ~ "Duplicate",
                                                  AnalysisType == "triplicate" ~ "Triplicate",
                                                  TRUE ~ AnalysisType)) |>
    dplyr::arrange(SiteCode, VisitDate)
    
  # ----- CalibrationSpCond -----
  
  agol_layers$CalibrationSpCond <- agol_layers$CalibrationSpCond |>
    dplyr::rename(StandardValue_uS_per_cm = StandardValue_microS_per_cm,
                  PreCalReading_uS_per_cm = PreCalibrationReading_microS_pe,
                  PostCalReading_uS_per_cm = PostCalibrationReading_microS_p) |>
    dplyr::select(SpCondInstrumentID, CalibrationDate, CalibrationTime, StandardValue_uS_per_cm, PreCalReading_uS_per_cm, PostCalReading_uS_per_cm, Notes, SpCondUniqueID)
  
  # ----- CalibrationpH -----
  
  agol_layers$CalibrationpH <- agol_layers$CalibrationpH |>
    dplyr::rename(PreCalReading_pH = PreCalibrationReading_pH,
                  PreCalTemp_C= PreCalibrationTemperature_C,
                  PostCalReading_pH = PostCalibrationReading_pH,
                  PostCalTemp_C = PostCalibrationTemperature_C) |>
    dplyr::select(pHInstrumentID, CalibrationDate, CalibrationTime, StandardValue_pH, PreCalReading_pH, PreCalTemp_C, PostCalReading_pH, PostCalTemp_C, Notes, pHUniqueID)
  
  # ----- CalibrationDO -----

  agol_layers$CalibrationDO <- agol_layers$CalibrationDO |>
    dplyr::rename(BaroPressure_mmHg = BarometricPressure_mmHg,
                  PreCalReading_percent = PreCalibrationReading_percent,
                  PreCalTemp_C = PreCalibrationTemperature_C,
                  PostCalReading_percent = PostCalibrationReading_percent,
                  PostCalTemp_C = PostCalibrationTemperature_C) |>
    dplyr::select(DOInstrumentID, CalibrationDate, CalibrationTime, BaroPressure_mmHg, PreCalReading_percent, PreCalTemp_C, PostCalReading_percent, PostCalTemp_C, DOUniqueID)
  
  return(agol_layers)
}

#' Read data from Aquarius
#'
#' @returns List of tibbles
#'
ReadAquarius <- function(username = "aqreadonly", password = "aqreadonly") {

  if(file.exists("R//timeseries_client.R")) {
  
  timeseries$connect("https://aquarius.nps.gov/aquarius", username, password)
  
  data <- list()
  q_data <- tibble::tibble()
  stage_data <- tibble::tibble()
  depth_data <- tibble::tibble()
  discrete_data <- tibble::tibble()
  volumetric_data <- tibble::tibble()
  
  discharge <- c("LAKE_P_BLUE0", "PARA_P_PAKO0")
  stage <- c("LAKE_P_BLUE0", "MOJA_P_MCSP0", "PARA_P_PAKO0")
  depth <- c("GRBA_W_BAKR0_DP1", "GRBA_W_BAKR0_OC2", "GRBA_W_BAKR0_SH3", "GRBA_W_BGID0")
  discrete <- c("LAKE_P_BLUE0", "MOJA_P_MCSP0", "PARA_P_PAKO0", "LAKE_W_ROGE0", "GRBA_W_BAKR0_DP1", "GRBA_W_BAKR0_OC2", "GRBA_W_BAKR0_SH3", "GRBA_W_BGID0")
  volumetric <- c("LAKE_P_BLUE0")
  
  for (location in discharge) {
    site.imp <- timeseries$getTimeSeriesData(paste0("Discharge.Cumulative@", location))
    site.data <- site.imp$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1,
                    Grade = GradeName1,
                    Approval = ApprovalName1,
                    DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = dplyr::case_when(SiteCode == "LAKE_P_BLUE0" ~ "LAKE",
                                            SiteCode == "PARA_P_PAKO0" ~ "PARA",
                                            TRUE ~ NA_character_))
    q_data <- rbind(q_data, site.data)
  }
  
  q_data <- list(q_data)
  names(q_data) <- "TimeseriesDischarge"
  
  for (location in stage) {
    site.imp <- timeseries$getTimeSeriesData(paste0("Stage.Cumulative@", location))
    site.data <- site.imp$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1,
                    Grade = GradeName1,
                    Approval = ApprovalName1,
                    DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = dplyr::case_when(SiteCode == "LAKE_P_BLUE0" ~ "LAKE",
                                            SiteCode == "MOJA_P_MCSP0" ~ "MOJA",
                                            SiteCode == "PARA_P_PAKO0" ~ "PARA",
                                            TRUE ~ NA_character_))
    stage_data <- rbind(stage_data, site.data)
  }
  
  stage_data <- list(stage_data)
  names(stage_data) <- "TimeseriesStage"
  
  for (location in depth) {
    site.imp <- timeseries$getTimeSeriesData(paste0("DepthToWaterFromGround.Corrected@", location))
    site.data <- site.imp$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1,
                    Grade = GradeName1,
                    Approval = ApprovalName1,
                    DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = dplyr::case_when(stringr::str_detect(SiteCode, "GRBA") ~ "GRBA",
                                            TRUE ~ NA_character_))
    depth_data <- rbind(depth_data, site.data)
  }
  
  depth_data <- list(depth_data)
  names(depth_data) <- "TimeseriesDepth"
  
  for (location in discrete) {
    site.imp <- timeseries$getFieldVisits(location)
    site.data <- site.imp$Details$InspectionActivity$Readings |>
      dplyr::bind_rows() |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = dplyr::case_when(SiteCode == "LAKE_P_BLUE0" ~ "LAKE",
                                            SiteCode == "MOJA_P_MCSP0" ~ "MOJA",
                                            SiteCode == "PARA_P_PAKO0" ~ "PARA",
                                            stringr::str_detect(SiteCode, "GRBA") ~ "GRBA",
                                            TRUE ~ NA_character_)) |>
      dplyr::select(Park, SiteCode, Time, Parameter, MonitoringMethod, Value, Unit, ReadingType) |>
      dplyr::mutate(Value = as.vector(unlist(Value))) |>
      dplyr::rename(DateTime = Time)
    discrete_data <- rbind(discrete_data, site.data)
  }
  
  discrete_data <- list(discrete_data)
  names(discrete_data) <- "TimeseriesFieldVisit"
  
  for (location in volumetric) {
    site.imp <- timeseries$getFieldVisits(location)
    site.data <- site.imp$Details$DischargeActivities |>
      dplyr::bind_rows() |>
      dplyr::mutate(DateTime = as.vector(unlist(DischargeSummary$MeasurementTime)),
                    Unit_Discharge = as.vector(unlist(DischargeSummary$Discharge$Unit)),
                    Unit_Stage = as.vector(unlist(DischargeSummary$MeanGageHeight$Unit)),
                    Unit_Correction = as.vector(unlist(DischargeSummary$GageHeightAdjustmentAmount$Unit)),
                    Value_Discharge = as.vector(unlist(DischargeSummary$Discharge$Numeric)),
                    Value_Stage = as.vector(unlist(DischargeSummary$MeanGageHeight$Numeric)),
                    Value_Correction = as.vector(unlist(DischargeSummary$GageHeightAdjustmentAmount$Numeric)),
                    MonitoringMethod_Stage = as.vector(unlist(DischargeSummary$MeanGageHeightMethod)),
                    MonitoringMethod_Discharge = as.vector(unlist(DischargeSummary$DischargeMethod)),
                    ReadingType = as.vector(unlist(DischargeSummary$DischargeMeasurementReason))) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = dplyr::case_when(SiteCode == "LAKE_P_BLUE0" ~ "LAKE",
                                            TRUE ~ NA_character_)) |>
      dplyr::select(Park, SiteCode, DateTime, MonitoringMethod_Stage, MonitoringMethod_Discharge, Unit_Discharge, Value_Discharge, Unit_Stage, Value_Stage, Unit_Correction, Value_Correction, ReadingType) |>
      tidyr::pivot_longer(cols = c(starts_with("Unit"), starts_with("Value"), starts_with("MonitoringMethod")),
                          names_to = c(".value", "Parameter"),
                          names_pattern = '(.*?)_(.*)') |>
      dplyr::relocate(ReadingType, .after = Value) |>
      dplyr::relocate(MonitoringMethod, .after = Parameter)
    volumetric_data <- rbind(volumetric_data, site.data)
  }
  
  volumetric_data <- list(volumetric_data)
  names(volumetric_data) <- "TimeseriesVolumetric"
  
  aquarius <- c(q_data, stage_data, depth_data, discrete_data, volumetric_data)
  
  # Tidy up the data
  aquarius <- lapply(aquarius, function(df) {
    df |>
      dplyr::mutate(DateTime = lubridate::ymd_hms(DateTime, tz = "America/Los_Angeles", quiet = TRUE)) |>
      dplyr::mutate(FieldSeason = ifelse(lubridate::month(DateTime) < 10,
                                         lubridate::year(DateTime),
                                         lubridate::year(DateTime) + 1))
  })
  } else {
  aquarius <- NULL  
  }
  return(aquarius)
}

#' Read data from IRMA Data Store
#'
#' @param reference_id Reference ID for the published data package as viewed on the IRMA Data Store (e.g., "2307285").
#'
#' @returns List of tibbles
#'
ReadDataStore <- function(reference_id) {
  # Download data package to current working directory
  NPSutils::get_data_package(reference_id)
  # Read data package from working directory into R
  datastore <- NPSutils::load_data_package(reference_id)
  
  return(datastore)
}

#' Read data from local folder
#'
#' @param path Path to local data package.
#'
#' @returns list of tibbles
#'
ReadLocal <- function(path) {
  
  local <- "PH" # placeholder value
  
  return(local)
}

#' Load raw data into the package environment
#' @description Run this function before you do anything else
#'
#' @param source Default is "agol" and "aquarius." Can also be set to "local" or "irma" for downloading directly from the data store.
#'
#' @returns Invisible list of tibbles
#'
#' @examples
LoadSelectedLargeSprings <- function(source = c("agol", "aquarius"),
                                     agol_username = "mojn_data",
                                     agol_password = rstudioapi::askForPassword(paste("Please enter the password for AGOL account", agol_username)),
                                     ...) {
  
  data <- list()
  
  if("agol" %in% source) {
    agol <- WrangleAGOL(...)
    data <- append(data, agol)
  }
  
  if("aquarius" %in% source) {
    aquarius <- ReadAquarius(...)
    data <- append(data, aquarius)
  }
  
  if("irma" %in% source) {
    irma <- ReadDataStore(...)
    data <- append(data, irma)
  }
  
  if("local" %in% source) {
    local <- ReadLocal(...)
    data <- append(data, local)
  }

  # Tidy up the data
  data <- lapply(data, function(df) {
    df |>
      dplyr::mutate_if(is.character, utf8::utf8_encode) |>
      dplyr::mutate_if(is.character, trimws, whitespace = "[\\h\\v]") |>  # Trim leading and trailing white space
      dplyr::mutate_if(is.character, dplyr::na_if, "") |>  # Replace empty strings with NA
      dplyr::mutate_if(is.numeric, dplyr::na_if, -9999) |>  # Replace -9999 or -999 with NA
      dplyr::mutate_if(is.numeric, dplyr::na_if, -999) |>
      dplyr::mutate_if(is.character, dplyr::na_if, "NA") |>  # Replace "NA" strings with NA
      dplyr::mutate_if(is.character, stringr::str_replace_all, pattern = "[\\v]+", replacement = ";  ")  # Replace newlines with semicolons - reading certain newlines into R can cause problems
  })
    
  # Load the data into an environment for the package to use
  tbl_names <- names(data)
  lapply(tbl_names, function(n) {assign(n, data[[n]], envir = pkg_globals)})
  
  invisible(data)
}

#' Read an individual tibble from the package environment
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "DEVA".
#' @param site Optional. Site code to filter on, e.g. "JOTR_P_FORT0".
#' @param field.season Optional. Field season name to filter on, e.g. "2024".
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "CalibrationDO", "VisitQuarterly". See details for full list of data name options.
#'
#' @return Tibble of filtered data
#' @export
#'
#' @details \code{data.name} options are: 
#'
ReadAndFilterData <- function(park, site, field.season, data.name) {
  filtered.data <- get_data(data.name)
 
  if (!missing(field.season)) {
    field.season <- as.character(field.season)
  }
  
  if (!missing(park)) {
    filtered.data <- filtered.data |>
      dplyr::filter(Park %in% park) # Changed to allow filtering of multiple parks
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for the park specified"))
    }
  }
  
  if (!missing(site) & nrow(filtered.data) > 0) {
    filtered.data <- filtered.data |>
      dplyr::filter(SiteCode == site)
    
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for the site specified"))
    }
  }
  
  if ("FieldSeason" %in% names(filtered.data)) {
    filtered.data <- filtered.data |>
      dplyr::mutate(FieldSeason = as.character(FieldSeason))
  }
  
  if (!missing(field.season) & ("FieldSeason" %in% colnames(filtered.data)) & nrow(filtered.data) > 0) {
    filtered.data <- filtered.data |>
      dplyr::filter(FieldSeason %in% field.season)
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for one or more of the field seasons specified"))
    }
  }
  
  return(filtered.data)
}
