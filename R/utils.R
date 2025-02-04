#' @importFrom magrittr %>% %<>%

pkg_globals <- new.env(parent = emptyenv())

# Load data from global package environment
get_data <- function(data.name) {
  if (!missing(data.name)) {
    if (!(data.name %in% c(names(GetAGOLColSpec()), names(GetAquariusColSpec())))) {
      stop("Invalid data table name. Use names(selectedlargesprings:::GetColSpec()) to see valid options for data.name.")
    }
    tryCatch({data <- get(data.name, pkg_globals)},
             error = function(e) {
               if (grepl(".*object.* not found.*", e$message, ignore.case = TRUE)) {
                 stop(paste0("Could not find data. Did you remember to call LoadSelectedLargeSprings?\n\tOriginal error: ", e$message))
               }
               else {e}
             })
  } else {
    tryCatch({
      data <- lapply(c(names(GetAGOLColSpec()), names(GetAquariusColSpec())), get, pkg_globals)
      names(data) <- c(names(GetAGOLColSpec()), names(GetAquariusColSpec()))
    },
    error = function(e) {
      if (grepl(".*object.* not found.*", e$message, ignore.case = TRUE)) {
        stop(paste0("Could not find data. Did you remember to call LoadSelectedLargeSprings?\n\tOriginal error: ", e$message))
      }
      else {e}
    }
    )
    
  }
  
  return(data)
}

#' Open a connection to Aquarius
#'
#' @return A database connection pool object
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#' conn <- OpenDatabaseConnection()
#' }
OpenDatabaseConnection <- function() {
#Connect to Aquarius
  tryCatch({#fetchaquarius::connectToAquarius("aqreadonly", "aqreadonly")
    timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")
    assign(x = "aq", value = timeseries, envir = pkg_globals)},
    # aq <<- timeseries},
    error = function(e) {
      assign(x = "aq", value = NA, envir = pkg_globals)
      # aq <<- NA
      warning(paste("Could not connect to Aquarius. Verify that you are on the NPS network and that Aquarius is not down.", "Error message:", e, sep = "\n"))
    }
  )
  
  conn <- list(aquarius = get(x = "aq",
                              envir = pkg_globals))
  
  return(conn)
}

#' Get column specifications for AGOL database
#'
#' @return A list of column specifications for each table of Aquarius data.
#'
GetAGOLColSpec <- function() {
  col.spec <- list(
    BMIMetrics = readr::cols(
      SampleID = readr::col_integer(),
      CollectionDate = readr::col_date(),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    BMISpecies = readr::cols(
      SampleID = readr::col_integer(),
      CollectionDate = readr::col_date(),
      LabCount = readr::col_integer(),
      BigRareCount = readr::col_integer(),
      Sample_ID = readr::col_double(),
      .default = readr::col_character()
    ),
    BMIVisit = readr::cols(
      SampleID = readr::col_integer(),
      NAMC_Latitude = readr::col_double(),
      NAMC_Longitude = readr::col_double(),
      Customer_Latitude = readr::col_double(),
      Customer_Longitude = readr::col_double(),
      CollectionDate = readr::col_date(),
      Area = readr::col_double(),
      FieldSplit = readr::col_double(),
      LabSplit = readr::col_double(),
      SplitCount = readr::col_integer(),
      .default = readr::col_character()
    ),
    CalibrationDO = readr::cols(
      VisitDate = readr::col_date(),
      CalibrationDate = readr::col_date(),
      BarometricPressure_mmHg = readr::col_double(),
      PreCalibrationReading_percent = readr::col_double(),
      PreCalibrationTemperature_C = readr::col_double(),
      PostCalibrationReading_percent = readr::col_double(),
      PostCalibrationTemperature_C = readr::col_double(),
      .default = readr::col_character()
    ),
    CalibrationpH = readr::cols(
      VisitDate = readr::col_date(),
      CalibrationDate = readr::col_date(),
      StandardValue_pH = readr::col_double(),
      TemperatureCorrectedStd_pH = readr::col_double(),
      PreCalibrationReading_pH = readr::col_double(),
      PreCalibrationTemperature_C = readr::col_double(),
      PostCalibrationReading_pH = readr::col_double(),
      PostCalibrationTemperature_C = readr::col_double(),
      .default = readr::col_character()
    ),
    CalibrationSpCond = readr::cols(
      VisitDate = readr::col_date(),
      CalibrationDate = readr::col_date(),
      StandardValue_microS_per_cm = readr::col_double(),
      PreCalibrationReading_microS_per_cm = readr::col_double(),
      PostCalibrationReading_microS_per_cm = readr::col_double(),
      .default = readr::col_character()
    )
  )
  
  return(col.spec)
}

#' Wrangle AGOL data into a set of dataframes structured for use in this package.
#'
#' @param agol_layers The list of tibbles returned by `FetchAGOLLayers()`
#'
#' @return A list of tibbles
#' 
WrangleAGOLData <- function(agol_layers) {
  data <- list()
  
  # Clean up visit table and replace numeric keys with meaningful values
  visit <- agol_layers$visit %>%
    dplyr::select(-SiteCodeText) %>%
    dplyr::rename(VisitTypeCode = VisitType,
                  SpringTypeCode = SpringType,
                  GPSCode = GPS,
                  CameraCode = Camera,
                  CameraCardCode = CameraCard,
                  SensorTypeDepCode = SensorTypeDep,
                  FlowConditionCode = FlowCondition,
                  WQInstrumentCode = WQInstrument,
                  pHInstrumentCode = pHInstrument,
                  DOInstrumentCode = DOInstrument,
                  SpCondInstrumentCode = SpCondInstrument,
                  TemperatureInstrumentCode = TemperatureInstrument,
                  RoadsCode = Roads,
                  HumanUseCode = HumanUse,
                  PlantManagementCode = PlantManagement,
                  HikingTrailsCode = HikingTrails,
                  LivestockCode = Livestock,
                  FireCode = Fire,
                  FloodingCode = Flooding,
                  WildlifeCode = Wildlife,
                  OverallCode = Overall) %>%
    dplyr::mutate(EstimatedDischarge_L_per_sec = as.integer(EstimatedDischarge_L_per_sec)) %>%
    dplyr::left_join(dplyr::select(agol_layers$sites, name, SiteName = sitename, SiteCode, SampleFrame, PanelCode = PanelGroup), by = c("SiteCode" = "SiteCode")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_Panel, name, Panel = label), by = c("PanelCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_VisitType, name, VisitType = label), by = c("VisitTypeCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_MonitoringStatus, name, MonitoringStatus = label), by = c("Status" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringType, name, SpringType = label), by = c("SpringTypeCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_GPSUnit, name, GPS = label), by = c("GPSCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_Camera, name, Camera = label), by = c("CameraCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_CameraCard, name, CameraCard = label), by = c("CameraCardCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_FlowCondition, name, FlowCondition = label), by = c("FlowConditionCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_SensorModel, name, SensorTypeDep = label), by = c("SensorTypeDepCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_Sensor, name, SensorDep = label), by = c("SensorIDDep" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringbrookLengthFlag, name, SpringbrookLengthFlag = label), by = c("SPBKLength" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringbrookLengthFlag, name, DiscontinuousSpringbrookLengthFlag = label), by = c("DiscontinuousSPBKLength" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DischargeEstimatedClass, name, DischargeClass_L_per_s = label), by = c("EstimatedDischarge_L_per_sec" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_WaterQualityDataCollected, name, WQDataCollected = label), by = c("WasWaterQualityDataCollected" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, WQInstrument = label), by = c("WQInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, pHInstrument = label), by = c("pHInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, DOInstrument = label), by = c("DOInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, SpCondInstrument = label), by = c("SpCondInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, TemperatureInstrument = label), by = c("TemperatureInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_FlowModificationStatus, name, FlowModificationStatus = label), by = c("FlowModification" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Roads = label), by = c("RoadsCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, HumanUse = label), by = c("HumanUseCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, PlantManagement = label), by = c("PlantManagementCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, HikingTrails = label), by = c("HikingTrailsCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Livestock = label), by = c("LivestockCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, OtherAnthropogenic = label), by = c("Other_Anthro" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Fire = label), by = c("FireCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Flooding = label), by = c("FloodingCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Wildlife = label), by = c("WildlifeCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, OtherNatural = label), by = c("Other_Natural" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Overall = label), by = c("OverallCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_Shared_YesNo, name, IsWildlifeObserved = label), by = c("Waswildlifeobserved" = "name")) %>%
    dplyr::mutate(VisitDate = lubridate::as_date(DateTime),
                  FieldSeason = ifelse(lubridate::month(VisitDate) < 10, lubridate::year(VisitDate), lubridate::year(VisitDate) + 1)) %>%
    dplyr::rename(IsSensorRetrieved = SensorRetrieved,
                  IsVegetationObserved = WasRiparianVegetationObserved,
                  InvasivesObserved = WereInvasivesObserved,
                  pH_DataQualityFlag = pH_Flag,
                  Temp_C_DataQualityFlag = Temp_C_Flag,
                  SpCond_microS_DataQualityFlag = SpCond_microS_Flag,
                  DO_DataQualityFlag = DO_Flag,
                  GPSUnit = GPS,
                  visitglobalid = globalid,
                  SpringbrookType = SPBKType,
                  SpringbrookLength_m = Length_m,
                  SpringbrookWidth_m = Width_m,
                  DiscontinuousSpringbrookLength_m = DiscontinuousLength_m,
                  EstimatedCapture_percent = EstimatedCapture_Percent,
                  Notes = SpringComments,
                  Protocol = ProtocolID,
                  DPL = DataProcessingLevel)
  
  # ----- CalibrationDO -----
  data$CalibrationDO <- visit %>%
    dplyr::filter(grepl("DO",ParametersCollected)) %>%
    dplyr::inner_join(agol_layers$CalibrationDO, by = "DOUniqueID") %>%
    dplyr::mutate(StartTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationDate = lubridate::as_date(CalibrationDate.y)) %>%
    dplyr::left_join(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, by = c("DOInstrumentID" = "name")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, StartTime, FieldSeason, VisitType, CalibrationDate, 
                  CalibrationTime, DOInstrument = label, BarometricPressure_mmHg, PreCalibrationReading_percent, 
                  PreCalibrationTemperature_C, PostCalibrationReading_percent, PostCalibrationTemperature_C,
                  Notes = Notes.y)
  
  # ----- CalibrationpH -----
  data$CalibrationpH <- visit %>%
    dplyr::filter(grepl("pH",ParametersCollected)) %>%
    dplyr::select(visitglobalid, pHUniqueID_7, pHUniqueID_10, pHUniqueID_4) %>%
    tidyr::pivot_longer(cols=dplyr::starts_with("pHUniqueID_"),
                        values_to = "pHUniqueID", names_to = NULL) %>%
    dplyr::left_join(visit, by = "visitglobalid") %>%
    dplyr::mutate(StartTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::inner_join(agol_layers$CalibrationpH, by = "pHUniqueID") %>%
    dplyr::mutate(StartTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationDate = lubridate::as_date(CalibrationDate.y)) %>%
    dplyr::left_join(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, by = c("pHInstrumentID" = "name")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, StartTime, FieldSeason, VisitType, CalibrationDate, 
                  CalibrationTime, pHInstrument = label, StandardValue_pH, TemperatureCorrectedStd_pH,
                  PreCalibrationReading_pH, PreCalibrationTemperature_C, PostCalibrationReading_pH, 
                  PostCalibrationTemperature_C, Notes = Notes.y, pHUniqueID)
  
  # ----- CalibrationSpCond -----
  data$CalibrationSpCond <- visit %>%
    dplyr::filter(grepl("SpCond",ParametersCollected)) %>%
    dplyr::inner_join(agol_layers$CalibrationSpCond, by = "SpCondUniqueID") %>%
    dplyr::mutate(StartTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationDate = lubridate::as_date(CalibrationDate.y)) %>%
    dplyr::left_join(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, by = c("SpCondInstrumentID" = "name")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, StartTime, FieldSeason, VisitType, CalibrationDate, 
                  CalibrationTime, SpCondInstrument = label, StandardValue_microS_per_cm, 
                  PreCalibrationReading_microS_per_cm = PreCalibrationReading_microS_pe,
                  PostCalibrationReading_microS_per_cm = PostCalibrationReading_microS_p, Notes = Notes.y)
  
  # ----- Photos -----
  rep_photos_int <- agol_layers$repeats_int %>%
    dplyr::select(repphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  rep_photos_ext <- agol_layers$repeats_ext %>%
    dplyr::select(repphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  rep_photo_files <- dplyr::bind_rows(rep_photos_ext, rep_photos_int) %>%
    dplyr::mutate(IsLibraryPhoto = "TBD")
  
  rep_photos <- agol_layers$repeats %>%
    dplyr::select(repphotoglobalid = globalid, visitglobalid = parentglobalid, PhotoType, Notes = PhotoNotes_ExternalCamera) %>%
    dplyr::mutate(UtmX_m = 0, UtmY_m = 0,
                  PhotoSOP = "RPT") %>%
    dplyr::left_join(rep_photo_files, by ="repphotoglobalid")
  
  veg_photos_int <- agol_layers$riparian_veg_int %>%
    dplyr::select(vegphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  veg_photos_ext <- agol_layers$riparian_veg_ext %>%
    dplyr::select(vegphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  veg_photo_files <- dplyr::bind_rows(veg_photos_ext, veg_photos_int) %>%
    dplyr::mutate(IsLibraryPhoto = "TBD")
  
  veg_photos <- agol_layers$riparian_veg %>%
    dplyr::select(vegphotoglobalid = globalid, visitglobalid = parentglobalid, PhotoType = LifeForm) %>%
    dplyr::mutate(UtmX_m = 0, UtmY_m = 0,
                  PhotoSOP = "RVG") %>%
    dplyr::left_join(veg_photo_files, by ="vegphotoglobalid")
  
  inv_photos_ext <- agol_layers$invasives_ext %>%
    dplyr::select(invphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  inv_photos_int <- agol_layers$invasives_int %>%
    dplyr::select(invphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  inv_photo_files <- dplyr::bind_rows(inv_photos_ext, inv_photos_int) %>%
    dplyr::mutate(IsLibraryPhoto = "TBD")
  
  inv_photos <- agol_layers$invasives %>%
    dplyr::select(invphotoglobalid = globalid, visitglobalid = parentglobalid, PhotoType = InvasiveSpeciesCode) %>%
    dplyr::mutate(UtmX_m = 0, UtmY_m = 0,
                  PhotoSOP = "INV") %>%
    dplyr::left_join(inv_photo_files, by ="invphotoglobalid")
  
  add_photos_ext <- agol_layers$additional_photos_ext %>%
    dplyr::select(addphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  add_photos_int <- agol_layers$additional_photos_int %>%
    dplyr::select(addphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  add_photo_files <- dplyr::bind_rows(add_photos_ext, add_photos_int)
  
  add_photos <- agol_layers$additional_photos %>%
    dplyr::select(addphotoglobalid = globalid, visitglobalid = parentglobalid, PhotoType = AdditionalPhotoType, IsLibraryPhoto) %>%
    dplyr::mutate(UtmX_m = 0, UtmY_m = 0,
                  PhotoSOP = "MSC") %>%
    dplyr::left_join(add_photo_files, by ="addphotoglobalid")
  
  photos <- dplyr::bind_rows(veg_photos, rep_photos, inv_photos, add_photos)
  
  data$Photo <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, MonitoringStatus, SpringType, SampleFrame, DPL, Camera, CameraCard, GPSUnit, visitglobalid) %>%
    dplyr::mutate(DateTaken = VisitDate) %>%
    dplyr::inner_join(photos, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, MonitoringStatus, SpringType, SampleFrame, DPL, Camera, CameraCard, DateTaken, 
                  PhotoType, IsLibraryPhoto, OriginalFilePath, RenamedFilePath = renamedfilepath,GPSUnit, UtmX_m, UtmY_m, Notes, PhotoSOP)
  
  # ----- DischargeEstimated -----
  data$DischargeEstimated <- visit %>%
    dplyr::filter(DischargeMethod == "EST") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowCondition, DischargeClass_L_per_s, VisitType, DPL)
  
  # ----- DischargeFlowCondition -----
  data$DischargeFlowCondition <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowCondition, SpringbrookType, SpringbrookLengthFlag, SpringbrookLength_m, SpringbrookWidth_m, DiscontinuousSpringbrookLengthFlag, DiscontinuousSpringbrookLength_m, VisitType, DPL, Notes, SpringbrookNotes = DischargeNotes)
  
  # ----- DischargeVolumetric -----
  vol <- agol_layers$fill_time %>%
    dplyr::select(visitglobalid = parentglobalid, FillTime_seconds = FillTime_sec)
  
  data$DischargeVolumetric <- visit %>%
    dplyr::filter(DischargeMethod == "VOL") %>%
    dplyr::left_join(vol, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowCondition, ContainerVolume_mL, FillTime_seconds, EstimatedCapture_percent, VisitType, DPL)
  
  # ----- Disturbance -----
  data$Disturbance <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, Roads, HumanUse, PlantManagement, HikingTrails, Livestock, OtherAnthropogenic, Fire, Flooding, Wildlife, OtherNatural, Overall, FlowModificationStatus, VisitType, Notes = DisturbanceNotes, DPL)
  
  # ----- DisturbanceFlowModification -----
  flow_mod <- agol_layers$disturbance_flow_mod %>%
    dplyr::select(visitglobalid = parentglobalid, ModificationType)
  
  data$DisturbanceFlowModification <- visit %>%
    dplyr::left_join(flow_mod, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowModificationStatus, ModificationType, VisitType, DPL)
  
  # ----- Invasives -----
  invasives <- agol_layers$invasives %>%
    dplyr::select(visitglobalid = parentglobalid, InvasiveSpecies, RiparianVegBuffer) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_Taxon, name, ScientificName = scientificname), by = c("InvasiveSpecies" = "name")) %>%
    dplyr::select(visitglobalid, InRiparianVegBuffer = RiparianVegBuffer, USDAPlantsCode = InvasiveSpecies, ScientificName)
  
  data$Invasives <- visit %>%
    dplyr::left_join(invasives, by = "visitglobalid") %>%
    dplyr::left_join(agol_layers$MOJN_Ref_DS_ParkTaxonProtectedStatus, by = c("USDAPlantsCode" = "Taxon", "Park" = "parkname")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, InvasivesObserved, InRiparianVegBuffer, USDAPlantsCode, ScientificName, VisitType, ProtectedStatus = ProtectedStatusCode, DPL, Notes = InvasiveNotes)
  
  # ----- Riparian -----
  riparian <- agol_layers$riparian_veg %>%
    dplyr::select(visitglobalid = parentglobalid, LifeForm = lifeformname, DominantSpecies)
  
  riparian_visit <- visit %>%
    dplyr::select(visitglobalid, Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, MistletoePresent, `Woody >4m` = WoodyGT4m, `Woody 2-4m` = Woody2to4m, `Woody <2m` = WoodyLT2m, Forb, Rush, Grass, Reed, Sedge, Cattail, Bryophyte, `Non-Plant` = NonPlant, VisitType, DPL, Notes = RiparianVegetationNotes) %>%
    tidyr::pivot_longer(c(`Woody >4m`, `Woody 2-4m`, `Woody <2m`, Forb, Rush, Grass, Reed, Sedge, Cattail, Bryophyte, `Non-Plant`), names_to = "LifeForm", values_to = "Rank") %>%
    dplyr::group_by(visitglobalid) %>%
    dplyr::mutate(no_veg = all(is.na(Rank)),
                  LifeForm = ifelse(no_veg, NA, LifeForm)) %>%
    dplyr::ungroup() %>%
    unique() %>%
    dplyr::filter((!is.na(LifeForm) & (Rank != 12)) | (is.na(LifeForm) & is.na(Rank))) %>%
    dplyr::select(-no_veg)
  
  data$Riparian <- riparian_visit %>%
    dplyr::left_join(riparian, by = c("visitglobalid", "LifeForm")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, MistletoePresent, LifeForm, Rank, DominantSpecies, VisitType, DPL, Notes)
  
  # ----- SensorRetrievalAttempts -----
  sensor_retrieval <- agol_layers$sensor_retrieval %>%
    dplyr::select(visitglobalid = parentglobalid, RetrievalTime, DownloadSuccessful, UploadSuccessful, Notes = SensorRetrieveNotes, SensorProblem, SensorIDRet)
  
  retrieval_visit <- visit %>%
    dplyr::select(visitglobalid, IsSensorSpring, RetrievalFieldSeason = FieldSeason, SiteName, SiteCode, Park, RetrievalDate = VisitDate, RetrievalVisitType = VisitType, SensorRetrieved = IsSensorRetrieved)
  
  sensor_deployment <- visit %>%
    # dplyr::filter(IsSensorSpring == "Y") %>%
    dplyr::select(visitglobalid, DeploymentFieldSeason = FieldSeason, DeploymentDate = VisitDate, SiteCode, SiteName, Park, IsSensorSpring, DeploymentVisitType = VisitType, SensorDeployed, SensorDep, SensorIDDep, DeploymentTime, SensorDeployNote)
  
  sensorRetrieval_visit <- sensor_retrieval %>%
    dplyr::inner_join(retrieval_visit, by = "visitglobalid")
  
  # data$SensorRetrievalAttempts <- visit %>%
  #   dplyr::filter(IsSensorSpring == "Y") %>%
  #   dplyr::select(visitglobalid, RetrievalFieldSeason = FieldSeason, SiteName, SiteCode, Park, RetrievalDate = VisitDate, SensorRetrieved = IsSensorRetrieved) %>%
  #   dplyr::left_join(sensor_retrieval, by = "visitglobalid") %>%
  #   dplyr::left_join(sensor_deployment, by = c("SiteCode", "SensorIDRet" = "SensorIDDep")) %>%
  #   dplyr::filter(DeploymentDate < RetrievalDate) %>%
  #   # dplyr::mutate(time_since_dep = RetrievalDate - DeploymentDate) %>%
  #   # dplyr::group_by(SensorIDRet, time_since_dep) %>%
  #   # dplyr::filter(time_since_dep == min(time_since_dep)) %>%
  #   dplyr::ungroup()
  data$SensorRetrievalAttempts <- sensor_retrieval %>%
    dplyr::inner_join(retrieval_visit, by = "visitglobalid") %>%
    dplyr::filter(IsSensorSpring == "Y") %>%
    dplyr::inner_join(sensor_deployment, by = c("SiteCode", "SensorIDRet" = "SensorIDDep")) %>%
    dplyr::left_join(agol_layers$MOJN_Ref_DS_Sensor, by = c("SensorIDRet" = "name")) %>%
    dplyr::rename(SensorNumber = label) %>%
    dplyr::filter(DeploymentDate < RetrievalDate) %>%
    dplyr::select(SensorNumber, SerialNumber, DeploymentDate, DeploymentFieldSeason, RetrievalDate, RetrievalFieldSeason, SiteName = SiteName.x, SiteCode, Park = Park.x, SensorRetrieved, SensorProblem, DownloadResult = DownloadSuccessful, RetrievalVisitType, DeploymentVisitType, Notes)
  
  unk_sensors <- data$SensorRetrievalAttempts %>% 
    dplyr::filter(is.na(SerialNumber) | grepl("-9+", SerialNumber)) %>%
    dplyr::select(SensorNumber, SerialNumber, DeploymentDate, RetrievalDate, SiteName, SiteCode, SensorRetrieved) %>%
    unique()
  
  if(nrow(unk_sensors) > 0) {
    warn <- paste("Omitted", nrow(unk_sensors), "sensor retrievals due to missing serial numbers. Call `unk_sensors()` to see a list of omitted sensors.")
    assign("unk_sensors", unk_sensors, pkg_globals)
    warning(warn)
  }
  
  data$SensorRetrievalAttempts %>% 
    dplyr::filter(!is.na(SerialNumber) & !grepl("-9+", SerialNumber))
  
  # ----- SensorsCurrentlyDeployed -----
  data$SensorsCurrentlyDeployed <- sensor_deployment %>%
    dplyr::inner_join(sensorRetrieval_visit, by = c("SiteCode", "SensorIDDep" = "SensorIDRet", "DeploymentDate" = "RetrievalDate")) %>%
    dplyr::filter(SensorDeployed == "Y") %>%
    dplyr::left_join(agol_layers$MOJN_Ref_DS_Sensor, by = c("SensorIDDep" = "name")) %>%
    dplyr::rename(SensorNumber = label) %>%
    dplyr::select(SensorNumber, SerialNumber, SiteCode, SiteName = SiteName.x, 
                  VisitDate = DeploymentDate, FieldSeason = DeploymentFieldSeason, Park = Park.x, VisitType = DeploymentVisitType, Notes = SensorDeployNote)
  
  # ----- SensorsAllDeployments -----
  data$SensorsAllDeployments <- sensor_deployment %>%
    dplyr::inner_join(visit, by = c("visitglobalid" = "visitglobalid")) %>%
    dplyr::filter(IsSensorSpring.x == "Y", SensorDeployed.x == "Y") %>%
    dplyr::left_join(agol_layers$MOJN_Ref_DS_Sensor, by = c("SensorIDDep.y" = "name")) %>%
    dplyr::rename(SensorNumber = label) %>%
    dplyr::select(SensorNumber, SerialNumber, SiteCode = SiteCode.x, SiteName = SiteName.x, 
                  VisitDate, FieldSeason, Park = Park.x, VisitType, Notes = SensorDeployNote.x, SensorDeployed = SensorDeployed.x)
  
  # ----- Site -----
  data$Site <- agol_layers$sites %>%
    dplyr::select(Park,
                  Subunit,
                  SiteCode,
                  SiteName = sitename,
                  SiteDescription,
                  EcologicalDescription,
                  LogisticalDescription,
                  DriveDescription,
                  HikeDescription,
                  HikeDistance_m,
                  HikeTime_min,
                  GRTSDraw,
                  GRTSOrder, 
                  SiteStatus,
                  SampleFrame,
                  Panel = PanelGroup,
                  SiteProtectedStatus,
                  Lat_WGS84,
                  Lon_WGS84,
                  X_UTM_NAD83_11N, 
                  Y_UTM_NAD83_11N)
  
  # ----- Visit -----
  data$Visit <- visit %>%
    dplyr::select(Park,
                  SiteCode,
                  SiteName,
                  VisitDate,
                  FieldSeason,
                  SampleFrame,
                  Panel,
                  VisitType,
                  MonitoringStatus,
                  SpringType,
                  Notes,
                  DPL
    )
  
  # ----- VisitActivity -----
  yn <- agol_layers$MOJN_Lookup_Shared_YesNo$label
  names(yn) <- agol_layers$MOJN_Lookup_Shared_YesNo$name
  wq_collected <- agol_layers$MOJN_Lookup_DS_WaterQualityDataCollected$label
  names(wq_collected) <- agol_layers$MOJN_Lookup_DS_WaterQualityDataCollected$name
  data$VisitActivity <- visit %>%
    dplyr::select(Park,
                  SiteCode,
                  SiteName,
                  VisitDate,
                  FieldSeason,
                  SpringType,
                  FlowCondition, 
                  WQDataCollected = WasWaterQualityDataCollected, 
                  InvasivesObserved,
                  RiparianObserved = IsVegetationObserved,
                  MistletoePresent,
                  WildlifeObserved = Waswildlifeobserved,
                  SpringbrookLength_Class = SpringbrookLengthFlag,
                  SpringbrookWidth_m,
                  SpringbrookLength_m,
                  SampleFrame,
                  VisitType,
                  MonitoringStatus,
                  WaterQualityNotes = WQNotes) %>%
    dplyr::mutate(WQDataCollected = wq_collected[WQDataCollected],
                  InvasivesObserved = yn[InvasivesObserved],
                  RiparianObserved = yn[RiparianObserved],
                  MistletoePresent = yn[MistletoePresent],
                  WildlifeObserved = yn[WildlifeObserved])
  
  
  # ----- WaterQualityDO -----
  percent <- visit %>%
    dplyr::filter(WQDataCollected == "Yes") %>%
    dplyr::select(visitglobalid, WQDataCollected,
                  DissolvedOxygen_percent_1, DissolvedOxygen_percent_2, DissolvedOxygen_percent_3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("DissolvedOxygen_"),
                        values_to = "DissolvedOxygen_percent", names_to = NULL) %>%
    dplyr::right_join(visit, by = "visitglobalid") %>%
    dplyr::select(visitglobalid, Park, SiteCode, SiteName, VisitDate, FieldSeason, 
                  WQDataCollected = WQDataCollected.y, DissolvedOxygen_percent)
  
  percent <- dplyr::mutate(percent, ID = 1:nrow(percent))
  
  mg <- visit %>% 
    dplyr::filter(WQDataCollected == "Yes") %>%
    dplyr::select(visitglobalid,
                  DissolvedOxygen_mg_per_L_1, DissolvedOxygen_mg_per_L_2, DissolvedOxygen_mg_per_L_3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("DissolvedOxygen_"),
                        values_to = "DissolvedOxygen_mg_per_L", names_to = NULL) %>%
    dplyr::right_join(visit, by = "visitglobalid") %>%
    dplyr::select(visitglobalid, WQNotes, DO_DataQualityFlag, DOInstrument, 
                  VisitType, DPL, MonitoringStatus, DissolvedOxygen_mg_per_L)
  
  mg <- dplyr::mutate(mg, ID = 1:nrow(mg))
  
  WaterQualityDO <- mg %>%
    dplyr::inner_join(percent, by = c("visitglobalid", "ID")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, WQDataCollected, 
                  DissolvedOxygen_percent, DissolvedOxygen_mg_per_L, DataQualityFlag = DO_DataQualityFlag, 
                  DataQualityFlagNote = WQNotes, DOInstrument, VisitType, DPL, MonitoringStatus)
  
  wqDO_filterRepeats <- WaterQualityDO %>%
    dplyr::filter(is.na(DissolvedOxygen_percent), is.na(DissolvedOxygen_mg_per_L)) %>%
    unique()
  
  WaterQualityDO <- WaterQualityDO %>%
    dplyr::filter_at(dplyr::vars(DissolvedOxygen_mg_per_L, DissolvedOxygen_percent), dplyr::any_vars(!is.na(.)))
  
  data$WaterQualityDO <- dplyr::bind_rows(wqDO_filterRepeats, WaterQualityDO)
  
  # ----- WaterQualitypH -----
  WaterQualitypH <- visit %>%
    dplyr::select(visitglobalid, pH_1, pH_2, pH_3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("pH_"),
                        values_to = "pH", names_to = NULL) %>%
    dplyr::right_join(visit, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, WQDataCollected, pH, DataQualityFlag = pH_DataQualityFlag, 
                  DataQualityFlagNote = WQNotes, pHInstrument, VisitType, DPL, MonitoringStatus) 
  
  wqpH_filterRepeats <- WaterQualitypH %>%
    dplyr::filter(is.na(pH)) %>%
    unique()
  
  WaterQualitypH <- WaterQualitypH %>%
    dplyr::filter(!is.na(pH))
  
  data$WaterQualitypH <- dplyr::bind_rows(wqpH_filterRepeats, WaterQualitypH)
  
  
  # ----- WaterQualitySpCond -----
  WaterQualitySpCond <- visit %>%
    dplyr::select(visitglobalid, SpecificConductance_microS_1, SpecificConductance_microS_2, SpecificConductance_microS_3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("SpecificConductance_"),
                        values_to = "SpecificConductance_microS_per_cm", names_to = NULL) %>%
    dplyr::right_join(visit, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, WQDataCollected, 
                  SpecificConductance_microS_per_cm, DataQualityFlag = SpCond_microS_DataQualityFlag, DataQualityFlagNote = WQNotes, 
                  SpCondInstrument, VisitType, DPL, MonitoringStatus)
  
  wqSpCond_filterRepeats <- WaterQualitySpCond %>%
    dplyr::filter(is.na(SpecificConductance_microS_per_cm))%>%
    unique()
  
  WaterQualitySpCond <- WaterQualitySpCond %>%
    dplyr::filter(!is.na(SpecificConductance_microS_per_cm))
  
  data$WaterQualitySpCond <- dplyr::bind_rows(WaterQualitySpCond, wqSpCond_filterRepeats)
  
  # ----- WaterQualityTemperature -----
  WaterQualityTemperature <- visit %>%
    dplyr::select(visitglobalid, Temperature_C_1, Temperature_C_2, Temperature_C_3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Temperature_"),
                        values_to = "WaterTemperature_C", names_to = NULL) %>%
    dplyr::right_join(visit, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, WQDataCollected, WaterTemperature_C, 
                  DataQualityFlag = Temp_C_DataQualityFlag, DataQualityFlagNote = WQNotes, TemperatureInstrument, 
                  VisitType, DPL, MonitoringStatus)
  
  wqTemp_filterRepeats <- WaterQualityTemperature %>%
    dplyr::filter(is.na(WaterTemperature_C)) %>%
    unique()
  
  WaterQualityTemperature <- WaterQualityTemperature %>%
    dplyr::filter(!is.na(WaterTemperature_C))
  
  data$WaterQualityTemperature <- dplyr::bind_rows(WaterQualityTemperature, wqTemp_filterRepeats)
  
  # ----- Wildlife -----
  data$Wildlife <- agol_layers$wildlife %>%
    dplyr::inner_join(visit, by = c("parentglobalid" = "visitglobalid")) %>%
    dplyr::filter(VisitDate > "2018-11-04") %>%
    dplyr::left_join(agol_layers$MOJN_Lookup_DS_WildlifeType, by = c("WildlifeType" = "name")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, IsWildlifeObserved, WildlifeType = label, DirectObservation, Scat, Tracks, Shelter, Foraging, Vocalization, OtherEvidence, Notes = Species_Notes, VisitType, DPL) %>%
    dplyr::mutate(DirectObservation = yn[DirectObservation],
                  Scat = yn[Scat],
                  Tracks = yn[Tracks],
                  Shelter = yn[Shelter],
                  Foraging = yn[Foraging],
                  Vocalization = yn[Vocalization],
                  OtherEvidence = yn[OtherEvidence])
  
  
  
  return(data)
}


#' Read data from the Selected Large Springs AGOL feature layers. Returns the raw data in its current format on AGOL. Mostly used for data management purposes. 
#'
#' @param data_path URL to Desert Springs feature service on AGOL.
#' @param lookup_path URL to feature service on AGOL containing Desert Springs lookup tables.
#' @param sites_path URL to feature service on AGOL containing sites table
#' @param calibration_path URL to feature service on AGOL containing calibration tables.
#' @param agol_username Username of headless AGOL account with permissions to view the feature service.
#' @param agol_password Password for headless AGOL account.
#'
#' @return A list of tibbles
#' @export 
#'
FetchAGOLLayers <- function(data_path = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_SpringVisit/FeatureServer",
                            lookup_path = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Lookup_Database/FeatureServer",
                            sites_path = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_Sites_Master/FeatureServer",
                            calibration_path = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Calibration_Database/FeatureServer",
                            agol_username = "mojn_data", agol_password = keyring::key_get(service = "AGOL", username = "mojn_data")) {
  # Get a token with a headless account
  token_resp <- httr::POST("https://nps.maps.arcgis.com/sharing/rest/generateToken",
                           body = list(username = agol_username,
                                       password = agol_password,
                                       referer = 'https://irma.nps.gov',
                                       f = 'json'),
                           encode = "form")
  agol_token <- jsonlite::fromJSON(httr::content(token_resp, type="text", encoding = "UTF-8"))
  
  agol_layers <- list()
  
  # Fetch sites table
  agol_layers$sites <- fetchAllRecords(sites_path, 0, token = agol_token$token)
  
  # Fetch lookup tables from lookup feature service
  lookup_names <- httr::GET(paste0(lookup_path, "/layers"),
                            query = list(where="1=1",
                                         outFields="*",
                                         f="JSON",
                                         token=agol_token$token))
  lookup_names <- jsonlite::fromJSON(httr::content(lookup_names, type = "text", encoding = "UTF-8"))
  lookup_names <- lookup_names$tables %>%
    dplyr::select(id, name) %>%
    dplyr::filter(grepl("MOJN_(Lookup|Ref)(_Lookup|_Ref)?_(DS|Shared)", name))  # (_Lookup|_Ref)? is to accommodate weirdly named Camera lookup - can be removed once fixed in AGOL
  
  lookup_layers <- lapply(lookup_names$id, function(id) {
    df <- fetchAllRecords(lookup_path, id, token = agol_token$token)
    return(df)
  })
  names(lookup_layers) <- lookup_names$name
  
  #Fetch calibration tables from calibration feature service
  agol_layers$CalibrationSpCond <- fetchAllRecords(calibration_path, 3, token = agol_token$token)
  
  agol_layers$CalibrationpH <- fetchAllRecords(calibration_path, 4, token = agol_token$token)
  
  agol_layers$CalibrationDO <- fetchAllRecords(calibration_path, 5, token = agol_token$token)
  
  
  # Fetch each layer in the DS feature service
  
  # ----- MOJN_DS_SpringVisit - visit-level data -----
  agol_layers$visit <- fetchAllRecords(data_path, 0, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) %>%
    dplyr::mutate(DateTime = as.POSIXct(DateTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- Repeats - repeat photos -----
  agol_layers$repeats <- fetchAllRecords(data_path, 1, token = agol_token$token, geometry = TRUE) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- InvasivePlants - invasive plant data -----
  agol_layers$invasives <- fetchAllRecords(data_path, 2, token = agol_token$token, geometry = TRUE) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- Observers -----
  agol_layers$observers <- fetchAllRecords(data_path, 3, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- SensorRetrieval -----
  agol_layers$sensor_retrieval <- fetchAllRecords(data_path, 4, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- RepeatPhotos_Internal - repeat photos taken on internal device camera -----
  agol_layers$repeats_int <- fetchAllRecords(data_path, 5, token = agol_token$token)
  
  # ----- RepeatPhotos_External - repeat photos taken on external camera -----
  agol_layers$repeats_ext <- fetchAllRecords(data_path, 6, token = agol_token$token)
  
  # ----- FillTime - volumetric discharge fill time -----
  agol_layers$fill_time <- fetchAllRecords(data_path, 7, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- FlowModTypes - flow modifications observed -----
  agol_layers$disturbance_flow_mod <- fetchAllRecords(data_path, 8, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- WildlifeRepeat - wildlife observations -----
  agol_layers$wildlife <- fetchAllRecords(data_path, 9, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- VegImageRepeat - riparian veg photo data -----
  agol_layers$riparian_veg  <- fetchAllRecords(data_path, 10, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- InternalCamera - riparian veg photos taken on internal device camera -----
  agol_layers$riparian_veg_int <- fetchAllRecords(data_path, 11, token = agol_token$token)
  
  # ----- ExternalCameraFiles - riparian veg photos taken on external camera -----
  agol_layers$riparian_veg_ext <- fetchAllRecords(data_path, 12, token = agol_token$token)
  
  # ----- InvImageRepeat - invasive veg photos taken on internal device camera -----
  agol_layers$invasives_int <- fetchAllRecords(data_path, 13, token = agol_token$token)
  
  # ----- ExternalCameraFilesInv - invasive veg photos taken on external camera -----
  agol_layers$invasives_ext <- fetchAllRecords(data_path, 14, token = agol_token$token)
  
  # ----- AdditionalPhotos2 - info about additional photos -----
  agol_layers$additional_photos <- fetchAllRecords(data_path, 15, token = agol_token$token)
  
  # ----- AdditionalPhotoInternal - additional photos taken on internal camera -----
  agol_layers$additional_photos_int <- fetchAllRecords(data_path, 16, token = agol_token$token)
  
  # ----- AddtionalPhotoExternal - additional photos taken on external camera -----
  agol_layers$additional_photos_ext <- fetchAllRecords(data_path, 17, token = agol_token$token)
  
  agol_layers <- c(agol_layers, lookup_layers)
  
  agol_layers <- lapply(agol_layers, function(data_table) {
    data_table <- data_table %>%
      dplyr::mutate(dplyr::across(where(is.character), function(x) {
        x %>%
          utf8::utf8_encode() %>%  # Encode text as UTF-8 - this prevents a lot of parsing issues in R
          trimws() %>%  # Trim leading and trailing whitespace
          dplyr::na_if("")  # Replace empty strings with NA
      }))
    col_names <- names(data_table)
    name_and_label <- grepl("(name)|(label)", col_names, ignore.case = TRUE)
    names(data_table)[name_and_label] <- tolower(names(data_table[name_and_label]))
    
    return(data_table)
  })
  
  return(agol_layers)
}

#' Fetch tabular data from AGOL
#' 
#' Retrieves tabular data from AGOL layers and tables, even when number of rows exceeds maximum record count.
#'
#' @param data_path Feature service URL
#' @param layer_number Layer number
#' @param token Authentication token (optional)
#' @param geometry Include spatial data columns? Works with points, not tested with other geometry types
#' @param where Query clause specifying a subset of rows (optional; defaults to all rows). See AGOL REST API documentation.
#' @param outFields String indicating which fields to return (optional; defaults to all fields). See AGOL REST API documentation.
#'
#' @return A tibble
#' @export
#'
fetchAllRecords <- function(data_path, layer_number, token, geometry = FALSE, where = "1=1", outFields = "*") {
  result <- tibble::tibble()
  exc_transfer <- TRUE
  offset <- nrow(result)
  
  qry <- list(where = where,
              outFields = outFields,
              f = "JSON",
              resultOffset = offset)
  
  if (!missing(token)) {
    qry$token <- token
  }
  
  while(exc_transfer) {
    resp <- httr::GET(paste0(data_path, "/", layer_number, "/query"),
                      query = qry)
    
    content <- jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"))
    
    if ("exceededTransferLimit" %in% names(content)) {
      exc_transfer <- content$exceededTransferLimit
    } else {
      exc_transfer <- FALSE
    }
    
    if (geometry) {
      partial_result <- cbind(content$features$attributes, content$features$geometry) %>%
        dplyr::mutate(wkid = content$spatialReference$wkid) %>%
        tibble::as_tibble()
    } else {
      partial_result <- tibble::as_tibble(content$features$attributes)
    }
    result <- rbind(result, partial_result)
    offset <- nrow(result)
    qry$resultOffset <- offset
  }
  return(result)
}

#' Read data from the Selected Large Springs AGOL feature layer
#' 
#' @inheritParams FetchAGOLLayers
#' 
#' @return A list of tibbles
#'
ReadAGOL <- function(data_path = c(main_db = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_SpringVisit/FeatureServer", 
                                   lookup_db = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Lookup_Database/FeatureServer", 
                                   sites_db = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_Sites_Master/FeatureServer",
                                   calibration_db = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Calibration_Database/FeatureServer"), agol_username = "mojn_data", agol_password = rstudioapi::askForPassword(paste("Please enter the password for AGOL account", agol_username))) {
  agol_layers <- FetchAGOLLayers(data_path[1], data_path[2], data_path[3], data_path[4], agol_username, agol_password)
  data <- WrangleAGOLData(agol_layers)
  
  return(data)
}

#' Get column specifications for Aquarius database
#'
#' @return A list of column specifications for each table of Aquarius data.
#'
GetAquariusColSpec <- function() {
  col.spec <- list(
    TimeseriesDischarge = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriesStage = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      Value = readr::col_double(),
      .default = readr::col_character()
    )
  )
  
  return(col.spec)
}

#' Read data from the Aquarius database
#'
#' @return A list of tibbles
#'
ReadAquarius <- function(username = "aqreadonly", password = "aqreadonly") {

  timeseries$connect("https://aquarius.nps.gov/aquarius", username, password)
  
  data <- list()
  q_data <- tibble::tibble()
  stage_data <- tibble::tibble()
  
  discharge <- c("LAKE_P_BLUE0", "PARA_P_PAKO0")
  stage <- c("LAKE_P_BLUE0", "MOJA_P_MCSP0", "PARA_P_PAKO0")
  
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
  
  data <- c(q_data, stage_data)
  
  # Tidy up the data
  data <- lapply(data, function(df) {
    df |>
      dplyr::mutate(DateTime = lubridate::ymd_hms(DateTime, tz = "America/Los_Angeles", quiet = TRUE)) |>
      dplyr::mutate(FieldSeason = ifelse(lubridate::month(DateTime) < 10,
                                         lubridate::year(DateTime),
                                         lubridate::year(DateTime) + 1))
  })
}

#' Read data from a folder of csv files
#'
#' @param data_path A path to a folder containing the data in csv format
#'
#' @return A list of tibbles
#'
ReadCSV <- function(data_path) {
  data_path <- normalizePath(data_path)
  col.spec <- GetColSpec()
  is_zip <- grepl("\\.zip", data_path, ignore.case = TRUE)
  
  if(is_zip) {
    file_list <- basename(unzip(data_path, list = TRUE)$Name)
  } else {
    file_list <- list.files(data_path)
  }
  # Make sure that files in folder are valid csv's
  expected_files <- paste0(names(col.spec), ".csv")
  if (!all(expected_files %in% file_list)) {
    missing_files <- setdiff(expected_files, file_list)
    missing_files <- paste(missing_files, collapse = "\n")
    stop(paste0("The folder provided is missing required data. Missing files:\n", missing_files))
  }
  
  # Read data
  if (is_zip) {  # Unzip into a temporary directory to read files
    temp_dir <- tempdir()
    # Use this trycatch so that even if there's an error unzipping or reading, the temp dir will be deleted
    tryCatch({
      unzip(data_path, overwrite = TRUE, exdir = temp_dir, junkpaths = TRUE)
      data <- lapply(names(col.spec), function(data.name){
        file_path <- file.path(temp_dir, paste0(data.name, ".csv"))
        df <- readr::read_csv(file = file_path, col_types = col.spec[[data.name]])
        return(df)
      })
    },
    finally = unlink(temp_dir, recursive = TRUE)
    )
  } else {  # Read files from data path
    data <- lapply(names(col.spec), function(data.name){
      file_path <- file.path(data_path, paste0(data.name, ".csv"))
      df <- readr::read_csv(file = file_path, col_types = col.spec[[data.name]])
      return(df)
    })
  }
  
  names(data) <- names(col.spec)
  return(data)
}
