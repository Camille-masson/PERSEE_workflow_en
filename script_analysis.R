
#### 0. LIBRARIES AND CONSTANTS ####
#----------------------------------#
gc()

# Loading configuration
source("config.R")

# Definition of the analysis year and the alpine pastures to process
YEAR = 2025
alpage = "Mantet"
alpages = "Mantet"

ALPAGES_TOTAL <- list(
  "9999" = c("Alpage_demo"),
  "2022" = c("Ane-et-Buyant", "Cayolle", "Combe-Madame", "Grande-Fesse", "Jas-des-Lievres", "Lanchatra", "Pelvas", "Sanguiniere", "Viso"),
  "2023" = c("Cayolle", "Crouzet", "Grande-Cabane", "Lanchatra", "Rouanette", "Sanguiniere", "Vacherie-de-Roubion", "Viso"),
  "2024" = c("Viso", "Cayolle", "Sanguiniere"),
  "2025" = c("Viso", "Cayolle", "Sanguiniere", "Ponsonniere", "Mantet")
)
ALPAGES <- ALPAGES_TOTAL[[as.character(YEAR)]]


#### 1. Simplification in GPKG ####
#----------------------------------#
if (TRUE) {  
  ## DESCRIPTION ##
  
  # Generates a .GPKG file to identify, according to the method
  # detailed in the README, the installation and removal dates of the GPS collars.
  # It therefore allows filling in the three metadata tables required to run
  # this script, namely:
  # - YYYY_colliers_poses
  # - YYYY_infos_alpages
  # - YYYY_tailles_troupeaux
  
  ## LIBRARY ##
  library(terra)
  source(file.path(functions_dir, "Functions_filtering.R"))
  
  ## INPUT ##
  # Un dossier contenant les trajectoires brutes, au format csv issu des colliers catlog rangées dans des sous-dossiers au nom de leurs alpages
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  
  ## OUTPUT ##
  # Création du sous-dossier de sortie : GPS_simple_GPKG
  gps_output_dir <- file.path(output_dir, "1. GPS_simple_GPKG")
  if (!dir.exists(gps_output_dir)) {
    dir.create(gps_output_dir, recursive = TRUE)
  }
  # Créeation du GPKG de sortie nommé : Donnees_brutes_9999_Alpage_demo_simplifiees.gpkg
  output_file <- file.path(gps_output_dir, paste0("Donnees_brutes_", YEAR, "_", alpage, "_simplifiees.gpkg"))
  
  
  ## CODE ##
  
  lapply(alpages, function(alpage) {
    collar_dir <- file.path(raw_data_dir, alpage) 
    collar_files <- list.files(collar_dir, full.names = TRUE) 
    lapply(collar_files, function(collar_f) {
      collar_ID <- substr(basename(collar_f), 1, 3)
      load_catlog_data(collar_f) %>% 
        slice(which(row_number() %% 30 == 10)) %>% 
        mutate(ID = collar_ID, date = lubridate::format_ISO8601(date)) %>% 
        vect(geom = c("lon", "lat"), crs = CRS_WSG84) 
    }) %>% do.call(rbind, .) 
  }) %>% do.call(rbind, .) %>%
    writeVector(filename = output_file, overwrite = TRUE) 
}

#### 2.1 BJONERAAS FILTER CALIBRATION ####
#----------------------------------------#
if (TRUE) {
  ## DESCRIPTION ##
  
  # Optional section to optimize the parameters of the Bjorneraas filter
  # so they match the specific characteristics of the alpine pasture to be processed.
  # This step is optional, as the initial parameters are already satisfactory.
  # If the user nevertheless wishes to fine-tune these parameters, they must modify
  # the hard-coded default values in script 2.2.
  
  # Requires:
  # - .csv | "YYYY_infos_alpes.csv"
  # - folder containing the raw data | "Colliers_YYYY_brutes"
  
  # As output, a .pdf file is generated with the results of the applied filtering:
  # outputs/2. Bjorneraas_filters/Filtering_calibration_9999_Alpine_pasture_demo
  
  ## LIBRARY ##
  source(file.path(functions_dir, "Functions_filtering.R"))
  source(file.path(functions_dir, "Functions_map_plot.R"))
  source(file.path(functions_dir, "Functions_check_metadata.R"))
  
  ## INPUTS ##
  # A folder containing the raw trajectories in CSV format from Catlog collars,
  # stored in subfolders named after their alpine pastures
  raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
  
  # A .csv file "infos_alpages" filled in according to the demo dataset template
  AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  check_and_correct_csv(csv_path = AIF)
  
  ## OPTIONAL CHECK
  # AIF_data <- read.csv(AIF, sep = ",", header = TRUE, row.names = NULL, check.names = FALSE, encoding = "UTF-8")
  # str(AIF_data)
  
  ## SORTIE ##
  # Creation of the subfolder to store the results of the Bjorneraas filter
  filter_output_dir <- file.path(output_dir, "2. Filtre_de_Bjorneraas")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  # Output PDF file for filtering visualization
  pdf(file.path(filter_output_dir, paste0("Filtering_calibration_", YEAR, "_", alpage, ".pdf")), width = 9, height = 9)
  
  
  ## CODE ##
  
  # List of raw data files for the alpine pasture
  files <- list.files(file.path(raw_data_dir, alpage), full.names = TRUE)
  files <- files[1:3]  # Selection of the first three files (to avoid memory overload)

  # Loading and concatenation of the selected files' data
  data <- do.call(rbind, lapply(files, function(file) { 
    data <- load_catlog_data(file)
    data$ID <- file  
    return(data) 
  }))
  
  # Retrieval of collar installation and removal dates
  beg_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_pose"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
  end_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_retrait"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
  data = date_filter(data, beg_date, end_date) 
  
  # Projection of the data into Lambert93 (EPSG:2154) from WGS84 (EPSG:4326)
  data_xy <- data %>%
    terra::vect(crs="EPSG:4326") %>%
    terra::project("EPSG:2154") %>%
    as.data.frame(geom = "XY")
  
  # Histogram of time intervals between GPS points
  temps <- diff(data_xy$date)
  temps <- as.numeric(temps, units = "mins")
  hist(temps, nclass = 30)
  
  # Histogram of distances traveled between two successive positions
  dist <- sqrt(diff(data_xy$x)^2+diff(data_xy$y)^2)
  h <- hist(dist, nclass = 30, xlab='Distance (m)', xaxt="n")
  
  ### Testing different filters
  # Definition of test values for the Bjorneraas filter
  medcrits = c(750, 500, 750) 
  meancrits = c(500, 500, 350) 
  spikesps = c(1500, 1500, 1500) 
  spikecoss = c(-0.95, -0.95, -0.95) 
  
  for (i in 1:length(medcrits)) {
    # Application of the Bjorneraas filter with each parameter combination
    trajectories <- position_filter(data, medcrit=medcrits[i], meancrit=meancrits[i], spikesp=spikesps[i], spikecos=spikecoss[i])
    
    # Determination of the spatial boundaries of the map
    minmax_xy = get_minmax_L93(trajectories[!(trajectories$R1error | trajectories$R2error ),], buffer = 100)
    
    # Assignment of error codes for visualization
    trajectories$errors = 1
    trajectories$errors[trajectories$R1error] = 2
    trajectories$errors[trajectories$R2error] = 3
    
    # Definition of the color palette for the map
    pal <- c("#56B4E9", "red", "black")
    
    # Display of GPS trajectories with detected errors
    print(ggplot(trajectories, aes(x, y, col = errors)) +
            geom_path(size = 0.2) +
            geom_point(size = 0.3) +
            coord_equal() +
            xlim(minmax_xy$x_min, minmax_xy$x_max) + ylim(minmax_xy$y_min, minmax_xy$y_max) +
            ggtitle(paste0("medcrit = ", medcrits[i], ", meancrit = ", meancrits[i], ", spikesp = ", spikesps[i], ", spikecos = ", spikecoss[i])) +
            scale_colour_gradientn(colors=pal, guide="legend", breaks = c(1, 2, 3), labels = c("OK", "R1error", "R2error")))
    
    # Zoom on the first five days after installation
    trajectories <- trajectories %>%
      filter(date < beg_date + 3600*24*5)
    print(ggplot(trajectories, aes(x, y, col = errors)) +
            geom_path(size = 0.2) +
            geom_point(size = 0.3) +
            coord_equal() +
            ggtitle(paste0("5 days only, medcrit = ", medcrits[i], ", meancrit = ", meancrits[i], ", spikesp = ", spikesps[i], ", spikecos = ", spikecoss[i])) +
            scale_colour_gradientn(colors=pal, guide="legend", breaks = c(1, 2, 3), labels = c("OK", "R1error", "R2error")))
  }
  
  # Closing the PDF file containing the visualizations
  dev.off()
}

#### 2.2 FILTERING CATLOG DATA ####
#---------------------------------#
if (TRUE) {
  ## DESCRIPTION ##
  
  # Filtering of raw GPS data using the Bjorneraas filter,
  # in order to obtain cleaned data ready for the next steps.
  
  # Requires:
  # - .csv : "YYYY_infos_alpages.csv"
  # - .csv : "YYYY_colliers_poses.csv"
  # - folder containing the raw data: "Colliers_YYYY_brutes"
  
  # Output:
  # - .rds file containing the filtered data:
  #   outputs/2. Bjorneraas_filters/Catlog_9999_filtered_alpage_demo.rds
  # - .csv file containing collar performance:
  #   outputs/2. Bjorneraas_filters/9999_filtering_alpages.csv
  
  
  ## LIBRARY ##
  source(file.path(functions_dir, "Functions_filtering.R"))
  source(file.path(functions_dir, "Functions_check_metadata.R"))
  
  ## INPUTS ##
  # A folder containing the raw trajectories
  raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
  
  # A .CSV file "Colliers_poses"
  IIF = file.path(raw_data_dir, paste0(YEAR,"_colliers_poses.csv"))
  check_and_correct_csv(IIF)
  
  # OPTONIAL CKECK
  #str(read.csv(IIF, stringsAsFactors = FALSE, encoding = "UTF-8"))
  
  # A .csv file "infos_alpages" filled in according to the demo dataset template
  AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  check_and_correct_csv(csv_path = AIF)
  # OPTONIAL CKECK
  #str(read.csv(AIF, stringsAsFactors = FALSE, encoding = "UTF-8"))
  
  ## OUTPUTS ##
  filter_output_dir <- file.path(output_dir, "2. Filtre_de_Bjorneraas")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  # An .RDS file containing the filtered trajectories
  output_rds_file = file.path(filter_output_dir, paste0("Catlog_",YEAR,"_filtered_",alpages,".rds"))
  
  # A .csv file containing the collar performance
  indicator_file = file.path(filter_output_dir, paste0(YEAR,"_filtering_",alpages,".csv"))
  
  ## CODE ##
  for (alpage in alpages) {
    print(paste("WORKING ON ALPAGE :", alpage))
    collar_dir <- file.path(raw_data_dir, alpage)
    collar_files <- list.files(collar_dir, pattern = ".csv", full.names = TRUE)
    
    # Optional (advanced users wishing to adapt the parameters to each alpine pasture):
    # fill in the CSV file "infos_alpages" with the correct parameters.
    # medcrit = get_alpage_info(alpage, AIF, "medcrit")
    # meancrit = get_alpage_info(alpage, AIF, "meancrit")
    # spikesp = get_alpage_info(alpage, AIF, "spikesp")
    # spikecos = as.numeric(gsub(",", ".", get_alpage_info(alpage, AIF, "spikecos")))

    
    medcrit = 750
    meancrit = 500
    spikesp = 1500
    spikecos = -0.95
    print(paste0("Bjorneraas filter parameters: medcrit=",medcrit,", meancrit=", meancrit, ", spikesp=", spikesp, ", spikecos=", spikecos))
    print(collar_files)
    
    
    # Filtering of trajectories and calculation of indicators
    indicators <- lapply(collar_files, function(collar) {
      filter_one_collar(
        load_catlog_data(collar),  
        basename(collar), 
        output_rds_file, alpage, beg_date, end_date, IIF,
        bjoneraas.medcrit = medcrit,
        bjoneraas.meancrit = meancrit,
        bjoneraas.spikesp = spikesp,
        bjoneraas.spikecos = spikecos
      )
    }) %>%
      do.call(rbind, .)
    
    indicators_tot = indicators %>%
      filter(worked_until_end == 1) %>% 
      add_row(name = paste("TOTAL", alpage), worked_until_end = sum(.$worked_until_end), nloc = NA,
              R1error = NA, R2error = NA,
              error_perc = sum(.$nloc*.$error_perc)/sum(.$nloc), localisation_rate = mean(.$localisation_rate))
    indicators = rbind(indicators, indicators_tot[nrow(indicators_tot),])
    
    write.table(indicators, file=indicator_file, append = T, sep=',', row.names=F, col.names=F)
  }
}

#### 3. HMM FITTING #### 
#----------------------#
if (TRUE) {
  ## DESCRIPTION ##
  # Trajectories characterized by the "HMM" behavior:
  # - Rest
  # - Movement
  # - Grazing
  #
  # Requires:
  # - .csv : "YYYY_infos_alpages.csv"
  # - .csv : "YYYY_colliers_poses.csv"
  # - .rds file containing the filtered data: "Catlog_YYYY_filtered_alpage_demo.rds"
  #
  # Output:
  # - An .rds file containing the data annotated with behaviors:
  #   outputs/3. HMM_Behavior/Catlog_YYYY_alpage_demo_viterbi.rds
  # - A .pdf file containing the trajectories categorized by behavior:
  #   outputs/3. HMM_Behavior/Output_PDF_YYYY_alpage_demo
  
  
  ## LIBRARY ##
  library(snow)
  library(stats)
  library(momentuHMM)
  library(adehabitatLT)
  library(adehabitatHR)
  library(knitr)
  library(rmarkdown)
  source(file.path(functions_dir, "Functions_HMM_fitting.R"))
  
  ## INPUTS ##
  # An .RDS file containing the trajectories filtered in 2.2
  input_rds_file <- file.path(output_dir, "2. Filtre_de_Bjorneraas", paste0("Catlog_", YEAR, "_filtered_", alpage,".rds"))
  
  # A data.frame containing the correspondence between collars and alpine pastures
  individual_info_file <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"), paste0(YEAR, "_colliers_poses.csv"))
  
  # OPTONIAL CKECK
  #str(read.csv(individual_info_file, stringsAsFactors = FALSE, encoding = "UTF-8"))
  
  
  ## OUTPUTS ##
  # Creation of the subfolder to store the results of the Bjorneraas filter
  filter_output_dir <- file.path(output_dir, "3. HMM_comportement")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  # Creation of the subfolder to store the individual results as a PDF
  hmm_pdf_case <- file.path(filter_output_dir, paste0("Output_PDF_", YEAR, "_",alpage))
  if (!dir.exists(hmm_pdf_case)) {
    dir.create(hmm_pdf_case, recursive = TRUE)
  }
  
  # An .RDS file containing the trajectories categorized by behavior 
  # (new trajectories are appended to the previously processed ones)
  output_rds_file = file.path(output_dir, "3. HMM_comportement", paste0("Catlog_",YEAR,"_",alpage,"_viterbi.rds"))
  
  
  
  ## CODE ##
  
  data = readRDS(input_rds_file)
  
  run_parameters = list(
    # Model
    model = "HMM",
    
    # Resampling
    resampling_ratio = 5,
    resampling_first_index = 0,
    rollavg = FALSE,
    rollavg_convolution = c(0.15, 0.7, 0.15),
    knownRestingStates = FALSE,
    
    # Observation distributions (step lengths and turning angles)
    dist = list(step = "gamma", angle = "vm"),
    # Design matrices to be used for the probability distribution parameters of each data stream
    DM = list(angle=list(mean = ~1, concentration = ~1)),
    # Covariants formula
    covariants = ~cos(hour*3.141593/12), # ~1 if no covariants used
    
    # 3-state HMM
    Par0 = list(step = c(10, 25, 50, 10, 15, 40), angle = c(tan(pi/2), tan(0/2), tan(0/2), log(0.5), log(0.5), log(3))),
    fixPar = list(angle = c(tan(pi/2), tan(0/2), tan(0/2), NA, NA, NA))
  )
  run_parameters = scale_step_parameters_to_resampling_ratio(run_parameters)
  
  # Check the internet connection
  startTime = Sys.time()
  results = par_HMM_fit(data, run_parameters, ncores = ncores, individual_info_file, sampling_period = 120, output_dir = hmm_pdf_case)
  endTime = Sys.time()
   
  data_hmm <- do.call("rbind", lapply(results, function(result) result$data))
  viterbi_trajectory_to_rds(data_hmm, output_rds_file, individual_info_file)
}

#### 4. FLOCK STOCKING RATE (charge) BY DAY AND BY STATE ####
#-------------------------------------------------------------#
if (TRUE){
  
  ## DESCRIPTION ##
  # Calculation of the stocking rate using the "BBMM" method
  #
  # Requires:
  # - .csv : "YYYY_infos_alpages.csv"
  # - .rds file containing the behavior data: "Catlog_YYYY_alpage_demo_viterbi.rds"
  #
  # Output:
  # - An .rds file with stocking by day and by behavior:
  #   outputs/4. Calculated_stocking/by_day_and_state_YYYY_alpage_demo
  # - An .rds file with stocking by day:
  #   outputs/4. Calculated_stocking/by_day_YYYY_alpage_demo
  # - An .rds file with stocking by behavior:
  #   outputs/4. Calculated_stocking/by_state_YYYY_alpage_demo
  # - An .rds file with total stocking:
  #   outputs/4. Calculated_stocking/total_YYYY_alpage_demo
  
  
  ## LIBRARY ##
  
  library(adehabitatHR)
  library(data.table)
  library(snow)
  source(file.path(functions_dir, "Functions_map_plot.R"))
  source(file.path(functions_dir, "Functions_flock_density.R"))
  
  ## INPUTS ##
  
  # An .RDS file containing the trajectories categorized by behavior
  input_rds_file <- file.path(output_dir, "3. HMM_comportement",  paste0("Catlog_", YEAR, "_", alpage, "_viterbi.rds"))

  # A .csv file "infos_alpages" filled in according to the demo dataset template
  AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  check_and_correct_csv(csv_path = AIF)
  
  # A data.frame containing herd sizes and their changes over time based on the date
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  flock_size_file <- file.path(raw_data_dir, paste0(YEAR, "_tailles_troupeaux.csv"))
  check_and_correct_csv(csv_path = flock_size_file)
  
  ## OPTIONAL CHECK
  #str(read.csv(flock_size_file, stringsAsFactors = FALSE, encoding = "UTF-8"))
  
  
  ## OUTPUTS ##
  # Output folder
  
  save_dir <- file.path(output_dir, "4. Chargements_calcules")
  
  # One .RDS per alpine pasture containing the daily loads by behavior
  state_daily_rds_prefix <- paste0("by_day_and_state_", YEAR, "_")
  # One .RDS per alpine pasture containing the daily loads
  daily_rds_prefix <- paste0("by_day_", YEAR, "_")
  # One .RDS per alpine pasture containing the loads by behavior
  state_rds_prefix <- paste0("by_state_", YEAR, "_")
  # One .RDS per alpine pasture containing the total load over the entire season
  
  total_rds_prefix <- paste0("total_", YEAR, "_")
  
  
  ## CODE ##
  h <- 25 # Characteristic distance for calculating stocking

  for (alpage in alpages) {
    flock_sizes <- get_flock_size_through_time(alpage, flock_size_file)
    prop_time_collar_on <- get_alpage_info(alpage, AIF, "proportion_jour_allume")
    
    # Loading the filtered data for the alpine pasture
      data <- readRDS(input_rds_file)
    data <- data[data$alpage == alpage,]
    
    if(FALSE){
    # Loading the phenology raster with the correct path
    raster_file <- file.path(raster_dir, paste0("ndvis_", YEAR,"_",alpage, "_pheno_metrics.tif"))
    pheno_t0 <- get_raster_cropped_L93(raster_file, get_minmax_L93(data, 100), reproject = TRUE, band = 2, as = "SpatialPixelDataFrame")
    }
    
    # Definition of the storage folder specific to the alpine pasture
    alpage_save_dir <- file.path(save_dir, paste0(YEAR, "_", alpage))
    if (!dir.exists(alpage_save_dir)) dir.create(alpage_save_dir, recursive = TRUE)
    
    
    # BY day and by state 
    # Calculation of stocking based on an automatic grid (pixelization)
    if(TRUE){
    flock_load_by_day_and_state_to_rds_kernelbb_Auto_grid(data, alpage_save_dir,state_daily_rds_prefix, flock_sizes,prop_time_collar_on)
    }
    
    # Calculation of stocking based on the NDVI raster (unsuitable for other users)
    if(FALSE){
    flock_load_by_day_and_state_to_rds_kernelbb_NDVI_grid(data, grid, save_dir, save_rds_name, flock_sizes, prop_time_collar_on)
    }
    
    # Merging individual files
    merged_file <- flock_merge_rds_files(alpage_save_dir, state_daily_rds_prefix)
    
    rm(data)
    
    charge <- readRDS(file.path(alpage_save_dir, paste0(state_daily_rds_prefix, alpage, ".rds")))
    unique(charge$state)
    
    # By state
    charge_state <- charge %>%
      group_by(x, y, state) %>%
      summarise(Charge = sum(Charge, na.rm = TRUE), .groups = 'drop') %>%
      as.data.frame()
    saveRDS(charge_state, file.path(alpage_save_dir, paste0(state_rds_prefix, alpage, ".rds")))
    rm(charge_state)
    
    # By day
    charge_day <- lapply(unique(charge$day), function(d) {
      charge %>%
        filter(day == d) %>%
        group_by(x, y, day) %>%
        summarise(Charge = sum(Charge, na.rm = TRUE), .groups = 'drop')
    })
    charge_day <- as.data.frame(rbindlist(charge_day, use.names = TRUE))
    saveRDS(charge_day, file.path(alpage_save_dir, paste0(daily_rds_prefix, alpage, ".rds")))
    rm(charge_day)
    
    # Total
    charge_tot <- charge %>%
      group_by(x, y) %>%
      summarise(Charge = sum(Charge, na.rm = TRUE), .groups = 'drop') %>%
      as.data.frame()
    saveRDS(charge_tot, file.path(alpage_save_dir, paste0(total_rds_prefix, alpage, ".rds")))
    rm(charge_tot)
    
    rm(charge)
    
    
    
    }



}




# BONUS
{

################# BONUS #####################
#-------------------------------------------#






#### 5. Extraction des raster CHARGEMENT ####
#-------------------------------------------#
if (TRUE) {
  # Exctraction des raster au format tif 
  # Génération de différent tif : 
  # - Chargement total
  # - Chargement par comportement
  # - Chargement par jour 
  
  # LIBRARY & FUNCTION
  library(raster)
  library(sf)
  library(ggplot2)
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  for(alpage in alpages){
    # ENTREE
    #Dossier contenant les sous dossier des chargement
    case_flock_file = file.path(output_dir, "4. Chargements_Calcules")
    #Dossier contenant les fichiers du tot de chargement
    case_flock_alpage_file = file.path(case_flock_file,paste0(YEAR,"_",alpage))
    
    # Un .RDS par alpage contenant les charges journalières
    daily_rds_prefix = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
    daily_rds_file = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
    # Un .RDS par alpage contenant les charges par comportement
    state_rds_prefix = file.path(case_flock_alpage_file, paste0("by_state_",YEAR,"_",alpage,".rds"))
    # Un .RDS par alpage contenant la charge totale sur toute la saison
    total_rds_prefix = file.path(case_flock_alpage_file, paste0("total_",YEAR,"_",alpage,".rds"))
    
    # Un dossier contenant les ratsers des Unités Pastorales (UP)
    case_UP_file = file.path(raster_dir, "UP")
    # Un .SHP avec les Unités pastorales UP
    UP_file = file.path(case_UP_file, "v1_bd_shape_up_inra_2012_2014_2154_all_emprise.shp")
    
    # Un dossier contenant les Infos sur les alpages
    raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
    # Un data.frame contenant les dates de pose et de retrait des colliers
    alpage_info_file <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
    
    
    # SORTIE 
    
    #Création du dossier de sortie des indicateur pour la visualistaion
    output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
    if (!dir.exists(output_visu_case)) {
      dir.create(output_visu_case, recursive = TRUE)
    }
    #Création du sous-dossier Indicateur traitée : Chargement
    output_chargement_case <- file.path(output_visu_case, "Taux_chargement")
    if (!dir.exists(output_chargement_case)) {
      dir.create(output_chargement_case, recursive = TRUE)
    }
    #Création du sous-sous-dossier alpage et années traitée : Chargement
    output_case_alpage <- file.path(output_chargement_case, paste0(YEAR,"_",alpage))
    if (!dir.exists(output_case_alpage)) {
      dir.create(output_case_alpage, recursive = TRUE)
    }
    
    # Un .TIF par alpage contenant les charges journalières
    output_flock_daily_tif = file.path(output_case_alpage, paste0("by_day_and_state_",YEAR,"_",alpage,".tif"))
    output_flock_daily_tif_crop = file.path(output_case_alpage, paste0("by_day_and_state_",YEAR,"_",alpage,"_crop.tif"))
    # Un .TIF par alpage contenant les charges par comportement
    output_flock_repos_tif = file.path(output_case_alpage, paste0("repos",YEAR,"_",alpage,".tif"))
    output_flock_deplacement_tif = file.path(output_case_alpage, paste0("deplacement",YEAR,"_",alpage,".tif"))
    output_flock_paturage_tif = file.path(output_case_alpage, paste0("paturage",YEAR,"_",alpage,".tif"))
    output_flock_repos_tif_crop = file.path(output_case_alpage, paste0("repos",YEAR,"_",alpage,"_crop.tif"))
    output_flock_deplacement_tif_crop = file.path(output_case_alpage, paste0("deplacement",YEAR,"_",alpage,"_crop.tif"))
    output_flock_paturage_tif_crop = file.path(output_case_alpage, paste0("paturage",YEAR,"_",alpage,"_crop.tif"))
    # Un .TIF par alpage contenant la charge totale sur toute la saison
    output_flock_tot_tif = file.path(output_case_alpage, paste0("total_",YEAR,"_",alpage,".tif"))
    output_flock_tot_tif_crop = file.path(output_case_alpage, paste0("total_",YEAR,"_",alpage,"_crop.tif"))
    
    # Un .tif par alpage avec le chargement median de toute les années disponible.
    output_flock_med_tif = file.path(output_case_alpage, paste0("total_med_",alpage,".tif"))
    
    # CODE
    
    #Indicateur : Charge total .TIF
    if (T) {
      total_flock_load_tif(total_rds_prefix, output_flock_tot_tif, output_flock_tot_tif_crop, UP_file, alpage, alpage_info_file)
    }
    
    
    #Indicateur : Charge_by_state
    if (T) {
      state_flock_load_tif(state_rds_prefix,output_flock_repos_tif,output_flock_deplacement_tif, output_flock_paturage_tif,
                           output_flock_repos_tif_crop, output_flock_deplacement_tif_crop , output_flock_paturage_tif_crop,
                           UP_file, alpage, alpage_info_file, CROP = "NO")
    }
    
    #Indicateur : Charge_by_day
    if (F){
      res_raster <- 10 # ou la valeur que tu souhaites explicitement
      day_flock_load_tif_nostack(daily_rds_prefix, output_case_alpage, UP_file, alpage, alpage_info_file, YEAR, res_raster, CROP = "NO")
      
      
    }
    
    if (FALSE){
      # Par quinzaine
      quinzaine_flock_load_tif_nostack(daily_rds_file = daily_rds_file,
                                       output_case_alpage = output_case_alpage,
                                       UP_file = UP_file,
                                       alpage = alpage,
                                       alpage_info_file = alpage_info_file,
                                       YEAR = YEAR,
                                       res_raster = 10,
                                       CROP = "NO")
      
      
    if (F){
      
      
      res <- charge_median(case_flock_file, alpage)
      
      
      median_flock_to_tif(case_flock_file, alpage, output_case_alpage)
      
    }
      
      
      
    }
    
  }
  
}
  

#### 6. Calcul du nombre de jour paturé ####
#-------------------------------------------#
if (TRUE) {
  
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  
  # ENTREE
  #Dossier contenant les sous dossier des chargement
  case_flock_file = file.path(output_dir, "4. Chargements_Calcules")
  #Dossier contenant les fichiers du tot de chargement
  case_flock_alpage_file = file.path(case_flock_file,paste0(YEAR,"_",alpage))
  
  # Un .RDS par alpage contenant les charges journalières
  daily_rds_file = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
  
  # OUTPUT
  
  #Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  #Création du sous-dossier Indicateur traitée : Chargement
  output_day_grazing_case <- file.path(output_visu_case, "Number_Grazing_day")
  if (!dir.exists(output_day_grazing_case)) {
    dir.create(output_day_grazing_case, recursive = TRUE)
  }
  #Création du sous-sous-dossier alpage et années traitée : Chargement
  output_case_alpage <- file.path(output_day_grazing_case, paste0(YEAR,"_",alpage))
  if (!dir.exists(output_case_alpage)) {
    dir.create(output_case_alpage, recursive = TRUE)
  }
  
  # Un .TIF par alpage contenant les charges journalières
  output_nb_grazing_day_tif = file.path(output_case_alpage, paste0("number_grazing_day_",YEAR,"_",alpage,".tif"))
  
  
  ## CODE
  
  nb_grazing_day(daily_rds_file, output_nb_grazing_day_tif)
  
  
  
  
  
  
  
  
  
  
}
  
  
  
  
  
  
####

#### 6. Calcul de la distance et du denivelé ####
#-----------------------------------------------#
if (FALSE) {
  # Calcul de la distance et du denivelé par jour sur l'ensemble des colliers
  # Le tout stocké par alpage et par année souys forme d'un csv
  # Attention il faut le MNT
  
  
  
  library(dplyr)
  library(lubridate)
  library(terra)
  library(moveHMM) 
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  for(alpage in alpages){
    # ENTREES
    
    # Dossier contenant les fichiers du comportement
    case_state_file = file.path(output_dir, "3. HMM_comportement")
    # Un .RDS contenant les trajectoires (filtrées, éventuellement sous-échantillonnées)
    state_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi.rds"))
    
    # Un raster d’altitude
    altitude_raster = file.path(raster_dir,"BDALTI.tif")
    
    
    # SORTIE 
    
    # Création du dossier de sortie des indicateur pour la visualistaion
    output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
    if (!dir.exists(output_visu_case)) {
      dir.create(output_visu_case, recursive = TRUE)
    }
    # Création du sous-dossier Indicateur traitée : Chargement
    output_distance_case <- file.path(output_visu_case, "Distance_&_Denivele")
    if (!dir.exists(output_distance_case )) {
      dir.create(output_distance_case , recursive = TRUE)}
    
    # Un .csv avec la distance et le denivelé par jour par collier
    distance_csv_file <- file.path(output_distance_case, paste0(YEAR,"_",alpage,"_distance_denivele.csv"))
    
    #CODE 
    
    save_distance_denivele(state_rds_file, distance_csv_file , altitude_raster, output_distance_case, YEAR, alpage)
    
  }
  
  
}

#### 7. Calcul date de pature ####
#--------------------------------#
if (TRUE) {
  # Calcul de la date de mise en pature de chaque espace pixel par pixel lorsque
  # le chargement dépasse un seuil de paturage on concidère la mise en pature du
  # pixel ainsi le jour julien et noté pour chaque pixel 
  
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  
  # ENTREE
  #Dossier contenant les sous dossier des chargement
  case_flock_file = file.path(output_dir, "4. Chargements_Calcules")
  #Dossier contenant les fichiers du tot de chargement
  case_flock_alpage_file = file.path(case_flock_file,paste0(YEAR,"_",alpage))
  
  # Un .RDS par alpage contenant les charges journalières
  daily_rds_prefix = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
  
  
  # SORTIE 
  #Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  #Création du sous-dossier Indicateur traitée : Espace paturé
  output_espace_pature_case <- file.path(output_visu_case, "Espace_pature")
  if (!dir.exists(output_espace_pature_case)) {
    dir.create(output_espace_pature_case, recursive = TRUE)
  }
  #Création du sous-sous-dossier alpage et années traitée : Chargement
  output_case_alpage <- file.path(output_espace_pature_case, paste0(YEAR,"_",alpage))
  if (!dir.exists(output_case_alpage)) {
    dir.create(output_case_alpage, recursive = TRUE)
  }
  
  # Un .rds avec le jour de déblocage de la zone paturée
  output_new_grazed_area_rds = file.path(output_case_alpage, paste0("new_grazed_area_",YEAR,"_",alpage,".rds"))
  # Un .TIF avec le jour de déblocage de la zone paturée
  output_new_grazed_area_tif = file.path(output_case_alpage, paste0("new_grazed_area_",YEAR,"_",alpage,".tif"))
  
  # Un .rds avec le nombre de jour dont le pixel est paturée
  output_nb_grazing_rds = file.path(output_case_alpage, paste0("nb_grazing_day_",YEAR,"_",alpage,".rds"))
  # Un .TIF avec le nombre de jour dont le pixel est paturée
  output_nb_grazing_tif = file.path(output_case_alpage, paste0("nb_grazing_day_test",YEAR,"_",alpage,".tif"))
  
  #CODE
  
  
  
  if(FALSE){
    generate_new_grazed_area(daily_rds_prefix, output_new_grazed_area_rds, output_new_grazed_area_tif, threshold = 1 ) #Valeur de filtre du chargement
  }
  
  
  if(TRUE){
    generate_new_grazed_area_by_day(daily_rds_prefix,output_case_alpage,YEAR,alpage,threshold = 1,crs_string = "+init=epsg:2154")
  }
  
  
  
}

#### 8. Vecteur du comportement ####
#----------------------------------#  
if (TRUE){
  #Library
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  library(sf)
  library(dplyr)
  TYPE = "catlog"
  
  
  for(alpage in alpages){
    
    # ENTREES
    # Dossier contenant les fichiers du comportement
    case_state_file = file.path(output_dir, "3. HMM_comportement")
    # Un .RDS contenant les trajectoires (filtrées, éventuellement sous-échantillonnées)
    state_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi.rds"))
    data = readRDS(state_rds_file)
    
    
    if (TYPE == "ofb"){
      # Charger le fichier des périodes d'échantillonnage
      sampling_period_file <- file.path(output_dir, "0. Sampling_Periods", paste0("Sampling_Periods_", YEAR, "_", alpage, ".rds"))
      sampling_periods <- readRDS(sampling_period_file)
    }
    
    # SORTIE 
    # Création du dossier de sortie des indicateur pour la visualistaion
    output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
    if (!dir.exists(output_visu_case)) {
      dir.create(output_visu_case, recursive = TRUE)
    }
    # Création du sous-dossier Indicateur traitée : Espace paturé
    output_state_traj_case <- file.path(output_visu_case, "Trajectoiree_par_comportement")
    if (!dir.exists(output_state_traj_case)) {
      dir.create(output_state_traj_case, recursive = TRUE)
    }
    
    # Un .shp contenant les données de trajectoires catégorisées par comportement et collier
    # Généré autaumatiquement dans la fonctions et déposé dans le dossier si dessus
    
    
    
    # CODE
    if (TYPE == "ofb"){
      generate_trajectory_gpkg_ofb(state_rds_file, output_state_traj_case, YEAR, alpage, sampling_interval = 10, sampling_periods = sampling_periods)
    }
    
    if (TYPE == "catlog"){
      generate_trajectory_gpkg_catlog(state_rds_file,output_state_traj_case,YEAR,alpage,sampling_interval = 60)#Point toute les 30 minutes (reglé de base a 10)
    }
    
  }
  
  
  
  
}

#### 9. Polygon d'utilisation tout les 15 jours ####
#--------------------------------------------------#  
if (TRUE){
  
  library(sf)
  library(dplyr)
  library(viridis)
  library(ggplot2)
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  for (alpage in alpages){
    # ENTREES
    # Dossier contenant les fichiers du comportement
    case_state_file = file.path(output_dir, "3. HMM_comportement")
    # Un .RDS contenant les trajectoires (filtrées, éventuellement sous-échantillonnées)
    state_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi.rds"))
    
    
    # Un dossier contenant les ratsers des Unités Pastorales (UP)
    case_UP_file = file.path(raster_dir, "UP")
    # Un .SHP avec les Unités pastorales UP
    UP_file = file.path(case_UP_file, "v1_bd_shape_up_inra_2012_2014_2154_all_emprise.shp")
    
    # Un dossier contenant les Infos sur les alpages
    raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
    # Un data.frame contenant les dates de pose et de retrait des colliers
    AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
    
    # SORTIE 
    # Création du dossier de sortie des indicateur pour la visualistaion
    output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
    if (!dir.exists(output_visu_case)) {
      dir.create(output_visu_case, recursive = TRUE)
    }
    # Création du sous-dossier Indicateur traitée : Espace paturé
    output_polygon_case <- file.path(output_visu_case, "Utilisation_par_quinzaine")
    if (!dir.exists(output_polygon_case)) {
      dir.create(output_polygon_case, recursive = TRUE)
    }
    
    # Un .shp contenant les données de trajectoires catégorisées par comportement et collier
    output_polygon_use_shp = file.path(output_polygon_case, paste0("Use_polygon_",YEAR,"_",alpage,".shp"))
    
    
    # CODE 
    # Ancienne fonction
    if (FALSE){
      generate_presence_polygons(state_rds_file,output_polygon_use_shp,YEAR,alpage,density_threshold = 1e-06,n_grid = 200,small_poly_threshold_percent = 0.05)
      # Seuil pour filtrer les petits polygones
      # Ajustez pour être plus ou moins restrictif
    }
    
    if(TRUE){
      generate_presence_polygons_by_percentage(state_rds_file, output_polygon_use_shp, YEAR, alpage, percentage = 0.85 ,n_grid = 200,small_poly_threshold_percent = 0.05 ,crs = 2154 )
      
      
    }
    
    
    if (TRUE){
      generate_presence_polygons_by_percentage_per_month(state_rds_file, output_polygon_use_shp, YEAR, alpage, percentage = 0.85 ,n_grid = 200,small_poly_threshold_percent = 0.05 ,crs = 2154 )  
    } 
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
}

#### 10. Carto et fond de carte ####
#---------------------------------#
if (FALSE){
  #LIBRARY
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  
  #ENTREE
  # Un dossier contenant les ratsers des Unités Pastorales (UP)
  case_UP_file = file.path(raster_dir, "UP")
  # Un .SHP avec les Unités pastorales UP
  UP_file_mask = file.path(case_UP_file, paste0("UP_",alpage,"_bis1.shp"))
  
  # Un Ratser hill de la zone d'étude
  hillShade_file = file.path(raster_dir, paste0("hillshade_",alpage,".tif"))
  
  
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_carto_case <- file.path(output_dir, "6. Carto_Fond_Carte")
  if (!dir.exists(output_carto_case)) {
    dir.create(output_carto_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_hill_case <- file.path(output_carto_case, "Hillshade")
  if (!dir.exists(output_hill_case)) {
    dir.create(output_hill_case, recursive = TRUE)
  }
  
  # Un .shp contenant les données de trajectoires catégorisées par comportement et collier
  output_hill_shade_tif = file.path(output_hill_case, paste0("Hillshade_",alpage,"_crop_bis3.tif"))
  
  #CODE
  
  crop_hillshape(UP_file_mask, hillShade_file, output_hill_shade_tif)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

#### 11. Création de GIF ####
#--------------------------#
if (FALSE){
  #LIBRARY
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  #ENTREE
  # Un dossier contenant les ratsers des Unités Pastorales (UP)
  case_GIF_file = file.path(raster_dir, "Image_GIF")
  
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_Plot_Animation_case <- file.path(output_dir, "7. Plot_et_Animation")
  if (!dir.exists(output_Plot_Animation_case)) {
    dir.create(output_Plot_Animation_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_GIF_case <- file.path(output_Plot_Animation_case, "GIF")
  if (!dir.exists(output_GIF_case)) {
    dir.create(output_GIF_case, recursive = TRUE)
  }
  
  # Un .GIF contenant les données de trajectoires catégorisées par comportement et collier
  output_gif = file.path(output_GIF_case, "Gif_Cayolle_quinz_2022.gif")
  
  # CODE
  
  if(TRUE){
    create_gif_from_images(case_GIF_file,output_gif,file_pattern = "Carte_cayolle.*\\.(png|jpg)$",delay_seconds = 1.25)
  }
  
  if(TRUE){
    create_gif_from_images_day_bis (case_GIF_file,output_gif,file_pattern = "Carte_cayolle.*\\.(png|jpg)$",delay_seconds = 0.5)
  }
  
  
  
}

#### 12. Graph Dénivelé et distance ####
#-------------------------------------#
if (FALSE){
  #LIBRARY
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  #ENTREE
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Chargement
  output_distance_case <- file.path(output_visu_case, "Distance_&_Denivele")
  if (!dir.exists(output_distance_case )) {
    dir.create(output_distance_case , recursive = TRUE)}
  
  # Un .csv avec la distance et le denivelé par jour par collier
  distance_csv_file = file.path(output_distance_case, paste0(YEAR,"_",alpage,"_distance_denivele.csv"))
  
  
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_Plot_Animation_case <- file.path(output_dir, "7. Plot_et_Animation")
  if (!dir.exists(output_Plot_Animation_case)) {
    dir.create(output_Plot_Animation_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_dit_deniv_case <- file.path(output_Plot_Animation_case, "Distance_&_Denivele")
  if (!dir.exists(output_dit_deniv_case)) {
    dir.create(output_dit_deniv_case, recursive = TRUE)
  }
  
  # Un .GIF contenant les données de trajectoires catégorisées par comportement et collier
  plot_Dist_Deniv_jpg = file.path(output_dit_deniv_case, paste0("plot_dist_denic_",YEAR,"_",alpage,".jpeg"))
  
  
  
  data = read.csv(file = distance_csv_file, sep = ",")
  
  
  library(ggplot2)
  library(scales)
  
  library(ggplot2)
  
  # Supposons que vos données s'appellent 'data' et contiennent :
  # data$date, data$distance, data$denivelation
  # Assurez-vous que data$date est bien de type Date
  # data$date <- as.Date(data$date, format = "%Y-%m-%d")
  
  library(ggplot2)
  library(dplyr)
  
  # Assurez-vous que data$date est bien au format Date et qu'il n'y a pas de NA
  # data$date <- as.Date(data$date, format = "%Y-%m-%d")
  
  # Filtrer les dates sans valeur (distance ou dénivelé manquants)
  data_filtered <- data %>%
    filter(!is.na(distance), !is.na(denivelation))
  
  # Déterminer la plage de dates valides
  x_min <- min(data_filtered$date)
  x_max <- max(data_filtered$date)
  
  # Calcul d'un ratio pour superposer le dénivelé (axe de droite) sur l'échelle de la distance (axe de gauche)
  ratio <- max(data_filtered$distance, na.rm = TRUE) / max(data_filtered$denivelation, na.rm = TRUE)
  
  ggplot(data_filtered, aes(x = date)) +
    # Barres pour la distance (axe de gauche)
    geom_col(aes(y = distance), fill = "steelblue", alpha = 0.7) +
    
    # Courbe lissée (smooth) pour le dénivelé, sans la courbe brute
    geom_smooth(
      aes(y = denivelation * ratio),
      method = "loess",
      se = FALSE,        # pas de bande d'incertitude
      color = "red",
      size = 1,
      span = 0.2         # ajustez ce paramètre pour un lissage plus ou moins prononcé
    ) +
    
    # Axe Y principal (distance) + axe Y secondaire (dénivelé)
    scale_y_continuous(
      name = "Distance (m)",
      # Ajout de lignes horizontales pour les graduations
      # (on s'en occupera via le thème ci-dessous)
      sec.axis = sec_axis(
        ~ . / ratio,
        name = "Dénivelé (m)"
      )
    ) +
    
    # Axe X : on limite la plage aux dates filtrées
    scale_x_date(
      limits = c(x_min, x_max),
      date_breaks = "1 day",
      date_labels = "%d/%m"
    ) +
    
    # Titres et légendes
    labs(
      x = "Date",
      title = "Distance et Dénivelé quotidiens"
    ) +
    
    # Thème plus épuré (classique) et personnalisation
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      
      # Ajouter des lignes horizontales pour les graduations Y
      panel.grid.major.y = element_line(color = "gray80"),
      panel.grid.minor.y = element_line(color = "gray90"),
      
      # Supprimer (ou laisser) les lignes verticales
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  
  library(ggplot2)
  library(dplyr)
  
  # 1) Filtrer les données pour retirer les NA
  data_filtered <- data %>%
    filter(!is.na(distance), !is.na(denivelation))
  
  # Vérifier la plage réelle de dates
  print(range(data_filtered$date))
  
  # 2) Calcul du ratio pour superposer le dénivelé (axe de droite)
  ratio <- max(data_filtered$distance, na.rm = TRUE) / max(data_filtered$denivelation, na.rm = TRUE)
  
  # 3) Graphique
  ggplot(data_filtered, aes(x = date)) +
    
    geom_col(
      aes(y = distance), 
      fill = "lightsteelblue2",   # Couleur de remplissage
      alpha = 0.7,         # Transparence (1 = opaque)
      color = "black",   # Couleur de la bordure
      size = 0.5         # Épaisseur de la bordure
    ) +
    
    # Courbe brute pour le dénivelé
    geom_line(aes(y = denivelation * ratio), color = "grey18", size = 1) +
    
    # Définition des axes Y
    scale_y_continuous(
      name = "Distance (m)",
      sec.axis = sec_axis(
        ~ . / ratio,
        name = "Dénivelé (m)"
      )
    ) +
    
    # Définition de l'axe X (dates)
    # - expand = c(0,0) pour ne pas ajouter de marge avant/après
    # - date_breaks = "1 day" si vous tenez vraiment à voir chaque jour
    scale_x_date(
      date_labels = "%d/%m",
      date_breaks = "1 day",
      expand = c(0, 0)
    ) +
    
    # Titres
    labs(
      x = "Date",
      title = "Distance et dénivelé quotidiens"
    ) +
    
    # Thème épuré + lignes horizontales
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      # Lignes horizontales pour les graduations Y
      panel.grid.major.y = element_line(color = "gray80"),
      panel.grid.minor.y = element_line(color = "gray90"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  
  
}

#### 13. Chargement paturage par jour ####
#---------------------------------------#
if (FALSE){
  # Library 
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  for(alpage in alpages){
    #ENTREE
    # Un dossier contenant carte de végétation
    carto_file = file.path(raster_dir, "Classifications_fusion_ColorIndexed_sc1_landforms_mnh.tif")
    
    # ENTREE
    #Dossier contenant les sous dossier des chargement
    case_flock_file = file.path(output_dir, "4. Chargements_Calcules")
    #Dossier contenant les fichiers du tot de chargement
    case_flock_alpage_file = file.path(case_flock_file,paste0(YEAR,"_",alpage))
    
    # Un .RDS par alpage contenant les charges journalières
    daily_rds_prefix = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
    
    # Un dossier contenant les ratsers des Unités Pastorales (UP)
    case_UP_file = file.path(raster_dir, "UP")
    # Un .SHP avec les Unités pastorales UP
    UP_file = file.path(case_UP_file, "v1_bd_shape_up_inra_2012_2014_2154_all_emprise.shp")
    
    
    # Un dossier contenant les Infos sur les alpages
    raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
    # Un data.frame contenant les dates de pose et de retrait des colliers
    alpage_info_file <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
    
    # SORTIE 
    # Création du dossier de sortie des indicateur pour la visualistaion
    output_Plot_Animation_case <- file.path(output_dir, "7. Plot_et_Animation")
    if (!dir.exists(output_Plot_Animation_case)) {
      dir.create(output_Plot_Animation_case, recursive = TRUE)
    }
    # Création du sous-dossier Indicateur traitée : Espace paturé
    output_load_veget_case <- file.path(output_Plot_Animation_case, "Chargement&Vegetation")
    if (!dir.exists(output_load_veget_case)) {
      dir.create(output_load_veget_case, recursive = TRUE)
    }
    
    # Un .rds contenant les données de trajectoires catégorisées par comportement et collier
    load_veget_rds = file.path(output_load_veget_case, paste0("charge_by_habitat_day_", YEAR, "_", alpage, ".rds"))
    
    # Un .gif avec le taux de chargement cumulatif par jour et par type d'habitat
    load_veget_day_gif = file.path(output_load_veget_case, paste0("Charge_cumulative_jour_habitat_", YEAR, "_", alpage, ".gif"))
    
    
    #CODE 
    
    if (FALSE){
      load_by_veget(alpage, alpage_info_file, UP_file, daily_rds_prefix, load_veget_rds)
    }
    
    if (FALSE){
      gif_plot_load_by_veget_day(load_veget_rds, load_veget_day_gif, delay_miliseconds = 49 )
    }
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
}

#### 14. Delta de chargement ####
#-------------------------------#
if (FALSE){
  # Library 
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  
  # OBJET
  
  YEAR_1 = 2022
  YEAR_2 = 2023
  
  # ENTREE
  
  #Dossier de sortie des indicateur pour la visualistaion
  file_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  #Sous-dossier Indicateur traitée : Chargement
  file_chargement_case <- file.path(output_visu_case, "Taux_chargement")
  
  #ANNEE 1
  # Sous-sous-dossier alpage et années traitée : Chargement
  file_case_alpage_YEAR_1 <- file.path(output_chargement_case, paste0(YEAR_1,"_",alpage))
  
  # Un .TIF par alpage contenant les charges par comportement
  output_flock_repos_tif_YEAR_1 = file.path(file_case_alpage_YEAR_1, paste0("repos",YEAR_1,"_",alpage,".tif"))
  output_flock_deplacement_tif_YEAR_1 = file.path(file_case_alpage_YEAR_1, paste0("deplacement",YEAR_1,"_",alpage,".tif"))
  output_flock_paturage_tif_YEAR_1 = file.path(file_case_alpage_YEAR_1, paste0("paturage",YEAR_1,"_",alpage,".tif"))
  # Un .TIF par alpage contenant la charge totale sur toute la saison
  output_flock_tot_tif_YEAR_1 = file.path(file_case_alpage_YEAR_1, paste0("total_",YEAR_1,"_",alpage,".tif"))
  
  
  #ANNEE 2
  # Sous-sous-dossier alpage et années traitée : Chargement
  file_case_alpage_YEAR_2 <- file.path(output_chargement_case, paste0(YEAR_2,"_",alpage))
  
  # Un .TIF par alpage contenant les charges par comportement
  output_flock_repos_tif_YEAR_2 = file.path(file_case_alpage_YEAR_2, paste0("repos",YEAR_2,"_",alpage,".tif"))
  output_flock_deplacement_tif_YEAR_2 = file.path(file_case_alpage_YEAR_2, paste0("deplacement",YEAR_2,"_",alpage,".tif"))
  output_flock_paturage_tif_YEAR_2 = file.path(file_case_alpage_YEAR_2, paste0("paturage",YEAR_2,"_",alpage,".tif"))
  # Un .TIF par alpage contenant la charge totale sur toute la saison
  output_flock_tot_tif_YEAR_2 = file.path(file_case_alpage_YEAR_2, paste0("total_",YEAR_2,"_",alpage,".tif"))
  
  
  
  # SORTIE
  
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_Plot_Animation_case <- file.path(output_dir, "7. Plot_et_Animation")
  if (!dir.exists(output_Plot_Animation_case)) {
    dir.create(output_Plot_Animation_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_load_delta_case <- file.path(output_Plot_Animation_case, "DELTA_Chargement")
  if (!dir.exists(output_load_delta_case)) {
    dir.create(output_load_delta_case, recursive = TRUE)
  }
  
  # Un .tif contenant le delta de chargement comportement 
  output_delta_load_paturage = file.path(output_load_delta_case, paste0("delta_chargement_paturage", YEAR_2, "_" ,YEAR_1, "_", alpage, ".tif"))
  output_delta_load_repos = file.path(output_load_delta_case, paste0("delta_chargement_repos", YEAR_2, "_" ,YEAR_1, "_", alpage, ".tif"))
  output_delta_load_deplacement = file.path(output_load_delta_case, paste0("delta_chargement_deplacement", YEAR_2, "_" ,YEAR_1, "_", alpage, ".tif"))
  # Un .tif contenant le delta de chargement total 
  output_delta_load_total = file.path(output_load_delta_case, paste0("delta_chargement_total", YEAR_2, "_" ,YEAR_1, "_", alpage, ".tif"))
  
  #CODE 
  
  
  
  library(raster)
  
  # Lecture des rasters de l'année 1
  repos_YEAR_1 <- raster(output_flock_repos_tif_YEAR_1)
  deplacement_YEAR_1 <- raster(output_flock_deplacement_tif_YEAR_1)
  paturage_YEAR_1 <- raster(output_flock_paturage_tif_YEAR_1)
  total_YEAR_1 <- raster(output_flock_tot_tif_YEAR_1)
  
  # Lecture des rasters de l'année 2
  repos_YEAR_2 <- raster(output_flock_repos_tif_YEAR_2)
  deplacement_YEAR_2 <- raster(output_flock_deplacement_tif_YEAR_2)
  paturage_YEAR_2 <- raster(output_flock_paturage_tif_YEAR_2)
  total_YEAR_2 <- raster(output_flock_tot_tif_YEAR_2)
  
  # Aligner les rasters de l'année 2 sur la grille de l'année 1
  repos_YEAR_2_aligned <- projectRaster(repos_YEAR_2, repos_YEAR_1, method = "bilinear")
  deplacement_YEAR_2_aligned <- projectRaster(deplacement_YEAR_2, deplacement_YEAR_1, method = "bilinear")
  paturage_YEAR_2_aligned <- projectRaster(paturage_YEAR_2, paturage_YEAR_1, method = "bilinear")
  total_YEAR_2_aligned <- projectRaster(total_YEAR_2, total_YEAR_1, method = "bilinear")
  
  # Calcul du delta de chargement (année 2 - année 1)
  delta_repos <- repos_YEAR_2_aligned - repos_YEAR_1
  delta_deplacement <- deplacement_YEAR_2_aligned - deplacement_YEAR_1
  delta_paturage <- paturage_YEAR_2_aligned - paturage_YEAR_1
  delta_total <- total_YEAR_2_aligned - total_YEAR_1
  
  
  ## Sauvegarde des résultats au format TIF
  writeRaster(delta_repos, filename = output_delta_load_repos, format = "GTiff", overwrite = TRUE)
  writeRaster(delta_deplacement, filename = output_delta_load_deplacement, format = "GTiff", overwrite = TRUE)
  writeRaster(delta_paturage, filename = output_delta_load_paturage, format = "GTiff", overwrite = TRUE)
  writeRaster(delta_total, filename = output_delta_load_total, format = "GTiff", overwrite = TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

}
