
#### 0. LIBRARIES AND CONSTANTS ####
#----------------------------------#
gc()

## Loading configuration ##
source("config.R")

## Definition of the analysis year and the alpine pastures to process ##
YEAR = 2025
alpage = "Mourtes"
alpages = "Mourtes"
TYPE <- "other" #Type of input data : catlog (at 2 minute) or other (catlog/other)

ALPAGES_TOTAL <- list(
  "9999" = c("Alpage_demo"),
  "2013" = c("Combe-Madame"),
  "2014" = c("Combe-Madame"),
  "2015" = c("Combe-Madame"),
  "2016" = c("Combe-Madame"),
  "2017" = c("Combe-Madame"),
  "2018" = c("Ane-et-Buyant", "Bedina", "Pesee", "Sept-Laux"),
  "2019" = c("Ane-et-Buyant", "Bedina", "Pesee", "Sept-Laux"),
  "2020" = c("Ane-et-Buyant", "Bedina", "Pesee","Rieuxclaret", "Sept-Laux"),
  "2021" = c("Ane-et-Buyant", "Bedina", "Pesee","Combe-Madame", "Sept-Laux"),
  "2022" = c("Ane-et-Buyant", "Bedina", "Cayolle", "Combe-Madame", "Grande-Fesse", "Jas-des-Lievres", "Lanchatra", "Pelvas","Pesee", "Sanguiniere","Sept-Laux", "Viso"),
  "2023" = c("Ane-et-Buyant", "Bedina", "Cayolle", "Crouzet", "Combe", "Combe-Madame", "Grande-Cabane", "Lanchatra", "Pesee", "Rouanette", "Sanguiniere", "Sept-Laux", "Vacherie-de-Roubion", "Viso"),
  "2024" = c("Viso", "Cayolle", "Sanguiniere"),
  "2025" = c("Viso", "Cayolle", "Sanguiniere", "Ponsonniere", "Mourtes", "Parau")
)
ALPAGES <- ALPAGES_TOTAL[[as.character(YEAR)]]




## Definition of the sampling period ##
if (TRUE){
  
  source(file.path(functions_dir, "Functions_filtering.R"))
  
  
  ## INPUT ##
  
  # A folder containing the raw trajectories
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  
  
  
  ## OUTPUT ##
  
  # Creation of the subfolder to store the sampling periods
  filter_output_dir <- file.path(output_dir, "0. Sampling_Periods")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  sampling_periods <- identify_sampling_period(data_dir, YEAR, TYPE, alpages, output_dir)
  
  print(sampling_periods)
  
}

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
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  
  sampling_period_file <- file.path(
    output_dir,
    "0. Sampling_Periods",
    paste0("Sampling_periods_", YEAR, "_", alpage, ".rds")
  )
  
  sampling_periods <- readRDS(sampling_period_file)
  
  
  ## OUTPUT ##
  gps_output_dir <- file.path(output_dir, "1. GPS_simple_GPKG")
  
  if (!dir.exists(gps_output_dir)) {
    dir.create(gps_output_dir, recursive = TRUE)
  }
  
  output_file <- file.path(
    gps_output_dir,
    paste0("Donnees_brutes_", YEAR, "_", alpage, "_simplifiees.gpkg")
  )
  
  
  ## CODE ##
  
  lapply(alpages, function(alpage) {
    
    collar_dir <- file.path(raw_data_dir, alpage)
    
    # Sélection du type de fichier
    file_pattern <- if (TYPE == "catlog") "\\.csv$" else "\\.Rdata$"
    collar_files <- list.files(
      collar_dir,
      pattern = file_pattern,
      full.names = TRUE
    )
    
    if (length(collar_files) == 0) {
      warning(paste(
        "No files found in",
        collar_dir,
        "for TYPE =",
        TYPE
      ))
      return(NULL)
    }
    
    lapply(collar_files, function(collar_f) {
      
      # Extraction de l'ID du collier
      collar_ID <- if (TYPE == "catlog") {
        strsplit(basename(collar_f), split = "_")[[1]][1]
      } else {
        strsplit(basename(collar_f), split = "_")[[1]][1]
      }
      
      print(paste(
        "Processing file:",
        collar_f,
        "Collar ID:",
        collar_ID
      ))
      
      # Charger les données GPS
      traject <- switch(
        TYPE,
        "catlog" = load_catlog_data(collar_f),
        "other" = load_other_data_rdata(collar_f),
        stop("Unrecognized TYPE: please choose 'catlog' or 'other'")
      )
      
      # Vérifier si les données sont vides
      if (is.null(traject) || nrow(traject) == 0) {
        warning(paste("Empty dataset after loading:", collar_f))
        return(NULL)
      }
      
      # Récupérer le sampling_period pour ce collier
      sampling_period <- sampling_periods %>%
        filter(ID == collar_ID) %>%
        pull(SAMPLING)
      
      # Si aucun sampling_period trouvé, erreur
      if (length(sampling_period) == 0 || is.na(sampling_period)) {
        stop(paste(
          "ERREUR: Aucun sampling_period trouvé pour le collier",
          collar_ID
        ))
      }
      
      # Ajustement vers un échantillonnage d'une heure
      if (sampling_period != 60) {
        
        print(paste(
          "Collar",
          collar_ID,
          "has sampling_period =",
          sampling_period,
          "minutes. Resampling to 60 minutes."
        ))
        
        # Calcul du facteur de réduction
        reduction_factor <- max(1, round(60 / sampling_period))
        
        # Application du filtre
        traject <- traject %>%
          slice(which(row_number() %% reduction_factor == 0))
      }
      
      # Transformation et formatage des données
      traject <- traject %>%
        mutate(
          ID = collar_ID,
          date = lubridate::format_ISO8601(date)
        ) %>%
        vect(
          geom = c("lon", "lat"),
          crs = CRS_WSG84
        )
      
      return(traject)
      
    }) %>%
      do.call(rbind, .)
    
  }) %>%
    do.call(rbind, .) -> merged_data
  
  # Vérifier si les données fusionnées sont vides
  if (is.null(merged_data) || nrow(merged_data) == 0) {
    stop(
      "No data available to export to GPKG. Check input files and processing steps."
    )
  }
  
  # Exporter les données vers un fichier GPKG
  writeVector(
    merged_data,
    filename = output_file,
    overwrite = TRUE
  )
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
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  AIF <- file.path(raw_data_dir, paste0(YEAR, "_infos_alpages.csv"))
  check_and_correct_csv(csv_path = AIF)
  
  
  # List of parameters for the different stampling period   !!!!!!!! A voir avec mathieu !!!!!!!!!!!!!!
  param_bank_multi <- list(
    "1"  = list(
      A = list(medcrit=600,  meancrit=450, spikesp=1500, spikecos=-0.95),
      B = list(medcrit=650,  meancrit=500, spikesp=1500, spikecos=-0.95),
      C = list(medcrit=700,  meancrit=550, spikesp=1500, spikecos=-0.95)
    ),
    "2"  = list(
      A = list(medcrit=750,  meancrit=500, spikesp=1500, spikecos=-0.95),
      B = list(medcrit=500,  meancrit=500, spikesp=1500, spikecos=-0.95),
      C = list(medcrit=750,  meancrit=350, spikesp=1500, spikecos=-0.95)
    ),
    "10" = list(
      A = list(medcrit=900,  meancrit=450, spikesp=1500, spikecos=-0.95),
      B = list(medcrit=1000, meancrit=500, spikesp=1500, spikecos=-0.95),
      C = list(medcrit=1100, meancrit=550, spikesp=1500, spikecos=-0.95)
    ),
    "15" = list(
      A = list(medcrit=1000, meancrit=450, spikesp=1500, spikecos=-0.95),
      B = list(medcrit=1100, meancrit=500, spikesp=1500, spikecos=-0.95),
      C = list(medcrit=1200, meancrit=550, spikesp=1500, spikecos=-0.95)
    ),
    "20" = list(
      A = list(medcrit=1100, meancrit=450, spikesp=1500, spikecos=-0.95),
      B = list(medcrit=1200, meancrit=500, spikesp=1500, spikecos=-0.95),
      C = list(medcrit=1300, meancrit=550, spikesp=1500, spikecos=-0.95)
    ),
    "30" = list(
      A = list(medcrit=700, meancrit=450, spikesp=1500, spikecos=-0.95),
      B = list(medcrit=1300, meancrit=500, spikesp=1500, spikecos=-0.95),
      C = list(medcrit=800, meancrit=550, spikesp=1500, spikecos=-0.95)
    )
  )
  
  ## OUPUTS ##
  filter_output_dir <- file.path(output_dir, "2. Filtre_de_Bjorneraas")
  if (!dir.exists(filter_output_dir)) dir.create(filter_output_dir, recursive = TRUE)
  pdf(file.path(filter_output_dir, paste0("Filtering_calibration_", YEAR, "_", alpage, ".pdf")), width = 9, height = 9)
  
  
  
  ## CODE ##
  file_pattern <- if (TYPE == "catlog") "\\.csv$" else "\\.Rdata$"
  read_fun     <- if (TYPE == "catlog") load_catlog_data else load_other_data_rdata
  
  ## 1) Fichiers (échantillon de 3 max)
  files <- list.files(file.path(raw_data_dir, alpage), pattern = file_pattern, full.names = TRUE)
  files <- files[1:min(3, length(files))]
  
  ## 2) Données combinées pour histogrammes (lecture via read_fun)
  data_list <- lapply(files, function(file) {
    d <- read_fun(file)
    # Harmonisation légère pour .Rdata
    if ("long" %in% names(d) && !"lon" %in% names(d)) {
      names(d)[names(d) == "long"] <- "lon"
    }
    # date POSIXct tolérante + drop NA
    if (!inherits(d$date, "POSIXct")) d$date <- suppressWarnings(as.POSIXct(d$date, tz = "GMT"))
    d <- d[!is.na(d$date), ]
    d
  })
  data <- dplyr::bind_rows(data_list)
  
  ## 3) Bornage temporel via AIF (méthode d’origine)
  beg_date <- as.POSIXct(get_alpage_info(alpage, AIF, "date_pose"),    tz="GMT", format="%d/%m/%Y %H:%M:%S")
  end_date <- as.POSIXct(get_alpage_info(alpage, AIF, "date_retrait"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
  data <- date_filter(data, beg_date, end_date)
  
  ## 4) Histogrammes : ne garder que lat/lon finies AVANT terra (bornes retirées)
  data_xy_input <- data %>%
    dplyr::mutate(
      lat = suppressWarnings(as.numeric(lat)),
      lon = suppressWarnings(as.numeric(lon))
    ) %>%
    dplyr::filter(is.finite(lat), is.finite(lon))
  if (nrow(data_xy_input) == 0) stop("Aucune localisation valide (lat/lon) dans l'intervalle de dates.")
  
  data_xy <- data_xy_input %>%
    terra::vect(crs = "EPSG:4326") %>%
    terra::project("EPSG:2154") %>%
    as.data.frame(geom = "XY")
  
  temps <- diff(data_xy$date); temps <- as.numeric(temps, units = "mins")
  hist(temps, nclass = 30)
  
  dist <- sqrt(diff(data_xy$x)^2 + diff(data_xy$y)^2)
  h <- hist(dist, nclass = 30, xlab='Distance (m)', xaxt="n")
  
  ## 5) Sampling periods RDS (par alpage)
  sp_dir  <- file.path(output_dir, "0. Sampling_Periods")
  sp_path <- file.path(sp_dir, paste0("Sampling_periods_", YEAR, "_", alpage, ".rds"))
  sampling <- readRDS(sp_path)
  
  ## 6) Pour chaque collier : SON pas -> appliquer les 3 combos correspondantes
  for (file in files) {
    collar_base <- basename(file)
    collar_ID   <- sub("[_-].*$", "", tools::file_path_sans_ext(collar_base))
    
    # lecture selon TYPE
    d <- read_fun(file)
    if ("long" %in% names(d) && !"lon" %in% names(d)) {
      names(d)[names(d) == "long"] <- "lon"
    }
    if (!inherits(d$date, "POSIXct")) d$date <- suppressWarnings(as.POSIXct(d$date, tz = "GMT"))
    d <- d[!is.na(d$date), ]
    d <- date_filter(d, beg_date, end_date)
    
    # lat/lon uniquement finies (bornes supprimées)
    d$lat <- suppressWarnings(as.numeric(d$lat))
    d$lon <- suppressWarnings(as.numeric(d$lon))
    d <- d[is.finite(d$lat) & is.finite(d$lon), ]
    
    # tri + ID de longueur exacte
    d <- d[order(d$date), ]
    d$ID <- rep(collar_ID, nrow(d))
    
    # pas -> 3 combos
    sp_min <- get_sp_min_from_rds(sampling, collar_ID, file)
    combos <- param_bank_multi[[as.character(sp_min)]]
    if (is.null(combos)) {
      warning("Aucune banque de paramètres pour pas=", sp_min, " min (", collar_ID, ").")
      next
    }
    
    for (lab in names(combos)) {
      p <- combos[[lab]]
      trajectories <- position_filter(
        d,
        medcrit  = p$medcrit,
        meancrit = p$meancrit,
        spikesp  = p$spikesp,
        spikecos = p$spikecos
      )
      
      ok <- trajectories[!(trajectories$R1error | trajectories$R2error), ]
      minmax_xy <- get_minmax_L93(ok, buffer = 100)
      
      trajectories$errors <- 1
      trajectories$errors[trajectories$R1error] <- 2
      trajectories$errors[trajectories$R2error] <- 3
      pal <- c("#56B4E9", "red", "black")
      
      print(
        ggplot(trajectories, aes(x, y, col = errors)) +
          geom_path(size = 0.2) +
          geom_point(size = 0.3) +
          coord_equal() +
          xlim(minmax_xy$x_min, minmax_xy$x_max) +
          ylim(minmax_xy$y_min, minmax_xy$y_max) +
          ggtitle(paste0("[", lab, "] ", collar_ID, " | pas=", sp_min, " min | medcrit=", p$medcrit,
                         " meancrit=", p$meancrit, " spikesp=", p$spikesp, " spikecos=", p$spikecos)) +
          scale_colour_gradientn(colors = pal, guide = "legend",
                                 breaks = c(1,2,3), labels = c("OK","R1error","R2error"))
      )
      
      traj5 <- trajectories %>% dplyr::filter(date < beg_date + 3600*24*5)
      print(
        ggplot(traj5, aes(x, y, col = errors)) +
          geom_path(size = 0.2) +
          geom_point(size = 0.3) +
          coord_equal() +
          ggtitle(paste0("[ZOOM 5j] [", lab, "] ", collar_ID, " | pas=", sp_min, " min")) +
          scale_colour_gradientn(colors = pal, guide = "legend",
                                 breaks = c(1,2,3), labels = c("OK","R1error","R2error"))
      )
    }
  }
  
  # Fermer le PDF
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
  
  
  # A .rds file "sampling periode" identify in the part 0.
  filter_output_dir <- file.path(output_dir, "0. Sampling_Periods")
  sampling_period_file <- file.path(filter_output_dir, paste0("Sampling_periods_", YEAR, "_",alpage,".rds"))
  sampling <- readRDS(sampling_period_file)
  
  
  # List of parameters for the different stampling period   !!!!!!!! A voir avec mathieu !!!!!!!!!!!!!!
  param_bank <- list(
    "1"  = list(medcrit = 650,  meancrit = 500, spikesp = 1500, spikecos = -0.95),
    "2"  = list(medcrit = 750,  meancrit = 500, spikesp = 1500, spikecos = -0.95),
    "10" = list(medcrit = 1000, meancrit = 500, spikesp = 1500, spikecos = -0.95),
    "15" = list(medcrit = 1100, meancrit = 500, spikesp = 1500, spikecos = -0.95),
    "20" = list(medcrit = 1200, meancrit = 500, spikesp = 1500, spikecos = -0.95),
    "30" = list(medcrit = 1300, meancrit = 500, spikesp = 1500, spikecos = -0.95)
  )
  
  
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
    message("WORKING ON ALPAGE :", alpage)
    
    # Sampling (par alpage)
    sp_dir  <- file.path(output_dir, "0. Sampling_Periods")
    sp_path <- file.path(sp_dir, paste0("Sampling_periods_", YEAR, "_", alpage, ".rds"))
    if (!file.exists(sp_path)) stop("Sampling RDS manquant: ", sp_path)
    sampling <- readRDS(sp_path)
    
    # Fichiers bruts
    collar_dir   <- file.path(raw_data_dir, alpage)
    file_pattern <- if (TYPE == "catlog") "\\.csv$" else "\\.Rdata$"
    read_fun     <- if (TYPE == "catlog") load_catlog_data else load_other_data_rdata
    collar_files <- list.files(collar_dir, pattern = file_pattern, full.names = TRUE)
    if (!length(collar_files)) { warning("Aucun fichier ", file_pattern, " pour ", alpage); next }
    
    # Sorties par alpage
    output_rds_file <- file.path(out_dir, paste0("Catlog_", YEAR, "_filtered_", alpage, ".rds"))
    indicator_file  <- file.path(out_dir, paste0(YEAR, "_filtering_", alpage, ".csv"))
    
    indicators_list <- lapply(collar_files, function(collar) {
      collar_base <- basename(collar)
      collar_ID   <- sub("[_-].*$", "", tools::file_path_sans_ext(collar_base))
      
      sp_min <- get_sp_min_from_rds(sampling, collar_ID, collar)
      p      <- choose_params_strict(sp_min, param_bank)
      message(sprintf("Collier: %s | SAMPLING=%s min -> medcrit=%s meancrit=%s spikesp=%s spikecos=%s",
                      collar_base, sp_min, p$medcrit, p$meancrit, p$spikesp, p$spikecos))
      
      dat <- read_fun(collar)
      
      res <- tryCatch(
        filter_one_collar(
          traject = dat,
          collar_file = collar_base,
          output_rds_file = output_rds_file,
          alpage_name = alpage,
          beg_date = NA, end_date = NA,
          individual_info_file = IIF,
          bjoneraas.medcrit  = p$medcrit,
          bjoneraas.meancrit = p$meancrit,
          bjoneraas.spikesp  = p$spikesp,
          bjoneraas.spikecos = p$spikecos,
          sampling_period    = as.numeric(sp_min) * 60
        ),
        error = function(e) {
          warning(sprintf("Collier %s: %s", collar_base, e$message))
          data.frame(
            name = collar_ID,
            worked_until_end = NA_integer_, nloc = NA_integer_,
            R1error = NA_integer_, R2error = NA_integer_,
            localisation_rate = NA_real_,  error_perc = NA_real_,
            stringsAsFactors = FALSE
          )
        }
      )
      
      ensure_indicator_shape(res)
    })
    
    indicators <- dplyr::bind_rows(indicators_list)
    
    if (nrow(indicators)) {
      indicators <- dplyr::bind_rows(
        indicators,
        data.frame(
          name = paste("TOTAL", alpage),
          worked_until_end = sum(indicators$worked_until_end == 1, na.rm = TRUE),
          nloc = NA, R1error = NA, R2error = NA,
          error_perc = sum(indicators$nloc * indicators$error_perc, na.rm = TRUE) /
            sum(indicators$nloc, na.rm = TRUE),
          localisation_rate = mean(indicators$localisation_rate, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      )
    }
    
    write.table(indicators, file = indicator_file, append = TRUE, sep = ",",
                row.names = FALSE, col.names = FALSE)
  }




}

#### 2.Bis FILTERING OFB DATA ####
#--------------------------------#
if (TRUE) {
  
  # Temporary processing of OFB data previously filtered
  # with the Bjorneraas filter.
  
  if (TYPE == "other") {
    
    ## LIBRARY ##
    library(sf)
    
    
    ## INPUT ##
    
    # Folder containing raw OFB trajectories
    raw_data_dir <- file.path(
      data_dir,
      paste0("Colliers_", YEAR, "_brutes")
    )
    
    # Folder containing the Pastoral Unit shapefile
    case_UP_file <- file.path(raster_dir, "UP")
    
    # Pastoral Unit shapefile
    UP_file <- file.path(
      case_UP_file,
      paste0("UP_", alpage, ".shp")
    )
    
    
    ## OUTPUT ##
    
    filter_output_dir <- file.path(
      output_dir,
      "2. Filtre_de_Bjorneraas"
    )
    
    if (!dir.exists(filter_output_dir)) {
      dir.create(filter_output_dir, recursive = TRUE)
    }
    
    output_rds_file <- file.path(
      filter_output_dir,
      paste0("Catlog_", YEAR, "_filtered_", alpages, ".rds")
    )
    
    
    ## CODE ##
    
    files <- list.files(
      file.path(raw_data_dir, alpage),
      pattern = "\\.Rdata$",
      full.names = TRUE
    )
    
    data <- do.call(rbind, lapply(files, function(file) {
      
      traject <- load_other_data_rdata(file)
      traject$ID <- basename(file)
      
      return(traject)
    }))
    
    
    # Remove missing coordinates
    initial_count <- nrow(data)
    
    data <- data %>%
      filter(!is.na(lat) & !is.na(lon))
    
    print(paste(
      initial_count - nrow(data),
      "points with NA were removed. Remaining points:",
      nrow(data)
    ))
    
    
    # Remove positions identified by the Bjorneraas filter
    initial_count <- nrow(data)
    
    data <- data %>%
      filter(infoloc != "pb_bjorneraas")
    
    print(paste(
      initial_count - nrow(data),
      "points with location issues were removed. Remaining points:",
      nrow(data)
    ))
    
    
    # Load the Pastoral Unit
    UP <- st_read(UP_file, quiet = TRUE)
    
    if (is.na(st_crs(UP))) {
      stop("The Pastoral Unit shapefile has no CRS.")
    }
    
    
    # Identify positions located inside the Pastoral Unit
    points_sf <- st_as_sf(
      data,
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    ) %>%
      st_transform(st_crs(UP))
    
    data$inside_UP <- lengths(
      st_intersects(points_sf, UP)
    ) > 0
    
    
    # Identify the first and last position inside the Pastoral Unit
    grazing_periods <- data %>%
      arrange(ID, date) %>%
      filter(inside_UP) %>%
      group_by(ID) %>%
      summarise(
        arrival_date = first(date),
        departure_date = last(date),
        .groups = "drop"
      )
    
    
    # Print detected arrival and departure dates
    cat("\nDetected grazing period for each collar:\n")
    print(grazing_periods, n = Inf)
    
    
    # Keep all positions between arrival and departure,
    # including temporary exits from the Pastoral Unit
    data <- data %>%
      inner_join(grazing_periods, by = "ID") %>%
      filter(
        date >= arrival_date,
        date <= departure_date
      ) %>%
      dplyr::select(
        -inside_UP,
        -arrival_date,
        -departure_date
      )
    
    
    # Format data to match the CATLOG output
    data <- data %>%
      rename(time = date) %>%
      mutate(
        ID = sub("_.*", "", ID),
        alpage = alpage,
        species = "brebis",
        race = "Merinos"
      ) %>%
      dplyr::select(-infoloc)
    
    
    # Save filtered trajectories
    saveRDS(data, output_rds_file)
  }
}


#### 3. Night parc identification ####
#------------------------------------#
if (TRUE){
  ## Description ----
  # This part is a new approach to identify for each day the night park. This 
  # approach is a proxy of the different grazing sector of the summer pasture
  # And because the night park is a structuralist element of the grazing plan.
  
  # Method:
  # - Filter fixes to retain night locations in parks (19:30–04:30 UTC).
  # - Cluster night locations with DBSCAN (eps = 100 m; minPts = 80).
  # - For each cluster, compute a KDE home range (hr_kde) and extract the 95% isopleth
  #   to delineate each night park.
  # - Merge night parks whose centroids are within 500 m (same park ID / same treatment).
  # - Assign one park per animal per day:
  #     * For each ID × day, identify the dominant park during the first 30 minutes
  #       and the last 30 minutes of the night track.
  #     * If start-park == end-park, assign that park to the day.
  #     * If different, label the day as "transition_day".
  #   (Spatial assignment done with st_join / st_within.)
  # - Parks that occur only on transition days are labelled "transition_park".
  
  
  # Data :
  # - INPUT : GPS point filtered by Bjorneras ("Catlog_",YEAR,"_filtered_",alpages,".rds")
  # - OUTPUT : a .RDS with the point and a new column park (park_1, park_2, park_3, ...)
  
  ## FUNCTION & PACKAGE ----
  library(dplyr)
  library(lubridate)
  library(amt)
  library(dbscan)
  library(terra)
  
  ## INPUT ----
  # Filter case (part2.2)
  filter_case <- file.path(output_dir, "2. Filtre_de_Bjorneraas")
  if (!dir.exists(filter_case)) {dir.create(filter_case, recursive = TRUE)}
  
  # An .RDS file containing the filtered trajectories
  input_rds_file = file.path(filter_case, paste0("Catlog_",YEAR,"_filtered_",alpage,".rds"))
  
  # A case with Pastoral unite shapefile (UP)
  case_UP_file = file.path(raster_dir, "UP")
  # Un .SHP avec les Unités pastorales UP
  UP_file = file.path(case_UP_file, paste0("UP_",alpage,".shp"))
  
  ## OUTPUT ----
  nightpark_output_dir <- file.path(output_dir, "3. Night_park")
  if (!dir.exists(nightpark_output_dir)) {dir.create(nightpark_output_dir, recursive = TRUE)}
  
  # An .RDS file containing the filtered trajectories
  output_rds_file = file.path(nightpark_output_dir, paste0("Catlog_",YEAR,"_night_park_",alpage,".rds"))
  
  output_shp_file = file.path(nightpark_output_dir, paste0("Night_park_",alpage,"_",YEAR,".shp"))
  
  ref_file <- file.path(nightpark_output_dir, paste0("NightPark_ref_", alpage, ".rds"))
  
  night_pen_use_file <- file.path(nightpark_output_dir, paste0("Use_night_pens_", YEAR, "_", alpage, ".rds"))
  
  # rast of the template 
  template_case = file.path(raster_dir, "template")
  if(!dir.exists(template_case)) {dir.create(template_case, recursive = TRUE)}
  output_rast_file = file.path(template_case, paste0("template_",alpage,".tif"))
  
  ## CODE ----
  
  # read input dataset
  d <- readRDS(input_rds_file)
  
  d_night <- d %>%
    mutate(hm = hour(time) + minute(time)/60) %>% 
    filter(hm >= 19.5 | hm < 4.5) %>%
    dplyr::select(-c(hm,species,race, lat, lon))
  
  
  d_night_vect <- st_as_sf(d_night, coords = c("x","y"), crs = 2154)
  
  # 2) Clustering global (amas = parcs)
  eps_m  <- 100  # ajuste 30-80
  minPts <- 80  # ajuste 10-80
  
  cl <- dbscan(st_coordinates(d_night_vect), eps = eps_m, minPts = minPts)
  d_night_vect$cluster <- cl$cluster
  d_night$cluster <- d_night_vect$cluster
  
  # 3) template creation
  res_m <- 10
  buffer_m <- 100
  
  up <- terra::vect(UP_file)
  up <- terra::project(up, "EPSG:2154")
  
  pts <- terra::vect(d_night_vect)
  pts <- terra::project(pts, "EPSG:2154")
  
  e_up  <- terra::ext(up)
  e_pts <- terra::ext(pts)
  
  e <- terra::ext(
    min(e_up$xmin, e_pts$xmin) - buffer_m,
    max(e_up$xmax, e_pts$xmax) + buffer_m,
    min(e_up$ymin, e_pts$ymin) - buffer_m,
    max(e_up$ymax, e_pts$ymax) + buffer_m
  )
  
  snap_down <- function(v, res) floor(v / res) * res
  snap_up   <- function(v, res) ceiling(v / res) * res
  
  e <- terra::ext(
    snap_down(e$xmin, res_m),
    snap_up(e$xmax, res_m),
    snap_down(e$ymin, res_m),
    snap_up(e$ymax, res_m)
  )
  
  template <- terra::rast(e, res = res_m, crs = "EPSG:2154")
  terra::values(template) <- 1
  
  terra::writeRaster(template, output_rast_file, overwrite = TRUE)
  
  
  
  
  
  # KDE + 95% par cluster
  k_list <- sort(unique(d_night$cluster))
  k_list <- k_list[k_list > 0]   # ignore cluster 0 = bruit
  
  iso_list <- list()
  
  for (k in k_list) {
    
    sub <- d_night[d_night$cluster == k, ]
    
    trk <- make_track(
      sub,
      x,
      y,
      time,
      crs = 2154,
      all_cols = TRUE
    )
    
    hr <- try(
      hr_kde(trk, trast = template),
      silent = TRUE
    )
    
    if (inherits(hr, "try-error")) {
      cat("Cluster", k, "skipped: KDE failed.\n")
      next
    }
    
    iso <- try(
      hr_isopleths(hr, levels = 0.8),
      silent = TRUE
    )
    
    if (inherits(iso, "try-error")) {
      cat("Cluster", k, "skipped: isopleth failed.\n")
      next
    }
    
    iso$cluster <- k
    iso_list[[as.character(k)]] <- iso
  }
  hr_isopleth95_all <- do.call(rbind, iso_list)
  
  
  ### APPLIED A SIMPLE LOWER TO ADJUSTE NIGHT PARK
  ## 1st RULES : Combine night park with a distance <500 m
  
  # Centroides of polygones
  isopleth_sf <- st_as_sf(hr_isopleth95_all)  # isopleths 95% en sf
  centroid_sf <- st_centroid(isopleth_sf)
  
  # Clustering des centroides <= 500m same grazing sector ans same night park
  xy_centroid <- st_coordinates(centroid_sf)
  buf <- if (alpage == "Sanguiniere") 500 else 500 
  buffer_500  <- dbscan::dbscan(xy_centroid, eps = buf, minPts = 1)$cluster
  isopleth_sf$park <- paste0("park_", buffer_500)
  
  # d : data.frame avec x,y
  data_vect <- st_as_sf(d, coords = c("x","y"), crs = 2154, remove = FALSE)
  
  # isopleth_sf : MULTIPOLYGON avec colonne "park"
  isopleth_sf <- st_make_valid(isopleth_sf)
  
  # park_point : NA si hors polygone
  data_vect <- st_join(data_vect, isopleth_sf["park"], join = st_within)
  
  majority <- function(x){
    x <- x[!is.na(x)]
    if(!length(x)) return(NA_character_)
    names(which.max(table(x)))
  }
  
  n_vote <- 30
  
  day_park <- data_vect %>%
    st_drop_geometry() %>%
    mutate(day = as.Date(time)) %>%
    group_by(day) %>%
    summarise(
      # 30 premiers points de la journée (tous IDs confondus)
      park_start = majority(park[order(time)][1:min(n_vote, n())]),
      # 30 derniers points
      park_end   = majority(park[order(time, decreasing = TRUE)][1:min(n_vote, n())]),
      park = case_when(
        !is.na(park_start) & park_start == park_end ~ park_start,
        !is.na(park_start) & !is.na(park_end) & park_start != park_end ~ "transition_day",
        is.na(park_start) & !is.na(park_end) ~ park_end,     # début de série
        !is.na(park_start) & is.na(park_end) ~ park_start,   # fin de série
        TRUE ~ NA_character_
      ),
      .groups = "drop"
    )
  
  exceptions <- list(
    "Sanguiniere_2023" = as.Date(c("2023-08-11","2023-08-12","2023-08-13",
                                   "2023-09-02","2023-09-03")),
    "Viso_2024" = as.Date(c("2024-10-04", "2024-10-05"))
  )
  
  day_park <- day_park %>% arrange(day)
  key <- paste0(alpage, "_", YEAR)
  
  if (key %in% names(exceptions)) {
    dates_fix <- exceptions[[key]]
    
    for (i in seq_len(nrow(day_park))) {
      
      if (day_park$day[i] %in% dates_fix) {
        
        # chercher le dernier "jour normal" avant i (pas transition_day, pas NA)
        j <- i - 1
        while (j >= 1 && (is.na(day_park$park[j]) || day_park$park[j] == "transition_day")) {
          j <- j - 1
        }
        
        # si trouvé, on applique
        if (j >= 1) {
          day_park$park[i] <- day_park$park[j]
        }
      }
    }
  }
  
  
  # 1) Remap old park -> park_1..park_n (on ignore transition_day)
  parks_kept <- day_park %>%
    dplyr::filter(!is.na(park), park != "transition_day") %>%
    dplyr::distinct(park) %>%
    dplyr::pull(park)
  
  parks_kept_sorted <- parks_kept[order(as.integer(sub("park_", "", parks_kept)))]
  
  remap <- tibble::tibble(
    old = parks_kept_sorted,
    new = paste0("park_", seq_along(parks_kept_sorted))
  )
  
  # 2) Appliquer remap dans day_park (transition_day inchangé), puis réinjecter dans d_end + save RDS
  day_park <- day_park %>%
    dplyr::left_join(remap, by = c("park" = "old")) %>%
    dplyr::mutate(park = dplyr::if_else(!is.na(new), new, park)) %>%
    dplyr::select(-new)
  
  # 3) SHP : garder transition_park (mais RDS garde transition_day)
  isopleth_final <- isopleth_sf %>%
    dplyr::mutate(park = dplyr::if_else(park %in% parks_kept, park, "transition_park")) %>%
    dplyr::left_join(remap, by = c("park" = "old")) %>%
    dplyr::mutate(park = dplyr::if_else(!is.na(new), new, park)) %>%
    dplyr::select(-new) %>%
    dplyr::group_by(park) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
  
  
  # ---- Stable park names between years (simple) ----
  dist_max <- 300
  
  cent <- st_coordinates(st_centroid(isopleth_final))
  parks_year <- tibble::tibble(park = isopleth_final$park, x = cent[,1], y = cent[,2]) %>%
    dplyr::filter(park != "transition_park") %>%
    dplyr::distinct()
  
  if (!file.exists(ref_file)) {
    # first run: create reference
    ref <- parks_year %>% dplyr::transmute(park_stable = park, x_ref = x, y_ref = y)
    saveRDS(ref, ref_file)
  } else {
    ref <- readRDS(ref_file)
    next_id <- max(as.integer(sub("park_", "", ref$park_stable)), na.rm = TRUE)
    
    # match each park of this year to closest reference park
    map <- parks_year %>%
      rowwise() %>%
      mutate(
        park_stable = {
          dvec <- sqrt((ref$x_ref - x)^2 + (ref$y_ref - y)^2)
          j <- which.min(dvec)
          if (dvec[j] <= dist_max) ref$park_stable[j] else NA_character_
        }
      ) %>%
      ungroup() %>%
      dplyr::select(park, park_stable)
    
    
    # new parks -> park_(max+1)
    for (i in which(is.na(map$park_stable))) {
      next_id <- next_id + 1
      map$park_stable[i] <- paste0("park_", next_id)
    }
    
    # apply to SHP
    isopleth_final <- isopleth_final %>%
      dplyr::left_join(map, by = "park") %>%
      dplyr::mutate(park = dplyr::if_else(!is.na(park_stable), park_stable, park)) %>%
      dplyr::select(-park_stable)
    
    # apply to day_park (RDS): only for real parks, keep transition_day
    day_park <- day_park %>%
      dplyr::left_join(map, by = "park") %>%
      dplyr::mutate(park = dplyr::if_else(park != "transition_day" & !is.na(park_stable), park_stable, park)) %>%
      dplyr::select(-park_stable)
    
    # update reference with new parks
    new_ref <- parks_year %>%
      dplyr::left_join(map, by = "park") %>%
      dplyr::filter(!(park_stable %in% ref$park_stable)) %>%
      dplyr::transmute(park_stable, x_ref = x, y_ref = y)
    
    if (nrow(new_ref) > 0) saveRDS(dplyr::bind_rows(ref, new_ref), ref_file)
  }
  
  sf::st_write(isopleth_final, output_shp_file, append = FALSE)
  
  d_end <- d %>%
    dplyr::mutate(day = as.Date(time)) %>%
    dplyr::left_join(day_park %>% dplyr::select(day, park), by = "day") %>%
    dplyr::select(-day)
  
  saveRDS(d_end, output_rds_file)
  
  
  # ---- Night-pen use dataset ----
  
  # Dates with an assigned night pen
  used_nights <- day_park %>%
    dplyr::filter(!is.na(park), park != "transition_day") %>%
    dplyr::transmute(date = day,  park, used = 1L)
  
  # Status of each day
  day_status <- day_park %>%
    dplyr::transmute(date = day, transition_day = !is.na(park) & park == "transition_day")
  
  # Complete dataset: one row per park and per day
  night_pen_use <- tidyr::expand_grid(
    park = unique(used_nights$park),
    date = seq(min(day_park$day, na.rm = TRUE), max(day_park$day, na.rm = TRUE),by = "day")) %>%
    dplyr::left_join(used_nights, by = c("park", "date")) %>%
    dplyr::left_join(day_status, by = "date") %>%
    dplyr::mutate(year = YEAR, alpage = alpage, doy = lubridate::yday(date),
                  used = tidyr::replace_na(used, 0L), transition_day = tidyr::replace_na(transition_day, FALSE)) %>%
    dplyr::group_by(park) %>%
    dplyr::mutate(n_nights = sum(used)) %>%
    dplyr::ungroup() %>%
    dplyr::select(year, alpage, park, date, doy, used, transition_day, n_nights) %>%
    dplyr::arrange(as.integer(sub("park_", "", park)), date)
  
  # Print the number and dates of nights used by each park
  night_pen_summary <- night_pen_use %>%
    dplyr::filter(used == 1) %>%
    dplyr::group_by(year, alpage, park) %>%
    dplyr::summarise(
      n_nights = dplyr::first(n_nights),
      dates = paste(date, collapse = ", "),
      .groups = "drop"
    )
  
  cat("\nNight-pen use by park:\n")
  print(night_pen_summary, n = Inf)
  
  # Save daily night-pen use
  saveRDS(night_pen_use, night_pen_use_file)
  
}

#### 4. HMM FITTING #### 
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
  check_and_correct_csv(csv_path = individual_info_file)
  
  # Charger le fichier des périodes d'échantillonnage
  sampling_period_file <- file.path(output_dir, "0. Sampling_Periods", paste0("Sampling_Periods_", YEAR, "_", alpage, ".rds"))
  sampling_periods <- readRDS(sampling_period_file)
  sampling_table <- readRDS(sampling_period_file)
  
  
  ## OUTPUTS ##
  # Creation of the subfolder to store the results of the Bjorneraas filter
  filter_output_dir <- file.path(output_dir, "4. HMM_comportement")
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
  output_rds_file = file.path(output_dir, "4. HMM_comportement", paste0("Catlog_",YEAR,"_",alpage,"_viterbi.rds"))
  
  
  
  ## CODE ##
  
  ### LOADING DATA FOR ANALYSES
  data = readRDS(input_rds_file)
  data = data[data$species == "brebis",]
  str(data)
  data %>%
    group_by(ID) %>%
    summarise(
      n = n(),
      na_time = sum(is.na(time)),
      na_x = sum(is.na(x)),
      na_y = sum(is.na(y)),
      na_lat = sum(is.na(lat)),
      na_lon = sum(is.na(lon))
    ) %>%
    arrange(desc(na_x + na_y + na_time), desc(na_lat + na_lon)) %>%
    print(n = 30)
  
  # Générer les paramètres pour chaque ID
  sampling_parameters_list <- lapply(sampling_periods$SAMPLING, get_sampling_parameters)
  names(sampling_parameters_list) <- sampling_periods$ID
  
  
  
  
  # HMM FIT
  run_parameters_list <- lapply(names(sampling_parameters_list), function(id) {
    params <- sampling_parameters_list[[id]]
    run_parameters <- list(
      model = "HMM",
      resampling_ratio = params$resampling_ratio,
      resampling_first_index = 0,
      rollavg = FALSE,
      rollavg_convolution = c(0.15, 0.7, 0.15),
      knownRestingStates = FALSE,
      dist = list(step = "gamma", angle = "vm"),
      DM = list(angle=list(mean = ~1, concentration = ~1)),
      covariants = ~cos(hour*3.141593/12),
      Par0 = list(step = c(10, 25, 50, 10, 15, 40), angle = c(tan(pi/2), tan(0/2), tan(0/2), log(0.5), log(0.5), log(3))),
      fixPar = list(angle = c(tan(pi/2), tan(0/2), tan(0/2), NA, NA, NA))
    )
    
    scale_step_parameters_to_resampling_ratio(run_parameters, alpage, params)
  })
  names(run_parameters_list) <- names(sampling_parameters_list)
  
  
  
  # ! Check the internet connection
  startTime <- Sys.time()
  results <- par_HMM_fit(data, run_parameters_list, ncores,individual_info_file, sampling_table,output_dir,pdf_dir = hmm_pdf_case)
  endTime <- Sys.time()
  
  keep_cols <- c("ID","time","x","y","hour","state","state_proba","alpage","species","race")
  
  data_hmm <- do.call(rbind, lapply(results, function(r) {
    d <- r$data
    for (cc in keep_cols) if (!cc %in% names(d)) d[[cc]] <- NA
    d <- d[, keep_cols, drop = FALSE]
    d
  }))
   
  viterbi_trajectory_to_rds(data_hmm, output_rds_file, individual_info_file)
  
}

#### 5. FLOCK STOCKING RATE (charge) BY DAY AND BY STATE ####
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
  input_rds_file <- file.path(output_dir, "4. HMM_comportement",  paste0("Catlog_", YEAR, "_", alpage, "_viterbi.rds"))
  data <- readRDS(input_rds_file)
  str(data)
  data %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    n = dplyr::n(),
    na_time = sum(is.na(time)),
    na_x    = sum(is.na(x)),
    na_y    = sum(is.na(y))
  ) %>%
  dplyr::arrange(dplyr::desc(na_time + na_x + na_y)) %>%
  print(n = 30)

  # A data.frame containing herd sizes and their changes over time based on the date
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  flock_size_file <- file.path(raw_data_dir, paste0(YEAR, "_tailles_troupeaux.csv"))
  check_and_correct_csv(csv_path = flock_size_file)
  
  
  # A case with Pastoral unite shapefile (UP)
  case_UP_file = file.path(raster_dir, "UP")
  # Un .SHP avec les Unités pastorales UP
  UP_file = file.path(case_UP_file, paste0("UP_",alpage,".shp"))
  
  
  ## OUTPUTS ##
  # Output folder
  
  save_dir <- file.path(output_dir, "5. Stocking_rate")
  
  # One .RDS per alpine pasture containing the daily loads by behavior
  state_daily_rds_prefix <- paste0("by_day_and_state_", YEAR, "_")
  # One .RDS per alpine pasture containing the daily loads
  daily_rds_prefix <- paste0("by_day_", YEAR, "_")
  # One .RDS per alpine pasture containing the loads by behavior
  state_rds_prefix <- paste0("by_state_", YEAR, "_")
  # One .RDS per alpine pasture containing the total load over the entire season
  
  total_rds_prefix <- paste0("total_", YEAR, "_")
  
  # rast of the template 
  template_case = file.path(raster_dir, "template")
  if(!dir.exists(template_case)) {dir.create(template_case, recursive = TRUE)}
  output_rast_file = file.path(template_case, paste0("template_",alpage,".tif"))
  
  
  
  ## CODE ##
  # creation of a template base on the UP (Pastoral unity)
  res_m <- 10
  buffer_m <- 100
  up <- terra::vect(UP_file)
  up <- terra::project(up, "EPSG:2154")
  e <- terra::ext(up)
  e <- terra::ext(e$xmin-buffer_m, e$xmax+buffer_m,
                  e$ymin-buffer_m, e$ymax+buffer_m)
  snap_down <- function(v, res) floor(v / res) * res
  snap_up   <- function(v, res) ceiling(v / res) * res
  
  e <- terra::ext(
    snap_down(e$xmin, res_m), snap_up(e$xmax, res_m),
    snap_down(e$ymin, res_m), snap_up(e$ymax, res_m)
  )
  template <- terra::rast(e, res = res_m, crs = "EPSG:2154")
  terra::values(template) <- 0
  terra::writeRaster(template, output_rast_file, overwrite = TRUE)
  
  
  h <- 25 # Characteristic distance for calculating stocking

  for (alpage in alpages) {
    flock_sizes <- get_flock_size_through_time(alpage, flock_size_file)
    sampling_info <- readRDS(file.path(output_dir, "0. Sampling_Periods", paste0("Sampling_periods_", YEAR, "_", alpage, ".rds")))
    
    prop_time_collar_on <- weighted.mean(sampling_info$proportion_jour_allume, sampling_info$n_days,na.rm = TRUE  )
    print(paste("Mean proportion of collar activity for", alpage, "=",round(prop_time_collar_on, 3)))
    
    # Loading the filtered data for the alpine pasture
      data <- readRDS(input_rds_file)
    data <- data[data$alpage == alpage,]
    
    # Definition of the storage folder specific to the alpine pasture
    alpage_save_dir <- file.path(save_dir, paste0(YEAR, "_", alpage))
    if (!dir.exists(alpage_save_dir)) dir.create(alpage_save_dir, recursive = TRUE)
    
    
    # BY day and by state 
    # Calculation of stocking based on the NDVI raster (unsuitable for other users)
    r_template <- raster::raster(output_rast_file)
    grid_sp <- as(r_template, "SpatialPixelsDataFrame")
    flock_load_by_day_and_state_to_rds_kernelbb_grid(data, grid_sp, alpage_save_dir, state_daily_rds_prefix, flock_sizes, prop_time_collar_on)
    
    # Merging individual files
    merged_file <- flock_merge_rds_files(alpage_save_dir, state_daily_rds_prefix, alpage)
    
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
    case_flock_file = file.path(output_dir, "5. Stocking_rate")
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
    output_visu_case <- file.path(output_dir, "6. Indicateurs_visualisation")
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
    
    
    
    d<-readRDS(total_rds_prefix)
    summary(d)
    # CODE
    
    #Indicateur : Charge total .TIF
    if (TRUE) {
      total_flock_load_tif(total_rds_prefix, output_flock_tot_tif, output_flock_tot_tif_crop, UP_file, alpage, alpage_info_file)
    }
    
    
    #Indicateur : Charge_by_state
    if (FALSE) {
      state_flock_load_tif(state_rds_prefix,output_flock_repos_tif,output_flock_deplacement_tif, output_flock_paturage_tif,
                           output_flock_repos_tif_crop, output_flock_deplacement_tif_crop , output_flock_paturage_tif_crop,
                           UP_file, alpage, alpage_info_file)
    }
    
    #Indicateur : Charge_by_day
    if (FALSE){
      res_raster <- 10 # ou la valeur que tu souhaites explicitement
      day_flock_load_tif_nostack(daily_rds_prefix, output_case_alpage, UP_file, alpage, alpage_info_file, YEAR, res_raster, CROP = "YES")
      
      
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
    }
    
  }
  
}

#### 6. Calcul de la distance et du denivelé ####
#-----------------------------------------------#
if (TRUE) {
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
    case_state_file = file.path(output_dir, "4. HMM_comportement")
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
if (FALSE) {
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  count_nb_grazing_days <- function(daily_rds_prefix,
                                    output_nb_grazing_rds,
                                    output_nb_grazing_tif,
                                    threshold = 10) {
    # Chargement des librairies nécessaires
    library(dplyr)
    library(raster)
    threshold = 1
    # Lecture des données journalières
    daily_data <- readRDS(daily_rds_prefix)
    
    # Vérification de la présence des colonnes nécessaires
    required_cols <- c("x", "y", "day", "Charge", "state")
    if (!all(required_cols %in% names(daily_data))) {
      stop("Les colonnes x, y, day, Charge ou state sont manquantes dans les données.")
    }
    
    # 1) Filtrer pour ne garder que l'état 'Paturage'
    # 2) Agréger par pixel et jour pour obtenir la somme de Charge du jour
    # 3) Créer un flag (1/0) si la Charge dépasse le threshold
    grazing_data <- daily_data %>%
      filter(state == "Paturage") %>%
      group_by(x, y, day) %>%
      summarize(Charge_day = sum(Charge, na.rm = TRUE), .groups = "drop") %>%
      mutate(grazing_flag = if_else(Charge_day >= threshold, 1, 0))
    
    # 4) Calcul du nombre total de jours paturés par pixel
    nb_grazing_data <- grazing_data %>%
      group_by(x, y) %>%
      summarize(nb_grazing_day = sum(grazing_flag), .groups = "drop")
    
    # Sauvegarde du résultat au format RDS
    saveRDS(nb_grazing_data, file = output_nb_grazing_rds)
    cat("Fichier RDS créé avec 'nb_grazing_day' :", output_nb_grazing_rds, "\n")
    
    # Conversion en data.frame si nécessaire
    nb_grazing_data_df <- as.data.frame(nb_grazing_data)
    
    # 5) Création du raster : 1ère col = x, 2ème = y, 3ème = nb_grazing_day
    rast <- rasterFromXYZ(nb_grazing_data_df, crs = CRS("+init=epsg:2154"))
    
    # 6) Export du raster
    writeRaster(rast, filename = output_nb_grazing_tif, format = "GTiff", overwrite = TRUE)
    cat("Raster sauvegardé avec succès :", output_nb_grazing_tif, "\n")
    gc()
    return(nb_grazing_data)
  }
  
  
  
  
}

#### 8. Vecteur du comportement ####
#----------------------------------#  
if (FALSE){
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
      generate_trajectory_gpkg_catlog(state_rds_file,output_state_traj_case,YEAR,alpage,sampling_interval = 20)#Point toute les 30 minutes (reglé de base a 10)
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
