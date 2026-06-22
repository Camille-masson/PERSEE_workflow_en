#### 0. LIBRARIES AND CONSTANTS ####
#----------------------------------#
gc()

# Loading configuration
source("config.R")

# Definition of the analysis year and the alpine pastures to process
YEAR = 2024
alpage = "Viso"
alpages = "Viso"

ALPAGES_TOTAL <- list(
  "9999" = c("Alpage_demo"),
  "2022" = c("Ane-et-Buyant", "Cayolle", "Combe-Madame", "Grande-Fesse", "Jas-des-Lievres", "Lanchatra", "Pelvas", "Sanguiniere", "Viso"),
  "2023" = c("Cayolle", "Crouzet", "Grande-Cabane", "Lanchatra", "Rouanette", "Sanguiniere", "Vacherie-de-Roubion", "Viso"),
  "2024" = c("Viso", "Cayolle", "Sanguiniere","Crouzet","Grande-Cabane"),
  "2025" = c("Viso", "Cayolle", "Sanguiniere", "Ponsonniere", "Mantet","Sanguiniere","Grande-Cabane")
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
    collar_files <- list.files(collar_dir,pattern = "\\.csv$",full.names = TRUE,ignore.case = TRUE) 
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
  minPts <- 80   # ajuste 10-80
  
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
    
    trk <- make_track(sub, x, y, time, crs = 2154, all_cols = TRUE)
    
    hr  <- hr_kde(trk, trast = template)          # lissage stable
    iso <- hr_isopleths(hr, levels = 0.9)                    # contour "parc"
    
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
  
  
  
  
  d_end$doy <- yday(d_end$time)
  
  df <- d_end %>%
    dplyr::count(doy, park, name = "n") %>%
    dplyr::group_by(doy) %>%
    dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(doy, park)
  
  ggplot(df, aes(x = doy, y = park, colour = park)) +
    geom_point(size = 2, alpha = 0.9) +
    labs(x = "DOY", y = NULL, colour = "Nigth_Park") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
  
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
  source(file.path(functions_dir, "Functions_check_metadata.R"))
  
  ## INPUTS ##
  nightpark_output_dir <- file.path(output_dir, "3. Night_park")
  if (!dir.exists(nightpark_output_dir)) {dir.create(nightpark_output_dir, recursive = TRUE)}
  
  # An .RDS file containing the trajectories filtered in 2.2
  input_rds_file <- file.path(output_dir, "3. Night_park", paste0("Catlog_",YEAR,"_night_park_",alpage,".rds"))
  
  # A data.frame containing the correspondence between collars and alpine pastures
  individual_info_file <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"), paste0(YEAR, "_colliers_poses.csv"))
  check_and_correct_csv(csv_path = individual_info_file)
  
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

#### 5. FLOCK STOCKING RATE BY DAY BY STATE AND BY PARK ####
#----------------------------------------------------------#
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
  
  # A .csv file "infos_alpages" filled in according to the demo dataset template
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  check_and_correct_csv(csv_path = AIF)
  
  # A data.frame containing herd sizes and their changes over time based on the date
  flock_size_file <- file.path(raw_data_dir, paste0(YEAR, "_tailles_troupeaux.csv"))
  check_and_correct_csv(csv_path = flock_size_file)
  
  # rast of the template 
  template_case = file.path(raster_dir, "template")
  grid = file.path(template_case, paste0("template_",alpage,".tif"))
  ## OPTIONAL CHECK
  #str(read.csv(flock_size_file, stringsAsFactors = FALSE, encoding = "UTF-8"))
  
  
  ## OUTPUTS ##
  # Output folder
  
  save_dir <- file.path(output_dir, "5. Stocking_rate")
  alpage_save_dir <- file.path(save_dir, paste0(YEAR, "_", alpage))
  if (!dir.exists(alpage_save_dir)) dir.create(alpage_save_dir, recursive = TRUE)
  
  # One .RDS per alpine pasture containing the daily loads by behavior
  state_daily_rds_prefix <- paste0("by_day_and_state_", YEAR, "_")
  # One .RDS per alpine pasture containing the daily loads by behavior and park 
  state_daily_park_rds_prefix <- paste0("by_day_state_and_park_", YEAR, "_")
  # One .RDS per alpine pasture containing the daily loads
  daily_rds_prefix <- paste0("by_day_", YEAR, "_")
  # One .RDS per alpine pasture containing the loads by behavior
  state_rds_prefix <- paste0("by_state_", YEAR, "_")
  # One .RDS per alpine pasture containing the total load over the entire season
  total_rds_prefix <- paste0("total_", YEAR, "_")
  # One .RDS per day and grazing sector
  day_park_rds_prefix <- paste0("by_day_and_park_", YEAR, "_")
  # One .RDS per grazing sector
  park_rds_prefix <- paste0("by_park_", YEAR, "_")
  
  ## CODE ##
  h <- 10 # Characteristic distance for calculating stocking
  
  for (alpage in alpages) {
    flock_sizes <- get_flock_size_through_time(alpage, flock_size_file)
    prop_time_collar_on <- get_alpage_info(alpage, AIF, "proportion_jour_allume")
    
    # Loading the filtered data for the alpine pasture
    data <- readRDS(input_rds_file)
    data <- data[data$alpage == alpage,]
    
    # Definition of the storage folder specific to the alpine pasture
    alpage_save_dir <- file.path(save_dir, paste0(YEAR, "_", alpage))
    if (!dir.exists(alpage_save_dir)) dir.create(alpage_save_dir, recursive = TRUE)
    
    # Calculation of stocking based on the NDVI raster (unsuitable for other users)
    template_file <- file.path(template_case, paste0("template_", alpage, ".tif"))
    r_template <- raster::raster(template_file)
    grid_sp <- as(r_template, "SpatialPixelsDataFrame")
    flock_load_by_day_and_state_to_rds_kernelbb_grid(data, grid_sp, alpage_save_dir, state_daily_rds_prefix, flock_sizes, prop_time_collar_on)
    
    
    # Merging individual files
    merged_file <- flock_merge_rds_files(alpage_save_dir, state_daily_rds_prefix, alpage)
    
    rm(data)
    
    # By day, state and park
    charge <- readRDS(file.path(alpage_save_dir, paste0(state_daily_rds_prefix, alpage, ".rds")))
    unique(charge$state)
    park <- readRDS(input_rds_file)
    park <- park %>%
      dplyr::mutate(day = lubridate::yday(time)) %>%
      dplyr::count(day, park, name = "n") %>%
      dplyr::group_by(day) %>%
      dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(day, park)
    
    by_day_state_park<- charge %>%
      left_join(park, by = "day")
    
    saveRDS(by_day_state_park, file.path(alpage_save_dir, paste0(state_daily_park_rds_prefix, alpage, ".rds")))
    
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
    
    # By park and day
    
    
    charge <- charge %>%
      left_join(park, by = "day")
    
    charge_day_park <- charge %>%
      group_by(x, y, day, park) %>%
      summarise(Charge = sum(Charge, na.rm = TRUE), .groups = "drop") %>%
      as.data.frame()
    
    saveRDS(charge_day_park, file.path(alpage_save_dir, paste0(day_park_rds_prefix, alpage, ".rds")))
    rm(charge_day_park)
    
    # By park
    
    charge_park <- charge %>%
      group_by(x, y, park) %>%
      summarise(Charge = sum(Charge, na.rm = TRUE), .groups = "drop") %>%
      as.data.frame()
    
    saveRDS(charge_park, file.path(alpage_save_dir, paste0(park_rds_prefix, alpage, ".rds")))
    rm(charge_park)
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
    case_flock_file = file.path(output_dir, "5. Stocking_rate")
    #Dossier contenant les fichiers du tot de chargement
    case_flock_alpage_file = file.path(case_flock_file,paste0(YEAR,"_",alpage))
    
    # Un .RDS par alpage contenant les charges journalières
    daily_rds_file = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
    
    # OUTPUT
    
    #Création du dossier de sortie des indicateur pour la visualistaion
    output_visu_case <- file.path(output_dir, "6. Indicateurs_visualisation")
    if (!dir.exists(output_visu_case)) {
      dir.create(output_visu_case, recursive = TRUE)
    }
    #Création du sous-dossier Indicateur traitée : Chargement
    output_day_grazing_case <- file.path(output_visu_case, "Number_Grazing_day")
    if (!dir.exists(output_day_grazing_case)) {
      dir.create(output_day_grazing_case, recursive = TRUE)
    }
    #Création du sous-sous-dossier alpage et années traitée : Chargement
    output_case_alpage <- file.path(output_day_grazing_case, paste0(alpage))
    if (!dir.exists(output_case_alpage)) {
      dir.create(output_case_alpage, recursive = TRUE)
    }
    
    # Un .TIF par alpage contenant les charges journalières
    output_nb_grazing_day_tif = file.path(output_case_alpage, paste0("number_grazing_day_",YEAR,"_",alpage,".tif"))
    
    
    ## CODE
    
    nb_grazing_day(daily_rds_file, output_nb_grazing_day_tif, seuil = 1)
    
    
  }
  
   #### 7. Calcul du jour du pic de chargement ####
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
    output_day_grazing_case <- file.path(output_visu_case, "Date_of_stocking_rate_peak")
    if (!dir.exists(output_day_grazing_case)) {
      dir.create(output_day_grazing_case, recursive = TRUE)
    }
    #Création du sous-sous-dossier alpage et années traitée : Chargement
    output_case_alpage <- file.path(output_day_grazing_case, paste0(alpage))
    if (!dir.exists(output_case_alpage)) {
      dir.create(output_case_alpage, recursive = TRUE)
    }
    
    # Un .TIF par alpage contenant les charges journalières
    date_of_stocking_rate_peak_tif = file.path(output_case_alpage, paste0("date_of_stocking_rate_peak_",YEAR,"_",alpage,".tif"))
    
    
    ## CODE
    
    date_of_stocking_rate_peak(daily_rds_file,date_of_stocking_rate_peak_tif)
    
    
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
      
      #dtm
      dtm_case <- file.path(raster_dir, "dtm")
      if (!dir.exists(dtm_case)) dir.create(dtm_case, recursive = TRUE)
      mnt_file <- file.path(dtm_case, paste0("DTM_1_", alpage, ".tif"))
      
      
      # SORTIE 
      
      # Création du dossier de sortie des indicateur pour la visualistaion
      output_visu_case <- file.path(output_dir, "6. Indicateurs_visualisation")
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
      
      save_distance_denivele(
        state_rds_file,
        distance_csv_file,
        mnt_file,              # chemin du raster
        output_distance_case,
        YEAR,
        alpage
      )
      
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
      case_state_file = file.path(output_dir, "4. HMM_comportement")
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
      output_visu_case <- file.path(output_dir, "6. Indicateurs_visualisation")
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
      case_state_file = file.path(output_dir, "4. HMM_comportement")
      # Un .RDS contenant les trajectoires (filtrées, éventuellement sous-échantillonnées)
      state_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi.rds"))
      
      
      # Un dossier contenant les ratsers des Unités Pastorales (UP)
      case_UP_file = file.path(raster_dir, "UP")
      # Un .SHP avec les Unités pastorales UP
      UP_file = file.path(case_UP_file, paste0("UP_",alpage,".shp"))
      
      # Un dossier contenant les Infos sur les alpages
      raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
      # Un data.frame contenant les dates de pose et de retrait des colliers
      AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
      
      # SORTIE 
      # Création du dossier de sortie des indicateur pour la visualistaion
      output_visu_case <- file.path(output_dir, "6. Indicateurs_visualisation")
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
      output_polygon_use_month_shp = file.path(output_polygon_case, paste0("Use_polygon_month_",YEAR,"_",alpage,".shp"))
      
      
      # CODE 
      # Ancienne fonction
      if (FALSE){
        generate_presence_polygons(state_rds_file,output_polygon_use_shp,YEAR,alpage,density_threshold = 1e-06,n_grid = 200,small_poly_threshold_percent = 0.05)
        # Seuil pour filtrer les petits polygones
        # Ajustez pour être plus ou moins restrictif
      }
      
      if(FALSE){
        generate_presence_polygons_by_percentage(state_rds_file, output_polygon_use_shp, YEAR, alpage, percentage = 0.85, n_grid = 200,small_poly_threshold_percent = 0.05 ,crs = 2154 )
        
        
      }
      
      
      if (TRUE){
        generate_presence_polygons_by_percentage_per_month(state_rds_file, output_polygon_use_month_shp, YEAR, alpage, percentage = 0.85 ,n_grid = 200,small_poly_threshold_percent = 0.05 ,crs = 2154 )  
      } 
      
      
      
      
    }
    
    
  }
  
}
    





















library(terra)

# paramètres
res_m    <- 10
buffer_m <- 100

# lire UP
up <- terra::vect(UP_file)
up <- terra::project(up, "EPSG:2154")

# extent UP + buffer
e <- terra::ext(up)
e <- terra::ext(
  e$xmin - buffer_m,
  e$xmax + buffer_m,
  e$ymin - buffer_m,
  e$ymax + buffer_m
)

# snap sur la résolution
snap_down <- function(v, res) floor(v / res) * res
snap_up   <- function(v, res) ceiling(v / res) * res

e <- terra::ext(
  snap_down(e$xmin, res_m),
  snap_up(e$xmax, res_m),
  snap_down(e$ymin, res_m),
  snap_up(e$ymax, res_m)
)

# créer template
template <- terra::rast(e, res = res_m, crs = "EPSG:2154")
terra::values(template) <- 1

# écrire raster
terra::writeRaster(template, output_rast_file, overwrite = TRUE)







diagnose_snow_projection <- function(
    input_stack_file,
    AOI_file,
    input_meta_file = NULL,
    START_VIS = NULL,
    END_VIS = NULL,
    TARGET_CRS = "EPSG:2154",
    TARGET_RES = 10,
    SOURCE_CRS = NULL,
    pad_x = 600,
    pad_y = 200,
    test_layer = NULL
) {
  
  library(terra)
  
  cat("\n==============================\n")
  cat("DIAGNOSTIC REPROJECTION NEIGE\n")
  cat("==============================\n\n")
  
  #----------------------------
  # Helpers
  #----------------------------
  snap_down <- function(v, res) floor(v / res) * res
  snap_up   <- function(v, res) ceiling(v / res) * res
  
  print_rast_info <- function(r, name) {
    cat("\n---", name, "---\n")
    cat("class      :", class(r)[1], "\n")
    cat("nlyr       :", terra::nlyr(r), "\n")
    cat("crs        :", terra::crs(r), "\n")
    cat("res        :", paste(terra::res(r), collapse = " / "), "\n")
    cat("ext        :\n")
    print(terra::ext(r))
    cat("names head :\n")
    print(utils::head(names(r), 5))
  }
  
  print_vect_info <- function(v, name) {
    cat("\n---", name, "---\n")
    cat("class      :", class(v)[1], "\n")
    cat("ngeom      :", length(v), "\n")
    cat("crs        :", terra::crs(v), "\n")
    cat("ext        :\n")
    print(terra::ext(v))
  }
  
  ext_overlap <- function(e1, e2) {
    !(
      terra::xmax(e1) <= terra::xmin(e2) ||
        terra::xmin(e1) >= terra::xmax(e2) ||
        terra::ymax(e1) <= terra::ymin(e2) ||
        terra::ymin(e1) >= terra::ymax(e2)
    )
  }
  
  print_values <- function(r, name) {
    cat("\n--- VALEURS :", name, "---\n")
    cat("global range:\n")
    print(terra::global(r, "range", na.rm = TRUE))
    cat("global mean:\n")
    print(terra::global(r, "mean", na.rm = TRUE))
    cat("freq, si raster catégoriel:\n")
    print(try(terra::freq(r, digits = 0), silent = TRUE))
  }
  
  #----------------------------
  # 1) Lecture stack
  #----------------------------
  stack_raw <- terra::rast(input_stack_file)
  
  if (is.na(terra::crs(stack_raw)) || terra::crs(stack_raw) == "") {
    cat("\n!!! CRS du stack absent.\n")
    if (is.null(SOURCE_CRS)) {
      stop("SOURCE_CRS est NULL. Il faut le renseigner pour diagnostiquer correctement.")
    } else {
      cat("Assignation manuelle du CRS source :", SOURCE_CRS, "\n")
      terra::crs(stack_raw) <- SOURCE_CRS
    }
  }
  
  print_rast_info(stack_raw, "STACK RAW")
  
  #----------------------------
  # 2) Lecture AOI
  #----------------------------
  AOI_raw <- terra::vect(AOI_file)
  
  if (is.na(terra::crs(AOI_raw)) || terra::crs(AOI_raw) == "") {
    stop("Le CRS de l'AOI est absent. Il faut corriger le .prj ou assigner le CRS.")
  }
  
  print_vect_info(AOI_raw, "AOI RAW")
  
  #----------------------------
  # 3) AOI en TARGET_CRS
  #----------------------------
  AOI_target <- terra::project(AOI_raw, TARGET_CRS)
  print_vect_info(AOI_target, "AOI TARGET")
  
  #----------------------------
  # 4) Template depuis AOI
  #----------------------------
  e_aoi <- terra::ext(AOI_target)
  
  e_raw <- terra::ext(
    terra::xmin(e_aoi) - pad_x,
    terra::xmax(e_aoi) + pad_x,
    terra::ymin(e_aoi) - pad_y,
    terra::ymax(e_aoi) + pad_y
  )
  
  e_template <- terra::ext(
    snap_down(terra::xmin(e_raw), TARGET_RES),
    snap_up(terra::xmax(e_raw), TARGET_RES),
    snap_down(terra::ymin(e_raw), TARGET_RES),
    snap_up(terra::ymax(e_raw), TARGET_RES)
  )
  
  template <- terra::rast(
    e_template,
    resolution = TARGET_RES,
    crs = TARGET_CRS
  )
  
  terra::values(template) <- NA
  
  print_rast_info(template, "TEMPLATE TARGET 10 m")
  
  #----------------------------
  # 5) Choix couche test
  #----------------------------
  if (!is.null(input_meta_file) && file.exists(input_meta_file)) {
    
    meta <- read.csv(input_meta_file)
    meta$DATE <- as.Date(meta$DATE)
    
    if (!is.null(START_VIS) && !is.null(END_VIS)) {
      START_VIS <- as.Date(START_VIS)
      END_VIS <- as.Date(END_VIS)
      meta_vis <- meta[meta$DATE >= START_VIS & meta$DATE <= END_VIS, ]
      meta_vis <- meta_vis[order(meta_vis$DATE), ]
    } else {
      meta_vis <- meta
    }
    
    cat("\n--- META ---\n")
    cat("nrow meta     :", nrow(meta), "\n")
    cat("nrow meta_vis :", nrow(meta_vis), "\n")
    print(utils::head(meta_vis, 5))
    
    if (is.null(test_layer)) {
      test_layer <- meta_vis$layer[1]
    }
  }
  
  if (is.null(test_layer)) {
    test_layer <- names(stack_raw)[1]
  }
  
  cat("\nCouche test choisie :", test_layer, "\n")
  
  if (!test_layer %in% names(stack_raw)) {
    stop("La couche test n'existe pas dans le stack : ", test_layer)
  }
  
  r_raw <- stack_raw[[test_layer]]
  print_values(r_raw, paste0(test_layer, " RAW"))
  
  #----------------------------
  # 6) Test overlap stack raw / AOI reprojetée dans CRS stack
  #----------------------------
  AOI_in_stack_crs <- terra::project(AOI_raw, terra::crs(stack_raw))
  template_poly_target <- terra::as.polygons(terra::ext(template), crs = terra::crs(template))
  template_poly_stack_crs <- terra::project(template_poly_target, terra::crs(stack_raw))
  
  cat("\n--- OVERLAP SOURCE CRS ---\n")
  cat("Overlap stack_raw / AOI_in_stack_crs      :",
      ext_overlap(terra::ext(stack_raw), terra::ext(AOI_in_stack_crs)), "\n")
  cat("Overlap stack_raw / template_in_stack_crs :",
      ext_overlap(terra::ext(stack_raw), terra::ext(template_poly_stack_crs)), "\n")
  
  cat("\nExtent AOI in stack CRS:\n")
  print(terra::ext(AOI_in_stack_crs))
  
  cat("\nExtent template in stack CRS:\n")
  print(terra::ext(template_poly_stack_crs))
  
  #----------------------------
  # 7) Crop source avant projection
  #----------------------------
  cat("\n--- CROP AVANT PROJECTION ---\n")
  
  r_crop_src <- try(
    terra::crop(r_raw, template_poly_stack_crs, snap = "out"),
    silent = TRUE
  )
  
  if (inherits(r_crop_src, "try-error")) {
    cat("ERREUR crop source :\n")
    print(r_crop_src)
  } else {
    print_rast_info(r_crop_src, "R CROP SOURCE")
    print_values(r_crop_src, paste0(test_layer, " CROP SOURCE"))
  }
  
  #----------------------------
  # 8) Projection directe vers template
  #----------------------------
  cat("\n--- PROJECTION VERS TEMPLATE ---\n")
  
  r_proj_direct <- try(
    terra::project(r_raw, template, method = "near"),
    silent = TRUE
  )
  
  if (inherits(r_proj_direct, "try-error")) {
    cat("ERREUR project direct :\n")
    print(r_proj_direct)
  } else {
    print_rast_info(r_proj_direct, "R PROJECT DIRECT TO TEMPLATE")
    print_values(r_proj_direct, paste0(test_layer, " PROJECT DIRECT"))
  }
  
  #----------------------------
  # 9) Crop puis projection
  #----------------------------
  cat("\n--- CROP SOURCE PUIS PROJECTION ---\n")
  
  if (!inherits(r_crop_src, "try-error")) {
    
    r_proj_crop <- try(
      terra::project(r_crop_src, template, method = "near"),
      silent = TRUE
    )
    
    if (inherits(r_proj_crop, "try-error")) {
      cat("ERREUR project après crop :\n")
      print(r_proj_crop)
    } else {
      print_rast_info(r_proj_crop, "R CROP + PROJECT TO TEMPLATE")
      print_values(r_proj_crop, paste0(test_layer, " CROP + PROJECT"))
    }
  }
  
  #----------------------------
  # 10) Masque AOI et fSCA
  #----------------------------
  cat("\n--- TEST AOI MASK ET FSCA ---\n")
  
  aoi_mask <- terra::rasterize(
    AOI_target,
    template,
    field = 1,
    touches = TRUE
  )
  
  print_values(aoi_mask, "AOI MASK")
  
  if (!inherits(r_proj_direct, "try-error")) {
    r_aoi <- terra::mask(r_proj_direct, aoi_mask)
    print_values(r_aoi, paste0(test_layer, " PROJECT DIRECT + MASK AOI"))
    
    fsca <- as.numeric(terra::global(r_aoi, "mean", na.rm = TRUE)[1, 1]) * 100
    cat("\nfSCA test direct + mask AOI =", fsca, "%\n")
  }
  
  #----------------------------
  # 11) Petit plot diagnostic
  #----------------------------
  cat("\n--- PLOT DIAGNOSTIC ---\n")
  cat("Un plot devrait s'ouvrir avec raw, projeté, projeté+AOI.\n")
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  
  par(mfrow = c(1, 3), mar = c(3, 3, 3, 4))
  
  plot(r_raw, main = paste0("RAW\n", test_layer))
  plot(AOI_in_stack_crs, add = TRUE, border = "red", lwd = 2)
  
  if (!inherits(r_proj_direct, "try-error")) {
    plot(r_proj_direct, main = "PROJECT DIRECT\nvers template")
    plot(AOI_target, add = TRUE, border = "red", lwd = 2)
    
    r_aoi <- terra::mask(r_proj_direct, aoi_mask)
    plot(r_aoi, main = "PROJECT + MASK AOI")
    plot(AOI_target, add = TRUE, border = "red", lwd = 2)
  }
  
  cat("\n==============================\n")
  cat("FIN DIAGNOSTIC\n")
  cat("==============================\n")
  
  invisible(list(
    stack_raw = stack_raw,
    AOI_raw = AOI_raw,
    AOI_target = AOI_target,
    template = template,
    test_layer = test_layer,
    r_raw = r_raw,
    r_proj_direct = if (!inherits(r_proj_direct, "try-error")) r_proj_direct else NULL,
    aoi_mask = aoi_mask
  ))
}



diag <- diagnose_snow_projection(
  input_stack_file = input_stack_file,
  AOI_file         = AOI_file,
  input_meta_file  = input_meta_file,
  START_VIS        = START_VIS,
  END_VIS          = END_VIS,
  TARGET_CRS       = "EPSG:2154",
  TARGET_RES       = 10,
  SOURCE_CRS       = NULL,
  pad_x            = 600,
  pad_y            = 200
)    
