


# Fonction Chargement total brut et crop
total_flock_load_tif <- function(total_rds_prefix, output_flock_tot_tif, output_flock_tot_tif_crop, UP_file, alpage, alpage_info_file, res_raster = 10, CROP = "YES") {
  
  # Chargement des données
  charge_data <- readRDS(total_rds_prefix)
  
  # Vérification des colonnes nécessaires
  if (!all(c("x", "y", "Charge") %in% names(charge_data))) {
    stop("Les colonnes x, y ou Charge sont manquantes dans les données.")
  }
  
  # Conversion en SpatialPointsDataFrame (Lambert 93)
  coordinates(charge_data) <- ~ x + y
  crs(charge_data) <- CRS("+init=epsg:2154")
  
  # Création du raster vide avec résolution définie
  raster_template <- raster(extent(charge_data), resolution = res_raster, crs = crs(charge_data))
  
  # Rasteriser les données
  charge_raster <- rasterize(charge_data, raster_template, field = "Charge", fun = mean, background = NA)
  
  
  
  # Export du raster complet
  if (file.exists(output_flock_tot_tif)) file.remove(output_flock_tot_tif)
  writeRaster(charge_raster, filename = output_flock_tot_tif, format = "GTiff", overwrite = TRUE)
  cat("Raster complet sauvegardé avec succès :", output_flock_tot_tif, "\n")
  
  # Gestion du raster découpé si demandé
  if (toupper(CROP) == "YES") {
    UP_selected <- get_UP_shp(alpage, alpage_info_file, UP_file)
    
    if (nrow(UP_selected) == 0) {
      warning("Aucune Unité Pastorale trouvée pour l'alpage spécifié. Le raster crop ne sera pas généré.")
    } else {
      charge_raster_crop <- mask(crop(charge_raster, UP_selected), UP_selected)
      
      # Suppression préalable si nécessaire
      if (file.exists(output_flock_tot_tif_crop)) file.remove(output_flock_tot_tif_crop)
      
      # Export du raster crop en GeoTIFF
      writeRaster(charge_raster_crop, filename = output_flock_tot_tif_crop, format = "GTiff", overwrite = TRUE)
      cat("Raster découpé sauvegardé avec succès :", output_flock_tot_tif_crop, "\n")
    }
  }
}


# Fonction pour générer des rasters par comportement avec/sans découpage par UP
state_flock_load_tif <- function(state_rds_prefix, 
                                 output_flock_repos_tif, output_flock_deplacement_tif, output_flock_paturage_tif,
                                 output_flock_repos_tif_crop, output_flock_deplacement_tif_crop, output_flock_paturage_tif_crop,
                                 UP_file, alpage, alpage_info_file, res_raster = 10, CROP = "YES") {
  
  # Chargement des données
  charge_data <- readRDS(state_rds_prefix)
  
  # Vérification des colonnes nécessaires
  if (!all(c("x", "y", "Charge", "state") %in% names(charge_data))) {
    stop("Les colonnes x, y, Charge ou state sont manquantes dans les données.")
  }
  
  # Conversion en SpatialPointsDataFrame (Lambert 93)
  coordinates(charge_data) <- ~ x + y
  crs(charge_data) <- CRS("+init=epsg:2154")
  
  # Création du raster vide avec résolution définie
  raster_template <- raster(extent(charge_data), resolution = res_raster, crs = crs(charge_data))
  
  # Traitement par état (comportement)
  for(state_current in unique(charge_data$state)) {
    
    cat("Traitement du comportement :", state_current, "\n")
    
    # Filtrer les données par comportement
    data_state <- charge_data[charge_data$state == state_current, ]
    
    # Rasteriser les données
    charge_raster <- rasterize(data_state, raster_template, field = "Charge", fun = mean, background = NA)
    
    # Définir les chemins de sortie selon le comportement
    if (state_current == "Repos") {
      output_tif_file <- output_flock_repos_tif
      output_tif_crop_file <- output_flock_repos_tif_crop
    } else if (state_current == "Deplacement") {
      output_tif_file <- output_flock_deplacement_tif
      output_tif_crop_file <- output_flock_deplacement_tif_crop
    } else if (state_current == "Paturage") {
      output_tif_file <- output_flock_paturage_tif
      output_tif_crop_file <- output_flock_paturage_tif_crop
    } else {
      next # si comportement inconnu, ne pas traiter
    }
    
    # Export du raster complet
    if (file.exists(output_tif_file)) file.remove(output_tif_file)
    writeRaster(charge_raster, filename = output_tif_file, format = "GTiff", overwrite = TRUE)
    cat("Raster complet sauvegardé avec succès :", output_tif_file, "\n")
    
    # Gestion du raster découpé si demandé
    if (toupper(CROP) == "YES") {
      UP_selected <- get_UP_shp(alpage, alpage_info_file, UP_file)
      
      if (nrow(UP_selected) == 0) {
        warning("Aucune Unité Pastorale trouvée pour l'alpage spécifié. Le raster crop ne sera pas généré.")
      } else {
        charge_raster_crop <- mask(crop(charge_raster, UP_selected), UP_selected)
        
        # Export du raster crop
        if (file.exists(output_tif_crop_file)) file.remove(output_tif_crop_file)
        writeRaster(charge_raster_crop, filename = output_tif_crop_file, format = "GTiff", overwrite = TRUE)
        cat("Raster découpé sauvegardé avec succès :", output_tif_crop_file, "\n")
      }
    }
  }
}

### A REVOIR ###
library(raster)
library(sf)
library(dplyr)

# Fonction optimisée pour traiter jour par jour (mémoire réduite)
day_flock_load_tif <- function(daily_rds_file, output_case_alpage,
                               UP_file, alpage, alpage_info_file, YEAR, res_raster = 10, CROP = "NO") {
  
  # Chargement initial léger (uniquement les jours disponibles)
  all_days <- unique(readRDS(daily_rds_file)$day)
  
  raster_list <- list()
  
  for(day_current in all_days) {
    
    cat("Traitement du jour :", day_current, "\n")
    
    # Charger seulement les données nécessaires pour le jour courant
    charge_data <- readRDS(daily_rds_file) %>% filter(day == day_current)
    
    # Vérification des colonnes nécessaires
    if (!all(c("x", "y", "Charge") %in% names(charge_data))) {
      stop("Les colonnes x, y ou Charge sont manquantes pour le jour :", day_current)
    }
    
    # Conversion en SpatialPointsDataFrame (Lambert 93)
    coordinates(charge_data) <- ~ x + y
    crs(charge_data) <- CRS("+init=epsg:2154")
    
    # Création du raster vide (première itération)
    if (!exists("raster_template")) {
      raster_template <- raster(extent(charge_data), resolution = res_raster, crs = crs(charge_data))
    }
    
    # Rasteriser les données du jour
    charge_raster <- rasterize(charge_data, raster_template, field = "Charge", fun = mean, background = NA)
    
    # Sauvegarder temporairement chaque raster journalier en fichier intermédiaire
    raster_temp_file <- file.path(output_case_alpage, paste0("day_", day_current, "_", YEAR, "_", alpage, ".tif"))
    writeRaster(charge_raster, filename = raster_temp_file, format = "GTiff", overwrite = TRUE)
    
    raster_list[[length(raster_list) + 1]] <- charge_raster
    
    rm(charge_data, charge_raster)
    gc() # Nettoyage mémoire
  }
  
  # Empilement final des rasters journaliers
  stacked_raster <- stack(raster_list)
  stacked_output_file <- file.path(output_case_alpage, paste0("stacked_days_", YEAR, "_", alpage, ".tif"))
  writeRaster(stacked_raster, filename = stacked_output_file, format = "GTiff", overwrite = TRUE)
  cat("Raster journalier empilé sauvegardé avec succès :", stacked_output_file, "\n")
  
  # Crop optionnel selon l'UP
  if (toupper(CROP) == "YES") {
    UP_selected <- get_UP_shp(alpage, alpage_info_file, UP_file)
    
    if (nrow(UP_selected) == 0) {
      warning("Aucune Unité Pastorale trouvée pour l'alpage spécifié. Le raster crop ne sera pas généré.")
    } else {
      stacked_raster_crop <- mask(crop(stacked_raster, UP_selected), UP_selected)
      stacked_crop_output_file <- file.path(output_case_alpage, paste0("stacked_days_crop_", YEAR, "_", alpage, ".tif"))
      writeRaster(stacked_raster_crop, filename = stacked_crop_output_file, format = "GTiff", overwrite = TRUE)
      cat("Raster journalier découpé sauvegardé avec succès :", stacked_crop_output_file, "\n")
    }
  }
}





# Fonction optimisée pour traiter jour par jour (mémoire réduite)
day_flock_load_tif_new_a_revoir <- function(daily_rds_file, output_case_alpage,
                               UP_file, alpage, alpage_info_file, YEAR, res_raster = 10, CROP = "YES") {
  
  # Chargement initial léger (uniquement les jours disponibles)
  all_days <- unique(readRDS(daily_rds_file)$day)
  raster_list <- list()
  raster_files_intermediaire <- c()
  
  for(day_current in all_days) {
    
    cat("Traitement du jour :", day_current, "\n")
    
    # Charger seulement les données nécessaires pour le jour courant
    charge_data <- readRDS(daily_rds_file) %>% filter(day == day_current)
    
    # Vérification des colonnes nécessaires
    if (!all(c("x", "y", "Charge") %in% names(charge_data))) {
      stop("Les colonnes x, y ou Charge sont manquantes pour le jour :", day_current)
    }
    
    # Conversion en SpatialPointsDataFrame (Lambert 93)
    coordinates(charge_data) <- ~ x + y
    crs(charge_data) <- CRS("+init=epsg:2154")
    
    # Création du raster vide (première itération)
    if (!exists("raster_template")) {
      raster_template <- raster(extent(charge_data), resolution = res_raster, crs = crs(charge_data))
    }
    
    # Rasteriser les données du jour
    charge_raster <- rasterize(charge_data, raster_template, field = "Charge", fun = mean, background = NA)
    
    # Sauvegarder temporairement chaque raster journalier
    raster_temp_file <- file.path(output_case_alpage, paste0("day_", day_current, "_", YEAR, "_", alpage, ".tif"))
    writeRaster(charge_raster, filename = raster_temp_file, format = "GTiff", overwrite = TRUE)
    raster_files_intermediaire <- c(raster_files_intermediaire, raster_temp_file)
    
    raster_list[[paste0("Jour_", day_current)]] <- charge_raster
    
    rm(charge_data, charge_raster)
    gc() # Nettoyage mémoire
  }
  
  
  # Empilement final des rasters journaliers
  stacked_raster <- stack(raster_list)
  
  # Attribuer clairement les noms des bandes
  names(stacked_raster) <- paste0("Jour_", all_days)
  
  # Export du raster empilé
  stacked_output_file <- file.path(output_case_alpage, paste0("stacked_days_", YEAR, "_", alpage, ".tif"))
  writeRaster(stacked_raster, filename = stacked_output_file, format = "GTiff", overwrite = TRUE)
  cat("Raster journalier empilé sauvegardé avec succès :", stacked_output_file, "\n")
  # Crop optionnel selon l'UP
  if (toupper(CROP) == "YES") {
    UP_selected <- get_UP_shp(alpage, alpage_info_file, UP_file)
    
    if (nrow(UP_selected) == 0) {
      warning("Aucune Unité Pastorale trouvée pour l'alpage spécifié. Le raster crop ne sera pas généré.")
    } else {
      stacked_raster_crop <- mask(crop(stacked_raster, UP_selected), UP_selected)
      stacked_crop_output_file <- file.path(output_case_alpage, paste0("stacked_days_crop_", YEAR, "_", alpage, ".tif"))
      writeRaster(stacked_raster_crop, filename = stacked_crop_output_file, format = "GTiff", overwrite = TRUE)
      cat("Raster journalier découpé sauvegardé avec succès :", stacked_crop_output_file, "\n")
    }
  }
}



### FIN DE A REVOIR ###


# Fonction optimisée pour traiter jour par jour (mémoire réduite)

day_flock_load_tif_nostack <- function(daily_rds_file, output_case_alpage,
                               UP_file, alpage, alpage_info_file, YEAR, res_raster = 10, CROP = "NO") {
  
  # Chargement initial léger (uniquement les jours disponibles)
  all_days <- unique(readRDS(daily_rds_file)$day)
  
  for(day_current in all_days) {
    
    cat("Traitement du jour :", day_current, "\n")
    
    # Charger seulement les données nécessaires pour le jour courant
    charge_data <- readRDS(daily_rds_file) %>% filter(day == day_current)
    
    # Vérification des colonnes nécessaires
    if (!all(c("x", "y", "Charge") %in% names(charge_data))) {
      stop("Les colonnes x, y ou Charge sont manquantes pour le jour :", day_current)
    }
    
    # Conversion en SpatialPointsDataFrame (Lambert 93)
    coordinates(charge_data) <- ~ x + y
    crs(charge_data) <- CRS("+init=epsg:2154")
    
    # Création du raster vide (première itération)
    raster_template <- raster(extent(charge_data), resolution = res_raster, crs = crs(charge_data))
    
    # Rasteriser les données du jour
    charge_raster <- rasterize(charge_data, raster_template, field = "Charge", fun = mean, background = NA)
    
    # Sauvegarder chaque raster journalier sans empilement
    raster_temp_file <- file.path(output_case_alpage, paste0("day_", day_current, "_", YEAR, "_", alpage, ".tif"))
    writeRaster(charge_raster, filename = raster_temp_file, format = "GTiff", overwrite = TRUE)
    
    rm(charge_data, charge_raster)
    gc() # Nettoyage mémoire
  }
}





quinzaine_flock_load_tif_nostack <- function(daily_rds_file, output_case_alpage,
                                             UP_file, alpage, alpage_info_file,
                                             YEAR, res_raster = 10, CROP = "NO") {
  library(raster)
  library(dplyr)
  library(lubridate)
  
  # Charger toutes les données journalières
  all_data <- readRDS(daily_rds_file)
  
  # Ajout d'une colonne date (en supposant que 'day' est le numéro du jour dans l'année)
  all_data <- all_data %>%
    mutate(date = as.Date(day - 1, origin = paste0(YEAR, "-01-01")))
  
  # Définir les intervalles pour les quinzaines (exemple pour juin à septembre)
  date_breaks <- yday(as.POSIXct(
    c(paste0("16/06/", YEAR),
      paste0("01/07/", YEAR),
      paste0("16/07/", YEAR),
      paste0("01/08/", YEAR),
      paste0("16/08/", YEAR),
      paste0("01/09/", YEAR),
      paste0("16/09/", YEAR)),
    format = "%d/%m/%Y")
  )
  
  # Étiquettes de quinzaine sans espaces (pour faciliter les noms de fichiers)
  date_labs <- c("avant15_jun", "16_30_jun", "1_15_jul", "16_30_jul",
                 "1_15_aou", "16_30_aou", "1_15_sep", "apres16_sep")
  
  # Attribution de la période à chaque observation
  all_data <- all_data %>%
    mutate(yday = yday(date)) %>%
    mutate(month_period = factor(date_labs[findInterval(yday, date_breaks) + 1],
                                 levels = date_labs))
  
  # Création du raster template basé sur l'étendue des données et la résolution souhaitée
  xmin <- min(all_data$x)
  xmax <- max(all_data$x)
  ymin <- min(all_data$y)
  ymax <- max(all_data$y)
  r_template <- raster(extent(xmin, xmax, ymin, ymax), resolution = res_raster)
  
  # Boucle sur chaque quinzaine pour générer un raster de somme de Charge
  quinzaine_periods <- unique(all_data$month_period)
  for (p in quinzaine_periods) {
    cat("Traitement de la periode :", p, "\n")
    sub_data <- all_data %>% filter(month_period == p)
    if(nrow(sub_data) < 5) next  # Ignorer si pas assez de points
    
    # Préparer les coordonnées et le champ "Charge" sous forme numérique
    pts_coords <- as.matrix(sub_data[, c("x", "y")])
    pts_field <- as.numeric(sub_data$Charge)
    
    # Rasteriser les points : calcul de la somme de "Charge" par cellule
    charge_raster <- rasterize(pts_coords,
                               r_template,
                               field = pts_field,
                               fun = function(x, ...) sum(x, na.rm = TRUE),
                               background = NA)
    
    # Définir le nom du fichier de sortie pour cette quinzaine
    output_file <- file.path(output_case_alpage,
                             paste0("by_quinzaine_", YEAR, "_", alpage, "_", p, ".tif"))
    
    # Sauvegarder le raster en GeoTIFF
    writeRaster(charge_raster, filename = output_file, format = "GTiff", overwrite = TRUE)
    
    rm(sub_data, charge_raster, pts_coords, pts_field)
    gc() # Nettoyage mémoire
  }
}





get_UP_shp <- function(alpage, alpage_info_file, UP_file) {
  UP_name1 <- get_alpage_info(alpage, alpage_info_file, "nom1_UP")
  
  UP <- vect(UP_file) %>%
    filter(nom1 == UP_name1)
  
  # Convertir SpatVector (terra) vers sf (compatible raster/sf)
  UP_sf <- st_as_sf(UP)
  
  return(UP_sf)
}



#Calcul de la distance et du dénivelé

save_distance_denivele <- function(state_rds_file, distance_csv_file , altitude_raster, output_distance_case, YEAR, alpage) {
  # Charger les données
  data <- readRDS(state_rds_file) %>%
    dplyr::select(ID, time, x, y)
  
  # Charger l'altitude en fonction des coordonnées
  altitude <- get_raster_cropped_L93(altitude_raster, get_minmax_L93(data, 20), reproject = FALSE, as = "spatRast")
  
  # Préparer les données pour obtenir les longueurs de pas
  data <- prepData(data, type = "UTM", covNames = NULL)
  
  # Extraire l'altitude
  data$altitude <- terra::extract(altitude, vect(as.matrix(data[c("x", "y")]), type="points", crs=CRS_L93), ID = FALSE)$BDALTI
  
  # Calcul du dénivelé
  data$denivelation <- append(0, pmax(diff(data$altitude), 0))
  data$day <- yday(data$time)
  
  # Fonction pour ignorer le premier élément de chaque groupe
  sum_without_first <- function(v) {
    return (sum(v[2:length(v)], na.rm = TRUE))
  }
  
  # Calcul de la distance et du dénivelé moyen par jour
  data <- data %>% group_by(ID, day) %>%
    summarise(date = first(as.Date(time)), 
              distance = sum_without_first(step), 
              denivelation = sum_without_first(denivelation)) %>%
    group_by(date) %>%
    summarise(distance = mean(distance, na.rm = TRUE), 
              denivelation = mean(denivelation, na.rm = TRUE))
  
  
  # Sauvegarde des données en CSV
  write.csv(data, distance_csv_file, row.names = FALSE)
  
  return(distance_csv_file)
}







generate_trajectory_gpkg_catlog <- function(state_rds_file, output_state_traj_case, YEAR, alpage, sampling_interval = 10) {
  
  # Charger les données depuis le fichier RDS
  data <- readRDS(state_rds_file)
  
  # Conversion de `hour` (décimal) en format HH:MM:SS
  data$hour <- format(as.POSIXct(data$hour * 3600, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
  
  # Liste des ID uniques pour générer un fichier par ID
  unique_ids <- unique(data$ID)
  
  # Boucle sur chaque ID unique
  for (id in unique_ids) {
    
    # Filtrer les données pour cet ID et trier par ordre chronologique
    data_filtered <- data %>%
      filter(ID == id) %>%
      arrange(time)
    
    # Sous-échantillonnage : garder un point tous les "sampling_interval" minutes
    data_filtered <- data_filtered[seq(1, nrow(data_filtered), by = sampling_interval / 10), ]  # Suppression des points selon l'intervalle choisi
    
    # Vérifier s'il y a assez de points après filtrage
    if (nrow(data_filtered) < 2) {
      print(paste("ID", id, "n'a pas assez de points après sous-échantillonnage. Fichier ignoré."))
      next
    }
    
    # Transformer les points en objet spatial `sf`
    sf_points <- st_as_sf(data_filtered, coords = c("x", "y"), crs = 2154)
    
    # Créer les lignes reliant chaque point à son suivant
    line_list <- list()
    
    for (i in 1:(nrow(data_filtered) - 1)) {
      # Création d'une ligne entre deux points successifs
      line_list[[i]] <- st_linestring(as.matrix(data_filtered[i:(i+1), c("x", "y")]))
    }
    
    # Création de l'objet sf contenant les trajectoires
    sf_lines <- st_sfc(line_list, crs = 2154)
    
    # Création d'un data frame pour stocker les informations des lignes
    traj_df <- data_filtered[-nrow(data_filtered), ]  # Supprime le dernier point pour aligner avec les lignes
    sf_traj <- st_sf(traj_df, geometry = sf_lines)
    
    # Définir le chemin de sortie pour le GeoPackage
    output_gpkg <- file.path(output_state_traj_case, paste0("Comportement_traj_", YEAR, "_", alpage, "_", id, "_",sampling_interval, ".gpkg"))
    
    # Sauvegarde des points et des lignes dans le même fichier
    st_write(sf_points, output_gpkg, layer = "points", delete_dsn = TRUE)  # Points
    st_write(sf_traj, output_gpkg, layer = "trajectoires", append = TRUE)  # Lignes
    
    print(paste("Fichier GeoPackage créé pour ID", id, "avec un sous-échantillonnage de", sampling_interval, "minutes :", output_gpkg))
  }
}



generate_trajectory_gpkg_ofb <- function(state_rds_file, output_state_traj_case, YEAR, alpage, sampling_interval = 10, sampling_periods) {
  
  # Charger les données depuis le fichier RDS
  data <- readRDS(state_rds_file)
  
  # Conversion de `hour` (décimal) en format HH:MM:SS
  data$hour <- format(as.POSIXct(data$hour * 3600, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
  
  # Liste des ID uniques pour générer un fichier par ID
  unique_ids <- unique(data$ID)
  
  # Boucle sur chaque ID unique
  for (id in unique_ids) {
    
    # Filtrer les données pour cet ID et trier par ordre chronologique
    data_filtered <- data %>%
      filter(ID == id) %>%
      arrange(time)
    
    # Récupérer la valeur de SAMPLING pour cet ID dans la table (en minutes)
    current_sampling <- sampling_periods$SAMPLING[sampling_periods$ID == id]
    if (length(current_sampling) == 0) {
      warning(paste("Aucun SAMPLING trouvé pour l'ID", id, "-> utilisation de 10 minutes par défaut"))
      current_sampling <- 10
    }
    
    # Calcul du facteur de sous-échantillonnage :
    # Si SAMPLING < 10, on conserve 1 point tous les round(10 / SAMPLING) points,
    # sinon, si SAMPLING >= 10, on ne sous-échantillonne pas (facteur = 1)
    subsample_factor <- if (current_sampling < 10) {
      round(10 / current_sampling)
    } else {
      1
    }
    
    # Application du sous-échantillonnage selon le facteur calculé
    data_filtered <- data_filtered[seq(1, nrow(data_filtered), by = subsample_factor), ]
    
    # Vérifier s'il y a assez de points après filtrage
    if (nrow(data_filtered) < 2) {
      print(paste("ID", id, "n'a pas assez de points après sous-échantillonnage. Fichier ignoré."))
      next
    }
    
    # Transformer les points en objet spatial `sf`
    sf_points <- st_as_sf(data_filtered, coords = c("x", "y"), crs = 2154)
    
    # Créer les lignes reliant chaque point à son suivant
    line_list <- list()
    for (i in 1:(nrow(data_filtered) - 1)) {
      # Création d'une ligne entre deux points successifs
      line_list[[i]] <- st_linestring(as.matrix(data_filtered[i:(i+1), c("x", "y")]))
    }
    
    # Création de l'objet sf contenant les trajectoires
    sf_lines <- st_sfc(line_list, crs = 2154)
    
    # Création d'un data frame pour stocker les informations des lignes (on supprime le dernier point pour aligner avec les lignes)
    traj_df <- data_filtered[-nrow(data_filtered), ]
    sf_traj <- st_sf(traj_df, geometry = sf_lines)
    
    # Définir le chemin de sortie pour le GeoPackage
    output_gpkg <- file.path(output_state_traj_case, paste0("Comportement_traj_", YEAR, "_", alpage, "_", id, "_", sampling_interval, ".gpkg"))
    
    # Sauvegarde des points et des lignes dans le même fichier
    st_write(sf_points, output_gpkg, layer = "points", delete_dsn = TRUE)  # Points
    st_write(sf_traj, output_gpkg, layer = "trajectoires", append = TRUE)  # Lignes
    
    print(paste("Fichier GeoPackage créé pour ID", id, "avec un sous-échantillonnage (facteur =", subsample_factor, ") :", output_gpkg))
  }
}



generate_presence_polygons <- function(state_rds_file, output_polygon_use_shp, YEAR, alpage,
                                       density_threshold,
                                       n_grid = 200,
                                       small_poly_threshold_percent = 0.05) {
  # Chargement des librairies nécessaires
  library(sf)
  library(dplyr)
  library(lubridate)
  library(MASS)
  
  # Lecture et filtrage des données
  data <- readRDS(state_rds_file)
  data <- data[data$alpage == alpage, ]
  
  # Définition des périodes (quinzaines) à partir des dates clés
  date_breaks <- yday(as.POSIXct(
    c(paste0("16/06/", YEAR),
      paste0("01/07/", YEAR),
      paste0("16/07/", YEAR),
      paste0("01/08/", YEAR),
      paste0("16/08/", YEAR),
      paste0("01/09/", YEAR),
      paste0("16/09/", YEAR)),
    format = "%d/%m/%Y")
  )
  date_labs <- c("avant 15 jun", "16 - 30 jun", "1 - 15 jul", "16 - 30 jul",
                 "1 - 15 aou", "16 - 30 aou", "1 - 15 sep", "après 16 sep")
  
  data <- data %>%
    mutate(month_period = factor(date_labs[findInterval(yday(time), date_breaks) + 1],
                                 levels = date_labs))
  
  # Calcul des polygones de densité pour chaque période
  liste_sf <- list()
  for (p in unique(data$month_period)) {
    sub_data <- data %>% filter(month_period == p)
    if (nrow(sub_data) == 0) next
    
    # Calcul de la densité 2D
    kd <- kde2d(x = sub_data$x, y = sub_data$y, n = n_grid)
    cl <- contourLines(kd$x, kd$y, kd$z, levels = density_threshold)
    if (length(cl) == 0) next
    
    # Conversion des contours en polygones (en fermant la boucle)
    polygons <- lapply(cl, function(cobj) {
      coords <- cbind(cobj$x, cobj$y)
      if (nrow(coords) < 3) return(NULL)
      if (!all(coords[1, ] == coords[nrow(coords), ])) {
        coords <- rbind(coords, coords[1, ])
      }
      poly <- st_polygon(list(coords))
      if (st_area(poly) == 0) return(NULL)
      poly
    })
    polygons <- Filter(Negate(is.null), polygons)
    if (length(polygons) == 0) next
    
    # Fusionner tous les polygones en un seul, puis les séparer pour filtrer les petits
    sfc_polygons <- st_sfc(polygons, crs = 2154)
    sfc_union <- st_union(sfc_polygons)
    sfc_union_poly <- st_cast(sfc_union, "POLYGON")
    areas <- st_area(sfc_union_poly)
    threshold_area <- small_poly_threshold_percent * max(areas)
    sfc_union_filtered <- sfc_union_poly[areas >= threshold_area]
    if (length(sfc_union_filtered) == 0) next
    
    # Fusion finale pour obtenir un seul MultiPolygon par période
    sfc_final <- st_union(sfc_union_filtered)
    liste_sf[[as.character(p)]] <- st_sf(month_period = p, geometry = sfc_final)
  }
  
  if (length(liste_sf) == 0) {
    message("Aucun polygone généré.")
    return(NULL)
  }
  
  all_polygons_sf <- do.call(rbind, liste_sf)
  st_write(all_polygons_sf, output_polygon_use_shp, delete_layer = TRUE)
  message(paste("Polygones exportés vers", output_polygon_use_shp))
  
  return(all_polygons_sf)
}








generate_presence_polygons_by_percentage <- function(state_rds_file, output_polygon_use_shp, YEAR, alpage,
                                                     percentage ,
                                                     n_grid ,
                                                     small_poly_threshold_percent ,
                                                     crs ) {
  # Chargement des librairies nécessaires
  library(sf)
  library(dplyr)
  library(lubridate)
  library(MASS)
  
  # Lecture des données et filtrage sur l'alpage
  data <- readRDS(state_rds_file)
  data <- data[data$alpage == alpage, ]
  
  # Définition des périodes (quinzaines) à partir de dates clés
  date_breaks <- yday(as.POSIXct(
    c(paste0("16/06/", YEAR),
      paste0("01/07/", YEAR),
      paste0("16/07/", YEAR),
      paste0("01/08/", YEAR),
      paste0("16/08/", YEAR),
      paste0("01/09/", YEAR),
      paste0("16/09/", YEAR)),
    format = "%d/%m/%Y")
  )
  
  date_labs <- c("avant 15 jun", "16 - 30 jun", "1 - 15 jul", "16 - 30 jul",
                 "1 - 15 aou", "16 - 30 aou", "1 - 15 sep", "après 16 sep")
  
  data <- data %>% 
    mutate(month_period = factor(date_labs[findInterval(yday(time), date_breaks) + 1],
                                 levels = date_labs))
  
  # Initialisation de la liste pour stocker les polygones par période
  liste_sf <- list()
  
  # Pour chaque période (quinzaine)
  for (p in unique(data$month_period)) {
    sub_data <- data %>% filter(month_period == p)
    if (nrow(sub_data) == 0) next
    
    # Calcul du KDE sur la grille
    kd <- kde2d(x = sub_data$x, y = sub_data$y, n = n_grid)
    
    # Calcul du seuil pour couvrir "percentage" de la masse totale
    zvals <- as.vector(kd$z)
    zsum <- sum(zvals)
    sorted_z <- sort(zvals, decreasing = TRUE)
    cumsum_z <- cumsum(sorted_z)
    idx <- which(cumsum_z >= percentage * zsum)[1]
    density_threshold <- sorted_z[idx]
    
    # Extraction des contours correspondant à ce seuil
    cl <- contourLines(kd$x, kd$y, kd$z, levels = density_threshold)
    if (length(cl) == 0) next
    
    # Conversion des contours en polygones en fermant la boucle
    polygons <- lapply(cl, function(cobj) {
      coords <- cbind(cobj$x, cobj$y)
      if (nrow(coords) < 3) return(NULL)
      if (!all(coords[1, ] == coords[nrow(coords), ])) {
        coords <- rbind(coords, coords[1, ])
      }
      poly <- st_polygon(list(coords))
      if (st_area(poly) == 0) return(NULL)
      poly
    })
    polygons <- Filter(Negate(is.null), polygons)
    if (length(polygons) == 0) next
    
    # Création d'un objet sfc puis fusion des polygones
    sfc_polygons <- st_sfc(polygons, crs = crs)
    sfc_union <- st_union(sfc_polygons)
    sfc_union_poly <- st_cast(sfc_union, "POLYGON")
    
    # Filtrage des petits polygones (exclure ceux dont l'aire est inférieure à un pourcentage du plus grand)
    areas <- st_area(sfc_union_poly)
    threshold_area <- small_poly_threshold_percent * max(areas)
    sfc_union_filtered <- sfc_union_poly[areas >= threshold_area]
    if (length(sfc_union_filtered) == 0) next
    
    # Fusion finale pour obtenir un seul objet (MultiPolygon) par période
    sfc_final <- st_union(sfc_union_filtered)
    sfc_final <- st_make_valid(sfc_final)  # Assurer la validité de la géométrie
    
    liste_sf[[as.character(p)]] <- st_sf(month_period = p, geometry = sfc_final)
  }
  
  if (length(liste_sf) == 0) {
    message("Aucun polygone généré.")
    return(NULL)
  }
  
  all_polygons_sf <- do.call(rbind, liste_sf)
  
  # Export vers le shapefile de sortie
  st_write(all_polygons_sf, output_polygon_use_shp, delete_layer = TRUE)
  message(paste("Polygones exportés vers", output_polygon_use_shp))
  
  return(all_polygons_sf)
}



generate_presence_polygons_by_percentage_per_month <- function(state_rds_file, output_polygon_use_shp, YEAR, alpage,
                                                     percentage,
                                                     n_grid,
                                                     small_poly_threshold_percent,
                                                     crs) {
  # Chargement des librairies nécessaires
  library(sf)
  library(dplyr)
  library(lubridate)
  library(MASS)
  
  # Lecture des données et filtrage sur l'alpage et l'année
  data <- readRDS(state_rds_file)
  data <- data[data$alpage == alpage, ]
  data <- data %>% filter(year(time) == YEAR)
  
  # Regroupement par mois : Juin, Juillet, Août, Septembre
  # Si le mois est octobre (10), il sera regroupé avec septembre (9)
  data <- data %>% 
    mutate(month = month(time)) %>%
    mutate(month = ifelse(month == 10, 9, month)) %>%
    mutate(month_period = factor(case_when(
      month == 6 ~ "Juin",
      month == 7 ~ "Juillet",
      month == 8 ~ "Aout",
      month == 9 ~ "Septembre"
    ), levels = c("Juin", "Juillet", "Aout", "Septembre")))
  
  # Initialisation de la liste pour stocker les polygones par période (mois)
  liste_sf <- list()
  
  # Pour chaque période (mois)
  for (p in unique(data$month_period)) {
    sub_data <- data %>% filter(month_period == p)
    if (nrow(sub_data) == 0) next
    
    # Calcul du KDE sur la grille
    kd <- kde2d(x = sub_data$x, y = sub_data$y, n = n_grid)
    
    # Calcul du seuil pour couvrir "percentage" de la masse totale
    zvals <- as.vector(kd$z)
    zsum <- sum(zvals)
    sorted_z <- sort(zvals, decreasing = TRUE)
    cumsum_z <- cumsum(sorted_z)
    idx <- which(cumsum_z >= percentage * zsum)[1]
    density_threshold <- sorted_z[idx]
    
    # Extraction des contours correspondant à ce seuil
    cl <- contourLines(kd$x, kd$y, kd$z, levels = density_threshold)
    if (length(cl) == 0) next
    
    # Conversion des contours en polygones en fermant la boucle
    polygons <- lapply(cl, function(cobj) {
      coords <- cbind(cobj$x, cobj$y)
      if (nrow(coords) < 3) return(NULL)
      if (!all(coords[1, ] == coords[nrow(coords), ])) {
        coords <- rbind(coords, coords[1, ])
      }
      poly <- st_polygon(list(coords))
      if (st_area(poly) == 0) return(NULL)
      poly
    })
    polygons <- Filter(Negate(is.null), polygons)
    if (length(polygons) == 0) next
    
    # Création d'un objet sfc puis fusion des polygones
    sfc_polygons <- st_sfc(polygons, crs = crs)
    sfc_union <- st_union(sfc_polygons)
    sfc_union_poly <- st_cast(sfc_union, "POLYGON")
    
    # Filtrage des petits polygones (exclure ceux dont l'aire est inférieure à un certain pourcentage du plus grand)
    areas <- st_area(sfc_union_poly)
    threshold_area <- small_poly_threshold_percent * max(areas)
    sfc_union_filtered <- sfc_union_poly[areas >= threshold_area]
    if (length(sfc_union_filtered) == 0) next
    
    # Fusion finale pour obtenir un seul objet (MultiPolygon) par période
    sfc_final <- st_union(sfc_union_filtered)
    sfc_final <- st_make_valid(sfc_final)  # Assurer la validité de la géométrie
    
    liste_sf[[as.character(p)]] <- st_sf(month_period = p, geometry = sfc_final)
  }
  
  if (length(liste_sf) == 0) {
    message("Aucun polygone généré.")
    return(NULL)
  }
  
  all_polygons_sf <- do.call(rbind, liste_sf)
  
  # Export vers le shapefile de sortie
  st_write(all_polygons_sf, output_polygon_use_shp, delete_layer = TRUE)
  message(paste("Polygones exportés vers", output_polygon_use_shp))
  
  return(all_polygons_sf)
}




generate_new_grazed_area <- function(daily_rds_prefix, output_new_grazed_area_rds, output_new_grazed_area_tif, threshold) {
  # Chargement des librairies nécessaires
  library(dplyr)
  library(raster)
  
  
  # Lecture des données journalières
  daily_data <- readRDS(daily_rds_prefix)
  
  
  
  # Vérification de la présence des colonnes nécessaires
  required_cols <- c("x", "y", "day", "Charge")
  if (!all(required_cols %in% names(daily_data))) {
    stop("Les colonnes nécessaires (x, y, day, chargement) sont manquantes dans les données.")
  }
  
  # Pour chaque pixel (x,y), trier par jour et calculer la somme cumulée du chargement.
  # Dès que la somme cumulée dépasse le seuil (50), on enregistre le jour julien correspondant.
  grazing_data <- daily_data %>%
    filter(state == "Paturage") %>%
    group_by(x, y) %>%
    arrange(day) %>%
    mutate(cum_load = cumsum(Charge)) %>%
    filter(cum_load >= threshold) %>%
    slice(1) %>%     # première fois que le seuil est dépassé
    ungroup() %>%
    dplyr::select(x, y, day) %>%
    dplyr::rename(Day_of_grazing = day)
  
  
  
  # Sauvegarde du résultat sous format RDS
  saveRDS(grazing_data, file = output_new_grazed_area_rds)
  cat("Nouveau champ 'Day_of_grazing' sauvegardé en RDS :", output_new_grazed_area_rds, "\n")
  
  # Conversion en data.frame si nécessaire
  grazing_data_df <- as.data.frame(grazing_data)
  
  # rasterFromXYZ attend : 1ère col = x, 2ème = y, 3ème = valeur
  rast <- rasterFromXYZ(grazing_data_df, crs = CRS("+init=epsg:2154"))
  
  writeRaster(rast, filename = output_new_grazed_area_tif, format = "GTiff", overwrite = TRUE)
  cat("Raster sauvegardé avec succès :", output_new_grazed_area_tif, "\n")
  gc()
  return(grazing_data)
}





generate_new_grazed_area_by_day <- function(daily_rds_prefix, 
                                            output_case_alpage, 
                                            YEAR, 
                                            alpage, 
                                            threshold = 1,
                                            crs_string = "+init=epsg:2154") {
  # Chargement des librairies nécessaires
  library(dplyr)
  library(raster)
  
  # 1. Lecture des données journalières
  daily_data <- readRDS(daily_rds_prefix)
  
  # Vérification des colonnes nécessaires
  required_cols <- c("x", "y", "day", "Charge", "state")
  if (!all(required_cols %in% names(daily_data))) {
    stop("Les colonnes nécessaires (x, y, day, Charge, state) sont manquantes dans les données.")
  }
  
  # 2. Calcul du jour de mise en pâture pour chaque pixel
  grazing_data <- daily_data %>%
    filter(state == "Paturage") %>%
    group_by(x, y) %>%
    arrange(day) %>%
    mutate(cum_load = cumsum(Charge)) %>%
    filter(cum_load >= threshold) %>%
    slice(1) %>%  # première occurrence où le seuil est dépassé
    ungroup() %>%
    dplyr::select(x, y, day) %>%
    dplyr::rename(Day_of_grazing = day)
  
  # Sauvegarde du résultat en RDS
  output_rds <- file.path(output_case_alpage, paste0("new_grazed_area_", YEAR, "_", alpage, ".rds"))
  saveRDS(grazing_data, file = output_rds)
  cat("Nouveau champ 'Day_of_grazing' sauvegardé en RDS :", output_rds, "\n")
  
  # Conversion en data.frame
  grazing_data_df <- as.data.frame(grazing_data)
  
  # 3. Création d'un template raster pour définir l'étendue et la résolution
  global_raster <- rasterFromXYZ(grazing_data_df, crs = CRS(crs_string))
  template_raster <- raster(extent(global_raster), resolution = res(global_raster), crs = CRS(crs_string))
  
  # 4. Pour chaque jour unique, générer un raster
  unique_days <- sort(unique(grazing_data$Day_of_grazing))
  
  for (d in unique_days) {
    # Sélection des pixels pour lesquels Day_of_grazing == d
    subset_day <- grazing_data %>% filter(Day_of_grazing == d)
    subset_day_df <- as.data.frame(subset_day)
    
    # Rasterisation sur le template : les cellules contenant ces pixels recevront la valeur du jour d (sinon NA)
    rast_day <- rasterize(subset_day_df[, c("x", "y")], template_raster, 
                          field = subset_day_df$Day_of_grazing, fun = mean)
    
    # Construction du nom du fichier de sortie
    output_tif <- file.path(output_case_alpage, 
                            paste0("new_grazed_area_", YEAR, "_", alpage, "_", d, ".tif"))
    
    # Export du raster au format GeoTIFF
    writeRaster(rast_day, filename = output_tif, format = "GTiff", overwrite = TRUE)
    cat("Raster pour le jour", d, "sauvegardé avec succès :", output_tif, "\n")
  }
  
  gc()
  return(grazing_data)
}






crop_hillshape<- function(UP_file_mask, hillShade_file, output_hill_shade_tif){
  #Library
  library(raster)
  library(sf)
  
  
  #Import data : Hillshade rast & Mask UP shp
  hill_raster <- raster(hillShade_file)
  Up_mask_shp <- st_read(UP_file_mask)
  
  
  charge_raster_crop <- mask(crop(hill_raster, Up_mask_shp), Up_mask_shp)
  
  # Suppression préalable si nécessaire
  if (file.exists(output_hill_shade_tif)) file.remove(output_hill_shade_tif)
  
  # Export du raster crop en GeoTIFF
  writeRaster(charge_raster_crop, filename = output_hill_shade_tif, format = "GTiff", overwrite = TRUE)
  cat("Raster découpé sauvegardé avec succès :", output_hill_shade_tif, "\n")
  
  
  
}


create_gif_from_images <- function(
    case_GIF_file,          # Dossier où se trouvent les images
    output_gif,         # Chemin complet du GIF de sortie
    file_pattern = "Carte_cayolle_.*\\.(png|jpg)$", # Motif pour repérer les fichiers
    delay_seconds = 0.5   # Durée d'affichage de chaque image (en secondes)
) {
  # Charge la librairie magick
  library(magick)
  
  # 1) Lister les fichiers correspondant au pattern (png/jpg)
  files <- list.files(case_GIF_file, pattern = file_pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("Aucun fichier image trouvé dans ", case_GIF_file, " avec le motif '", file_pattern, "'.")
  }
  
  # 2) Extraire la partie numérique pour trier dans l'ordre croissant
  #    Exemple de nom : "Carte_cayolle_185.jpg"
  #    On récupère "185" et on convertit en entier pour trier
  numeric_suffix <- function(fname) {
    as.integer(sub(".*_([0-9]+)\\..*", "\\1", fname))
  }
  files_sorted <- files[order(sapply(files, function(x) numeric_suffix(basename(x))))]
  
  # 3) Lire les images dans une liste
  img_list <- lapply(files_sorted, image_read)
  
  # 4) Les assembler
  img_joined <- image_join(img_list)
  
  # 5) Créer l'animation
  #    Le paramètre 'delay' dans image_animate() est exprimé en 1/100e de seconde.
  #    Si on veut 'delay_seconds' s, on fait delay = delay_seconds * 100
  magick_delay <- delay_seconds * 100
  img_animated <- image_animate(img_joined, delay = magick_delay)
  
  # 6) Écrire le GIF sur disque
  image_write(img_animated, path = output_gif)
  
  message("GIF créé avec succès : ", output_gif)
}


create_gif_from_images_day_bis <- function(
    case_GIF_file,          # Dossier où se trouvent les images
    output_gif,             # Chemin complet du GIF de sortie
    file_pattern = "Carte_cayolle_.*\\.(png|jpg)$",
    delay_seconds       
) {
  # Library
  library(magick)
  library(stringr)
  
  # Définir la locale 
  Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
  
  # Liste des fichiers 
  files <- list.files(case_GIF_file, pattern = file_pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("Aucun fichier image trouvé dans ", case_GIF_file, " avec le motif '", file_pattern, "'.")
  }
  
  # Extraire JJ
  numeric_suffix <- function(fname) {
    as.integer(sub(".*_([0-9]+)\\..*", "\\1", fname))
  }
  files_sorted <- files[order(sapply(files, function(x) numeric_suffix(basename(x))))]
  
  # Annoter et stocker les images dans une liste
  img_list <- lapply(files_sorted, function(file) {
    # Extraction du jour julien à partir du nom de fichier
    julian_day <- numeric_suffix(basename(file))
    
    # Conversion du jour julien en date (en considérant l'année 2022)
    date_converted <- as.Date(julian_day - 1, origin = "2022-01-01")
    
    # Formatage de la date en français, par exemple "3 septembre 2022"
    date_str <- format(date_converted, "%d %B %Y")
    
    # Mettre la première lettre du mois en majuscule (ex: "3 Septembre 2022")
    spl_date <- unlist(strsplit(date_str, " "))
    if (length(spl_date) >= 2) {
      spl_date[2] <- paste0(toupper(substring(spl_date[2], 1, 1)), substring(spl_date[2], 2))
    }
    date_str <- paste(spl_date, collapse = " ")
    
    # Lecture de l'image
    img <- image_read(file)
    
    # Annotation de l'image 
    img <- image_annotate(
      img,
      text = date_str,
      gravity = "northwest",
      location = "+100+100",
      size = 80, 
      color = "black",
      boxcolor = "none",
      weight = 700
    )
    return(img)
  })
  
  # Images en séquence
  img_joined <- image_join(img_list)
  
  # Animation
  magick_delay <- delay_seconds * 100
  img_animated <- image_animate(img_joined, delay = magick_delay)
  
  # Save
  image_write(img_animated, path = output_gif)
  
  message("GIF créé avec succès : ", output_gif)
}




load_by_veget <- function(alpage, alpage_info_file, UP_file, daily_rds_prefix, load_veget_rds)  {
  
  #Package
  library(dplyr)
  library(terra)
  library(sf)      
  library(magrittr)
  
  # Filtrage des données journalière
  # Chemin du fichier RDS avec les charges journalières
  df_all <- readRDS(daily_rds_prefix)
  
  # Filtrer pour conserver seulement les états "Deplacement" et "Paturage"
  df_filtered <- df_all %>%
    filter(state %in% c("Deplacement", "Paturage"))
  
  # Import de la carte de veget
  # Création du polygone de découpe à partir de UP
  # Récupérer l'UP 
  UP <- get_UP_shp(alpage, alpage_info_file, UP_file)
  # Conversion de UP (sf) en SpatVector (terra) et buffer de 300 m
  UP_vect <- terra::vect(UP)
  cropping_polygon <- terra::buffer(UP_vect, 300)
  # Typo de veget
  vegetation_typology_name <- get_alpage_info(alpage, alpage_info_file, "typologie_vegetation")
  
  # On aligne la rasterisation sur la grille contenue dans df_filtered (colonnes x et y)
  vegetation_df <- get_vegetation_rasterized(
    vegetation_file = carto_file,
    vegetation_typology_name = vegetation_typology_name,
    grid = df_filtered[c("x", "y")],
    cropping_polygon = cropping_polygon
  )
  
  #Jointure des données journalières avec la typologie de végétation
  # Arrondir les coordonnées dans les deux jeux de données
  df_filtered <- df_filtered %>%
    mutate(x = round(x, 1),
           y = round(y, 1))
  vegetation_df <- vegetation_df %>%
    mutate(x = round(x, 1),
           y = round(y, 1))
  
  # On joint par "x" et "y". Selon la précision des coordonnées, il peut être utile d'arrondir.
  df_joined <- df_filtered %>%
    left_join(vegetation_df, by = c("x", "y"))
  
  # Agrégation de la charge par jour et par habitat
  # Pour chaque jour et pour chaque type de végétation, sommer la charge
  df_habitat_day <- df_joined %>%
    group_by(day, vegetation_type) %>%
    summarise(charge_sum = sum(Charge, na.rm = TRUE),
              .groups = "drop")
  
 # Sauvegarde du résultat agrégé en RDS
  saveRDS(df_habitat_day, file = load_veget_rds)
  
  gc()
}





gif_plot_load_by_veget_day <- function(load_veget_rds, load_veget_day_gif, delay_miliseconds){
  # Library 
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(magick)
  
  # Load data
  df_habitat_day = readRDS(file = load_veget_rds)
  
  # Ajout du jour 0
  unique_habitats <- unique(df_habitat_day$vegetation_type)
  jour0 <- data.frame(
    day = 0,
    vegetation_type = unique_habitats,
    charge_sum = 0
  )
  df_with_day0 <- bind_rows(df_habitat_day, jour0)
  
  # Calcul de la charge cumulative
  all_days <- sort(unique(df_with_day0$day))
  # Cumul by habitat
  df_cum <- df_with_day0 %>%
    group_by(vegetation_type) %>%
    arrange(day) %>%
    mutate(cumulative = cumsum(charge_sum)) %>%
    ungroup()
  
  df_cum_complete <- df_cum %>%
    complete(vegetation_type, day = all_days) %>%
    group_by(vegetation_type) %>%
    fill(cumulative, .direction = "down") %>%
    mutate(cumulative = if_else(is.na(cumulative), 0, cumulative)) %>%
    ungroup()
  
  # Exclure les habitats qui n'apparaissent jamais (cumul=0 tout du long)
  df_cum_nonzero <- df_cum_complete %>%
    group_by(vegetation_type) %>%
    filter(any(cumulative > 0)) %>%
    ungroup()
  
  # Valeur max pour fixer l'échelle Y (sur toute la saison)
  max_cum <- max(df_cum_nonzero$cumulative, na.rm = TRUE)
  
  # Palette de couleurs (exemple)
  habitat_scale <- scale_fill_manual(
    values = c(
      "Formations minérales"             = "grey40",
      "Pelouses productives"             = "gold",
      "Pelouses nivales"                 = "dodgerblue",
      "Pelouses humides"                 = "darkturquoise",
      "Landes"                           = "purple",
      "Forêts non pastorales"            = "darkgreen",
      "Nardaies denses du subalpin"      = "limegreen",
      "Megaphorbiaies et Aulnaies"       = "chartreuse4",
      "P. thermiques écorchées"          = "coral",
      "P. intermédiaires de l’alpin"     = "red",
      "P. th. méditerranéo-montagnardes" = "tomato",
      "Autres"                           = "pink"
      
    )
  )
  
  # Dossier temporaire pour stocker les images
  plot_dir <- file.path(tempdir(), "plots_gif_temp")
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Boucle sur les jours 
  liste_jours <- all_days 
  
  for (d in liste_jours) {
    
    # Sélection des données pour le jour d
    df_jour <- df_cum_nonzero %>%
      filter(day == d)
    
    p <- ggplot(df_jour, aes(
      x = cumulative,
      y = reorder(vegetation_type, cumulative),
      fill = vegetation_type
    )) +
      geom_col() +
      scale_x_continuous(limits = c(0, max_cum)) +
      # Pas de titre pour le jour
      labs(
        x = "Présence du troupeau (brebis.jours)",
        y = NULL
      ) +
      habitat_scale +
      guides(fill = FALSE) +
      # Fond blanc
      theme_classic(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
      )
    
    # Sauvegarde du plot
    png_path <- file.path(plot_dir, sprintf("jour_%03d.png", d))
    ggsave(filename = png_path, plot = p, width = 7, height = 5)
  }
  
  # Assembler les PNG en un GIF
  png_files <- list.files(plot_dir, pattern = "^jour_.*\\.png$", full.names = TRUE)
  png_files <- sort(png_files)
  img_list <- lapply(png_files, image_read)
  img_joined <- image_join(img_list)
  # Delay = 100 => 1 seconde par image
  img_animated <- image_animate(img_joined, delay = delay_miliseconds)
  
  # Sauvegarder le GIF
  image_write(img_animated, path = load_veget_day_gif)
  
  message("GIF créé avec succès : ", load_veget_day_gif)
  gc()
  
  
  
}



