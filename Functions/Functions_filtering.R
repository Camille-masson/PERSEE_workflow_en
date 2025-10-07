#New


source("Functions/Functions_clean_Bjorneraas.R")


### FUNCTIONS ###
#---------------#

load_catlog_data  <- function(input_file) {
    # INPUT :
    #     input_file : path to a csv file containing a Catlog trajectory
    # OUTPUT :
    #     a data.frame containing trajectory, colnames being c("lat", "lon", "Altitude", "actX", "actY", "date")

    traject <- read.csv(input_file, skip=6, header=TRUE)
    colnames(traject)[c(3, 4)] = c("lat", "lon")
    traject$date = as.POSIXct(paste(traject$Date, traject$Time), tz="GMT", format="%m/%d/%Y %H:%M:%S")
    traject <- traject %>% dplyr::select(-c(Date, Time, Satellites, HDOP, PDOP, Temperature..C., Speed..km.h., TTFF, SNR.AVG, SNR.MAX))
    return(traject)
}





load_other_data  <- function(input_file) {
  # INPUT :
  #     input_file : path to a csv file containing a Other trajectory
  # OUTPUT :
  #     a data.frame containing trajectory, colnames being c("lat", "lon", "Altitude", "actX", "actY", "date")
  
  #Charger les data csv des logers
  traject <- read.csv(input_file, header=TRUE)
  
  # Renommer les colonnes pour correspondre au format Catlog (adapter si nécessaire)
  colnames(traject)[c(7, 8, 11)] <- c("lat", "lon", "Altitude")
  
  traject$date = as.POSIXct(paste(traject$Date, traject$Time), tz="GMT", format="%m/%d/%Y %H:%M:%S")
  
  # Sélection des colonnes pertinentes (adapter si besoin)
  traject <- traject %>% dplyr::select(lat, lon, Altitude, x, y, date, infoloc)
  return(traject)
}







load_other_data_rdata <- function(input_file) {
  # INPUT :
  #     input_file : path to an .Rdata file containing an OFB trajectory
  # OUTPUT :
  #     a data.frame containing trajectory, colnames being c("lat", "lon", "Altitude", "x", "y", "date")
  
  # Charger le fichier Rdata dans un environnement temporaire
  env <- new.env()
  load(input_file, envir = env)
  
  # Trouver tous les objets chargés
  loaded_objects <- ls(env)
  
  # Vérifier qu'il y a au moins un objet
  if (length(loaded_objects) == 0) {
    stop(paste("Aucun objet trouvé dans le fichier :", input_file))
  }
  
  # Vérifier si plusieurs objets existent
  if (length(loaded_objects) > 1) {
    message(paste("Attention : Plusieurs objets trouvés dans", input_file, 
                  ". Utilisation du premier :", loaded_objects[1]))
  }
  
  # Charger le premier objet trouvé
  traject <- env[[loaded_objects[1]]]
  
  # Vérifier que c'est bien un data.frame
  if (!is.data.frame(traject)) {
    stop("Le fichier Rdata ne contient pas un data.frame.")
  }
  
  # Renommer les colonnes pour correspondre au format Catlog (adapter si nécessaire)
  colnames(traject)[c(7, 8, 11)] <- c("lat", "lon", "Altitude")
  
  # Conversion de la colonne date en format POSIXct
  traject$date <- as.POSIXct(traject$date, tz="GMT", format="%Y-%m-%d %H:%M:%S")
  
  # Conversion des colonnes lat, lon et Altitude en numérique si nécessaire
  cols_to_convert <- c("lat", "lon", "Altitude")
  
  for (col in cols_to_convert) {
    if (!is.numeric(traject[[col]])) {
      warning(paste("Conversion de", col, "en numérique pour", input_file))
      traject[[col]] <- as.numeric(as.character(traject[[col]]))
    }
  }
  
  # Sélection des colonnes pertinentes (adapter si besoin)
  traject <- traject %>% dplyr::select(lat, lon, Altitude, x, y, date, infoloc)
  
  return(traject)
}






identify_sampling_period <- function(data_dir, YEAR, TYPE, alpages, output_dir) {
  # Définition du dossier contenant les trajectoires brutes
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  
  # Création du dossier de sortie s'il n'existe pas
  filter_output_dir <- file.path(output_dir, "0. Sampling_Periods")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  # Appliquer le traitement sur chaque alpage
  sampling_results <- lapply(alpages, function(alpage) {
    collar_dir <- file.path(raw_data_dir, alpage)  # Dossier GPS du pâturage
    
    # Sélection des fichiers selon le type
    file_pattern <- if (TYPE == "catlog") "\\.csv$" else "\\.Rdata$"
    collar_files <- list.files(collar_dir, pattern = file_pattern, full.names = TRUE)
    
    if (length(collar_files) == 0) {
      warning(paste("No files found in", collar_dir, "for TYPE =", TYPE))
      return(NULL)  # Skip processing if no files are found
    }
    
    #  Ouvrir un fichier PDF pour stocker les histogrammes
    pdf(file.path(filter_output_dir, paste0("Sampling_Periods_", YEAR, "_", alpage, ".pdf")), width = 9, height = 9)
    
    # Appliquer la détection du sampling period sur chaque fichier
    results <- lapply(collar_files, function(collar_f) {
      collar_ID <- if (TYPE == "catlog") {
        strsplit(basename(collar_f), split = "_")[[1]][1]
      } else {
        strsplit(basename(collar_f), split = "_")[[1]][1]
      }
      
      print(paste("Processing file:", collar_f, "Collar ID:", collar_ID))
      
      # Charger les données
      traject <- switch(
        TYPE,
        "catlog" = load_catlog_data(collar_f),
        "other" = load_other_data_rdata(collar_f),
        stop("Unrecognized TYPE: please choose 'catlog' or 'other'")
      )
      
      # Vérifier que la colonne 'date' existe
      if (!"date" %in% names(traject)) {
        stop(paste("ERREUR: les données du collier", collar_ID, "ne contiennent pas de colonne 'date'"))
      }
      
      # Convertir 'date' en format datetime
      traject$date <- as.POSIXct(traject$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      traject <- traject[order(traject$date), ]
      
      # Calculer les différences de temps en secondes
      time_diffs <- diff(as.numeric(traject$date))
      print(paste("Time diffs for", collar_ID, ":", paste(head(time_diffs, 20), collapse = ", ")))  # Debug
      
      if (length(time_diffs) == 0) {
        stop(paste("ERREUR: Aucune différence de temps calculée pour le collier", collar_ID, "- Vérifiez les données !"))
      }
      
      # Filtrer les valeurs aberrantes (> 95e percentile)
      threshold <- quantile(time_diffs, 0.95, na.rm = TRUE)
      time_diffs <- time_diffs[time_diffs <= threshold]
      
      if (length(time_diffs) == 0) {
        stop(paste("ERREUR: Aucune donnée valide après filtrage pour le collier", collar_ID, "- Vérifiez les données !"))
      }
      
      # Définition des bornes pour l'histogramme
      min_time <- min(time_diffs, na.rm = TRUE)
      max_time <- max(time_diffs, na.rm = TRUE)
      
      # Correction de l'histogramme
      if (min_time == max_time) {
        warning(paste("Tous les points sont espacés de", min_time, "secondes pour le collier", collar_ID, "- Histogramme inutile."))
        mode_interval <- min_time
      } else {
        breaks_seq <- seq(min_time, max_time, length.out = 30)
        hist_vals <- hist(time_diffs, breaks = breaks_seq, plot = FALSE)
        mode_interval <- hist_vals$breaks[which.max(hist_vals$counts)]  # Intervalle dominant
        
        #  Génération du graphique dans le PDF
        hist(time_diffs, breaks = breaks_seq, col = "lightblue", main = paste("Collar ID:", collar_ID),
             xlab = "Intervalle de temps (secondes)", ylab = "Fréquence")
        abline(v = mode_interval, col = "red", lwd = 2, lty = 2)
      }
      
      # Convertir en minutes et arrondir à la minute la plus proche
      sampling_period_min <- round(mode_interval / 60)
      sampling_period_min <- max(sampling_period_min, 1)
      
      # Retourner les résultats
      list(
        ID = collar_ID,
        SAMPLING= sampling_period_min,
        sampling_period = sampling_period_min * 60
      )
    })
    
    #  Fermer le fichier PDF après avoir ajouté tous les histogrammes
    dev.off()
    
    return(results)
  })
  
  # Convertir les résultats en data.frame
  sampling_periods <- do.call(rbind, lapply(sampling_results, function(x) if (!is.null(x)) do.call(rbind, lapply(x, data.frame))))
  
  #  Enregistrer en .RDS
  output_rds_file <- file.path(filter_output_dir, paste0("Sampling_periods_", YEAR, "_",alpage,".rds"))
  saveRDS(sampling_periods, output_rds_file)
  
  return(sampling_periods)
}



































load_followit_data  <- function(input_file) {
    # INPUT :
    #     input_file : path to a csv file containing a Folowit trajectory
    # OUTPUT :
    #     a data.frame containing trajectory, colnames being c("lat", "lon", "Altitude", "actX", "actY", "date")

    traject <- read.csv(input_file, header=TRUE, sep="\t")
    traject <- traject[-c(1),]
    traject$time = as.POSIXct(paste(traject$Date, traject$Time), format = "%Y %m %d %H:%M:%S", tz = "UTC")
    traject <- subset(traject, select = -c(X.1, X.2, X2D3D, TTF, DOP, SVs, FOM, Date, Time))
    colnames(traject) <- c("lat", "lon", "Altitude", "actX", "actY", "date")
    traject$lat = as.numeric(traject$lat)
    traject$lon = as.numeric(traject$lon)
    traject <- traject[!is.na(traject$lat) & !is.na(traject$lon),]
    return(traject)
}

followit_correct_date  <- function(traject, correct_beg_date) {
    date_diff <- date(correct_beg_date) - date(traject$date[1])
    date(traject$date) <- date(traject$date) + date_diff
    return(traject)
}

date_filter <- function(traject, beg_date, end_date) {
    previous_length = length(traject$date)
    traject <- traject %>% filter(between(date, beg_date, end_date))
    new_length = length(traject$date)
    print(paste(previous_length-new_length, "points were removed by date filter.", new_length,"points remain."))
    return(traject)
}

WSG84_speed <- function(i,x) {
    if (i==1) {
        return(NA)
    }
    else {
        distance = 6371008 * acos( sin(pi/180*x$lat[i])*sin(pi/180*x$lat[i-1]) + cos(pi/180*x$lat[i])*cos(pi/180*x$lat[i-1])*cos(pi/180*(x$lon[i-1]-x$lon[i])) )
    
        time = as.numeric(difftime(time1 = x$date[i], time2 = x$date[i-1], units = "secs"))
        return(distance/time)
    }
}

position_filter <- function(traject, medcrit=750, meancrit=500, spikesp=1500, spikecos=(-0.97)) {
    # Using Bjoneraas2010’s script
    
    WSG84_CRS = CRS("+proj=longlat +datum=WGS84")
    L93_CRS = CRS("+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
    traject <- SpatialPointsDataFrame(traject[,c("lon","lat")],
                                      traject,    #the R object to convert
                                      proj4string = WSG84_CRS)
    
    traject <- spTransform(traject, L93_CRS)
    traject@data$x = traject@coords[,1]
    traject@data$y = traject@coords[,2]
    
    # medcrit et meancrit sont en m
    # spikesp est en m/h
    scr = GPS.screening.wrp(id=traject$ID, x=traject@data$x, y=traject@data$y, 
                            da=traject$date, medcrit, meancrit, spikesp, spikecos)
    
    # GPS.screening.wrp mixes up the order of the individual trajectories (but not the points within each trajectory)
    # therefore the need for the loop
    traject$R1error <- TRUE
    traject$R2error <- TRUE
    for (collar in unique(traject$ID)) {
        print(paste("Nombre de R1errors pour", collar, ":", sum(scr$R1error[scr$id == collar], na.rm=TRUE)))
        print(paste("Nombre de R2errors pour", collar, ":", sum(scr$R2error[scr$id == collar], na.rm=TRUE)))
        traject$R1error[traject$ID==collar] <- scr$R1error[scr$id==collar]
        traject$R2error[traject$ID==collar] <- scr$R2error[scr$id==collar]
    }
    
    
    return(traject@data)
}








filter_one_collar <- function(traject, collar_file, output_rds_file, alpage_name, beg_date, end_date, individual_info_file,
                              bjoneraas.medcrit, bjoneraas.meancrit, bjoneraas.spikesp, bjoneraas.spikecos, sampling_period = 120) {
    # Filters the relocation from one collar based on date and speed/position (Bjoneraas2010).
    # The filtered trajectory is appended to output_rds_file
    # INPUTS
    #    traject : a data.frame containing the raw relocations of the collar, with columns date, lat and lon
    #    collar_ID : the ID of the collar
    #    output_rds_file : the rds file the filtered trajectory should be appended to
    #    alpage_name : name of the alpage
    #    beg_date : the date from which relocations are to be kept
    #    end_date : the date until which relocations are to be kept
    #    bjoneraas.medcrit, bjoneraas.meancrit, bjoneraas.spikesp, bjoneraas.spikecos : parameter of the Bjorneraas relocation errors filter
    #    sampling_period : theoretical time between two consecutive relocations, in seconds
    # OUPUTS
    #    a one-row data.frame of performance indicators of the collar: name (collar ID), worked_until_end (1 if the collar didn’t stop working until 24 hours before beg_date), nloc (number of relocations) and error_perc (percentage of relocations removed by Bjoneraas2010 filter)

    collar_ID = strsplit(collar_file, split = "_")[[1]][1]

    beg_date = as.POSIXct(get_individual_info(collar_ID, individual_info_file, "date_pose"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
    end_date = as.POSIXct(get_individual_info(collar_ID, individual_info_file, "date_retrait"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
    day_prop = as.numeric(gsub(",", ".", get_individual_info(collar_ID, individual_info_file, "proportion_jour_allume"))) # proportion of day with collar switched on

    n_loc_theory = as.numeric(difftime(end_date, beg_date, units = "secs")) * day_prop / sampling_period
    print(paste("Working on", collar_ID, "from", beg_date, "to", end_date))

    indicators = data.frame(name = collar_ID)
    traject$ID <- collar_ID

    # Filter on dates and compute corresponding indicators
    traject <- date_filter(traject, beg_date, end_date)
        # If there is less than one day missing at the end of the time-series, we consider that the collar worked until the end (1)
    indicators$worked_until_end = ifelse(as.numeric(difftime(end_date, traject$date[nrow(traject)], units = "secs")) <= 24*3600, 1, 0)
    indicators$nloc = nrow(traject)

    # Filter on position and speed (Bjoneraas2010) and compute corresponding indicators
    traject <- position_filter(traject, medcrit=bjoneraas.medcrit, meancrit=bjoneraas.meancrit, spikesp=bjoneraas.spikesp, spikecos=bjoneraas.spikecos)
    indicators$R1error = sum(traject$R1error, na.rm=TRUE)
    indicators$R2error = sum(traject$R2error, na.rm=TRUE)
    indicators$localisation_rate = indicators$nloc/n_loc_theory
    indicators$error_perc = (indicators$R1error + indicators$R2error)/nrow(traject)

    traject <- traject[!traject$R1error & !traject$R2error,]
    traject <- traject[!is.na(traject$x), ]
    traject$alpage <- alpage_name
    traject$R1error <- NULL
    traject$R2error <- NULL
    traject$species <- get_individual_info(collar_ID, individual_info_file, "Espece")
    traject$race <- get_individual_info(collar_ID, individual_info_file, "Race")

    colnames(traject)[4] <- "time"

    save_append_replace_IDs(traject, file = output_rds_file)
    return(indicators)
}




















filter_one_collar <- function(traject, collar_file, output_rds_file, alpage_name, beg_date, end_date, individual_info_file,
                              bjoneraas.medcrit, bjoneraas.meancrit, bjoneraas.spikesp, bjoneraas.spikecos, sampling_period = 120) {
  # Version d'origine, avec nettoyage minimal AVANT position_filter()
  # pour supporter proprement les trajectoires .Rdata (NAs lat/lon, bornes, ordre, etc.)
  
  collar_ID <- strsplit(collar_file, split = "_")[[1]][1]
  
  # Dates & proportion depuis IIF (inchangé)
  beg_date <- as.POSIXct(get_individual_info(collar_ID, individual_info_file, "date_pose"),    tz="GMT", format="%d/%m/%Y %H:%M:%S")
  end_date <- as.POSIXct(get_individual_info(collar_ID, individual_info_file, "date_retrait"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
  day_prop <- as.numeric(gsub(",", ".", get_individual_info(collar_ID, individual_info_file, "proportion_jour_allume")))
  if (!is.finite(day_prop) || day_prop <= 0 || day_prop > 1) day_prop <- 1  # garde-fou discret
  
  n_loc_theory <- as.numeric(difftime(end_date, beg_date, units = "secs")) * day_prop / sampling_period
  print(paste("Working on", collar_ID, "from", beg_date, "to", end_date))
  
  indicators <- data.frame(name = collar_ID)
  traject$ID <- collar_ID
  
  # --- Nettoyage minimal pour fiabiliser catlog ET Rdata ---
  # date au bon format + tri
  if (!("date" %in% names(traject))) stop("La colonne 'date' est absente du traject.")
  traject$date <- as.POSIXct(traject$date, tz = "GMT")
  traject <- traject[!is.na(traject$date), ]
  traject <- traject[order(traject$date), ]
  
  # lat/lon numériques et valides (évite 'NA values in coordinates')
  traject$lat <- suppressWarnings(as.numeric(traject$lat))
  traject$lon <- suppressWarnings(as.numeric(traject$lon))
  traject <- traject[is.finite(traject$lat) & is.finite(traject$lon) &
                       traject$lat >= -90 & traject$lat <= 90 &
                       traject$lon >= -180 & traject$lon <= 180, ]
  
  # --- Filtre de dates (inchangé) ---
  traject <- date_filter(traject, beg_date, end_date)
  # worked_until_end & nloc (inchangé)
  indicators$worked_until_end <- ifelse(as.numeric(difftime(end_date, traject$date[nrow(traject)], units = "secs")) <= 24*3600, 1, 0)
  indicators$nloc <- nrow(traject)
  
  # --- Filtre position/vitesse (inchangé) ---
  # (comme on a purgé les lat/lon invalides, plus d'erreur 'NA values in coordinates')
  traject <- position_filter(traject,
                             medcrit = bjoneraas.medcrit,
                             meancrit = bjoneraas.meancrit,
                             spikesp = bjoneraas.spikesp,
                             spikecos = bjoneraas.spikecos)
  
  indicators$R1error <- sum(traject$R1error, na.rm=TRUE)
  indicators$R2error <- sum(traject$R2error, na.rm=TRUE)
  indicators$localisation_rate <- indicators$nloc / n_loc_theory
  indicators$error_perc <- (indicators$R1error + indicators$R2error) / nrow(traject)
  
  # Garder les bons points (inchangé, avec un petit garde-fou sur x/y)
  traject <- traject[!traject$R1error & !traject$R2error, ]
  traject <- traject[is.finite(traject$x), ]  # au cas où
  traject$alpage <- alpage_name
  traject$R1error <- NULL
  traject$R2error <- NULL
  traject$species <- get_individual_info(collar_ID, individual_info_file, "Espece")
  traject$race    <- get_individual_info(collar_ID, individual_info_file, "Race")
  
  # Renommage robuste 'date' -> 'time' (au lieu de colnames(traject)[4])
  if ("date" %in% names(traject)) {
    names(traject)[names(traject) == "date"] <- "time"
  } else if (ncol(traject) >= 4) {
    # fallback pour rester 100% compatible avec ton ancien flux
    colnames(traject)[4] <- "time"
  }
  
  save_append_replace_IDs(traject, file = output_rds_file)
  return(indicators)
}



























filter_one_collar <- function(traject, collar_file, output_rds_file, alpage_name, beg_date, end_date, individual_info_file,
                              bjoneraas.medcrit, bjoneraas.meancrit, bjoneraas.spikesp, bjoneraas.spikecos, sampling_period = 120) {
  # Base inchangée + robustesse Rdata (renommages/clean) + schéma standard avant save
  
  collar_ID <- strsplit(collar_file, split = "_")[[1]][1]
  
  # Dates & proportion depuis IIF (inchangé)
  beg_date <- as.POSIXct(get_individual_info(collar_ID, individual_info_file, "date_pose"),    tz="GMT", format="%d/%m/%Y %H:%M:%S")
  end_date <- as.POSIXct(get_individual_info(collar_ID, individual_info_file, "date_retrait"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
  day_prop <- as.numeric(gsub(",", ".", get_individual_info(collar_ID, individual_info_file, "proportion_jour_allume")))
  if (!is.finite(day_prop) || day_prop <= 0 || day_prop > 1) day_prop <- 1
  
  n_loc_theory <- as.numeric(difftime(end_date, beg_date, units = "secs")) * day_prop / sampling_period
  print(paste("Working on", collar_ID, "from", beg_date, "to", end_date))
  
  indicators <- data.frame(name = collar_ID, stringsAsFactors = FALSE)
  traject$ID <- collar_ID
  
  ## ---------- Normalisation minimale pour CSV & Rdata ----------
  # 0) harmoniser noms éventuels venant des .Rdata
  if ("long" %in% names(traject) && !"lon" %in% names(traject)) {
    names(traject)[names(traject) == "long"] <- "lon"
  }
  if ("alti" %in% names(traject) && !"Altitude" %in% names(traject)) {
    traject$Altitude <- traject$alti
  }
  
  # 1) date au bon format + tri
  if (!("date" %in% names(traject))) stop("La colonne 'date' est absente du traject.")
  traject$date <- as.POSIXct(traject$date, tz = "GMT")
  traject <- traject[!is.na(traject$date), ]
  traject <- traject[order(traject$date), ]
  
  # 2) lat/lon numériques valides (évite crash dans position_filter)
  traject$lat <- suppressWarnings(as.numeric(traject$lat))
  traject$lon <- suppressWarnings(as.numeric(traject$lon))
  traject <- traject[is.finite(traject$lat) & is.finite(traject$lon) &
                       traject$lat >= -90 & traject$lat <= 90 &
                       traject$lon >= -180 & traject$lon <= 180, ]
  
  ## ---------- Filtre de dates (identique) ----------
  traject <- date_filter(traject, beg_date, end_date)
  if (!nrow(traject)) {
    indicators$worked_until_end <- 0
    indicators$nloc <- 0
    indicators$R1error <- 0
    indicators$R2error <- 0
    indicators$localisation_rate <- 0
    indicators$error_perc <- NA_real_
    return(indicators)
  }
  
  indicators$worked_until_end <- ifelse(as.numeric(difftime(end_date, traject$date[nrow(traject)], units = "secs")) <= 24*3600, 1, 0)
  indicators$nloc <- nrow(traject)
  
  ## ---------- Filtre position/vitesse (identique) ----------
  traject <- position_filter(
    traject,
    medcrit  = bjoneraas.medcrit,
    meancrit = bjoneraas.meancrit,
    spikesp  = bjoneraas.spikesp,
    spikecos = bjoneraas.spikecos
  )
  
  indicators$R1error <- sum(traject$R1error, na.rm = TRUE)
  indicators$R2error <- sum(traject$R2error, na.rm = TRUE)
  indicators$localisation_rate <- indicators$nloc / n_loc_theory
  indicators$error_perc <- (indicators$R1error + indicators$R2error) / nrow(traject)
  
  ## ---------- Nettoyage final + enrichissement (identique) ----------
  traject <- traject[!traject$R1error & !traject$R2error, ]
  traject <- traject[is.finite(traject$x), ]
  traject$alpage <- alpage_name
  traject$R1error <- NULL
  traject$R2error <- NULL
  traject$species <- get_individual_info(collar_ID, individual_info_file, "Espece")
  traject$race    <- get_individual_info(collar_ID, individual_info_file, "Race")
  
  # 'date' -> 'time' (robuste)
  if ("date" %in% names(traject)) {
    names(traject)[names(traject) == "date"] <- "time"
  } else if (!"time" %in% names(traject) && ncol(traject) >= 4) {
    colnames(traject)[4] <- "time"
  }
  
  ## ---------- Schéma standard avant sauvegarde (clé pour éviter le rbind error) ----------
  keep_cols <- c("ID","time","lat","lon","x","y","Altitude","alpage","species","race","infoloc")
  for (nm in setdiff(keep_cols, names(traject))) traject[[nm]] <- NA
  traject <- traject[, keep_cols]
  
  ## ---------- Append ----------
  save_append_replace_IDs(traject, file = output_rds_file)
  return(indicators)
}






## --- Helper pour fiabiliser l’agrégation des indicateurs ---
ensure_indicator_shape <- function(df) {
  cols <- c("name","worked_until_end","nloc","R1error","R2error","localisation_rate","error_perc")
  for (cl in cols) if (!cl %in% names(df)) df[[cl]] <- NA
  df <- df[, cols]
  for (cl in cols) if (is.factor(df[[cl]])) df[[cl]] <- as.character(df[[cl]])
  df
}






















































































































































