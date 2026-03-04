# Load functions and set paths
source("Functions/Functions_map_plot.R")



### FUNCTIONS ###
#---------------#

### HMM data preparation functions
split_at_gap <- function(data, max_gap = 60, shortest_track = 0) {
    #' Courtesy Théo Michelot
    #' Split track at gaps
    #'
    #' @param data Data frame with (at least) columns for "ID" and "time"
    #' @param max_gap Longest allowed gap, in minutes (track will be split at longer gaps)
    #' @param shortest_track Shortest track to keep after splitting, in minutes. Shorter
    #' tracks will be removed from the output data set.
    #'
    #' @return Data frame with identical structure as input, where ID column
    #' has been replaced by new ID for split tracks. Old ID still accessible as
    #' ID_old column

    # Number of tracks
    n_tracks <- length(unique(data$ID))

    # Save old ID and reinitialise ID column
    data$ID_old <- data$ID
    data$ID <- character(nrow(data))

    # Loop over tracks (i.e., over IDs)
    for(i_track in 1:n_tracks) {
        # Indices for this track
        ind_this_track <- which(data$ID_old == unique(data$ID_old)[i_track])
        track_length <- length(ind_this_track)

        # Time intervals in min
        dtimes <- difftime(data$time[ind_this_track[-1]],
                           data$time[ind_this_track[-track_length]],
                           units = "mins")

        # Indices of gaps longer than max_gap
        ind_gap <- c(0, which(dtimes > max_gap), track_length)

        # Create new ID based on split track
        subtrack_ID <- rep(1:(length(ind_gap) - 1), diff(ind_gap)) #la diff permet d’avoir la longueur de chaque subtrack, puis le rep va permettre de répéter l’indice ordinal de cette subtrack autant de fois que cette longueur
        data$ID[ind_this_track] <- paste0(data$ID_old[ind_this_track], "-", subtrack_ID)
    }

    # Only keep sub-tracks longer than some duration
    track_lengths <- sapply(unique(data$ID), function(id) {
        ind <- which(data$ID == id)
        difftime(data$time[ind[length(ind)]], data$time[ind[1]], units = "min")
    })
    ID_keep <- names(track_lengths)[which(track_lengths >= shortest_track)]
    data <- subset(data, ID %in% ID_keep)

    return(data)
}

regularise_trajectories <- function(data, sampling_period, max_gap = 90) {
  cat(paste0("[INFO] Début de la régularisation des trajectoires avec max_gap = ", max_gap, " minutes.\n"))
  flush.console()
  
  #Définition de max_gap si inférieur ou égale a 10 min vu sous échantillonage autamatique a 10 min max gap = sampling *2, pareil pour le reste
  if (sampling_period <= 600){ max_gap = 20} else {max_gap = sampling_period/60 * 2}
  
  
  # Segmentation en fonction des gaps
  data <- split_at_gap(data = data, max_gap = max_gap, shortest_track = 2 * 60)
  cat("[INFO] Segmentation des trajectoires effectuée.\n")
  flush.console()
  
  #  Si l'échantillonnage est déjà >= 10 min (600 sec), NE PAS RÉGULARISER
  if (sampling_period >= 600) {
    cat(paste0("[INFO] Aucun rééchantillonnage nécessaire (sampling_period = ", sampling_period, " sec).\n"))
    flush.console()
    return(data)
  }
  
  cat("[INFO] Application de la régularisation temporelle...\n")
  flush.console()
  
  if (!("ID" %in% colnames(data))) {
    stop("[ERREUR] La colonne ID est manquante dans les données.")
  }
  
  data <- data %>%
    group_by(ID) %>%
    group_modify(function(df, group_id) {
      if (nrow(df) < 2) return(df)  # Évite les erreurs sur les petits groupes
      
      data_na <- setNA(
        ltraj = as.ltraj(xy = df[, c("x", "y")], date = df$time, id = group_id),
        date.ref = df$time[1],
        dt = sampling_period, tol = 60, units = "sec"
      )
      data_na <- ld(data_na)[, c("x", "y", "date")]
      colnames(data_na) <- c("x", "y", "time")
      return(data_na)
    }) %>%
    ungroup() %>%
    as.data.frame()
  
  cat("[INFO] Régularisation terminée.\n")
  flush.console()
  
  return(data)
}


rolling_averaging_trajectories <- function(data, conv) {
    data <- data %>%
      group_by(ID) %>%
      group_modify( function(df, group_id) { # Rolling average
        conv = conv
        df$x = stats::filter(df$x, conv, sides = 2)
        df$y = stats::filter(df$y, conv, sides = 2)
        return(df[, c("x", "y", "time")])
      }, .keep = FALSE ) %>%
      ungroup() %>%
      as.data.frame()
          
    return(data)
}

prepare_hmm_trajectories <- function(data) {
    # Returns a dataframe
    # Prepare data for HMM (compute step lengths and turning angles)

    data <- prepData(data, type = "UTM", covNames = NULL) #'UTM' because easting/northing is provided, not lat/lon
    data[data$step == 0 & !is.na(data$step), "step"] = 0.01 # to avoid issues with the gamma distribution, 0s are eliminated

    hour <- as.POSIXlt(data$time)
    hour <- hour$h + hour$min/60 + hour$s/3600 + 2 # + 2 to convert from UTC to local time
    data$hour <- hour
    rm(hour)

    return(data)
}

resample_trajectories <- function(data_hmm, resampling_ratio, first_index=0) {
    # INPUTS:
    #   data_hmm : GPS data, with ID field identifying individual trajectories, each having a regular sampling period, padded with NAs if necessary
    #   resampling_ratio the ratio of old to new sampling frequency, integer
    #   first_index the index of the first point of the trajectory to be keeped, integer
    # OUTPUT:
    #   data_hmm, with each trajectory resampled at the new sampling frequency

    data_resamp = data.frame()
    for(id in unique(data_hmm$ID)) {
        indices = which(data_hmm$ID==id)
        data_resamp = rbind(data_resamp, data_hmm[indices[seq(first_index, length(indices), resampling_ratio)] ,])
    }

    return(data_resamp)
}




### HMM results plotting functions
plot_results <- function(hmm, output_pdf_file) {
    # Plot the hmm optimal parameter set and state-sequence resulting from the momentuHMM fit.
    # INPUTS :
    #   hmm : an hmm object, result of the momentuHMM fit.
    #   output_pdf_file : a path to the file in which to save the figures

    pdf(paste0(output_pdf_file,".pdf"), width = 7, height = 7, title=output_pdf_file)
    
    # momentuHMM’s color palette (modified order 3 1 2 4 5...)
    pal <- c("#009E73", "#E69F00", "#56B4E9", "#F0E442",
           "#0072B2", "#D55E00", "#CC79A7")[1:length(hmm$stateNames)]

    ombrage <- get_ombrage(get_minmax_L93(hmm$data, 100))
    hmm_scale = scale_colour_gradientn(colors = pal, guide="legend", breaks = c(1, 2, 3), labels = hmm$stateNames)

    # X vs time
    print(ggplot(hmm$data, aes(time, x, col = state, group=ID)) +
            geom_path(size = 0.3) +
            geom_point(size = 0.3) +
            hmm_scale +
            xlab("") +
            ylab("") +
            theme(legend.position="none") )
    
    # Y vs time
    print(ggplot(hmm$data, aes(time, y, col = state, group=ID)) +
            geom_path(size = 0.3) +
            geom_point(size = 0.3) +
            hmm_scale +
            xlab("") +
            ylab("") +
            theme(legend.position="none") )
               
    # Y vs X
    print(plot_trajectory_over_ombrage (hmm$data, "state", hmm_scale, ombrage, "", "") +
            guides(color = "none"))

    # Convert time to hour-min-sec only for plotting
    hour <- as.POSIXlt(hmm$data$time)
    hour <- hour$h + hour$min/60 + hour$s/3600 + 2 # + 2 to convert from UTC to local time
    hmm$data$hour <- hour
    rm(hour)

    # state vs hour
    hmm$data$state = hmm$stateNames[hmm$data$state]
    print(ggplot(hmm$data, aes(x = round(hour))) +
            geom_bar(position = "fill", aes(fill = state)) +
            scale_y_continuous(labels = scales::percent_format()) +
            theme(axis.text.x = element_text(angle = 90)) +
            scale_fill_manual(values = pal[c(3,2,1)]) +
            xlab("") +
            ylab("") +
            theme(legend.position="none") )

    # Step-length and Angle distributions
    plot(hmm, breaks=25, ask=FALSE, plotTracks=FALSE, plotCI=TRUE, col=pal)
    
    dev.off() # Close the pdf file
}


# Saving functions
next_index_filename = function(prefix, suffix){
    i=1
    repeat {
        f = paste0(prefix,i,suffix)
        if(!file.exists(f)){return(i)}
        i=i+1
    }
}

parameters_to_data.frame <- function(run_parameters) {
    # extensive list of parameters
    par_df = data.frame(model = run_parameters$model)
    par_df$resampling_ratio = run_parameters$resampling_ratio
    par_df$resampling_first_index = run_parameters$resampling_first_index
    par_df$rollavg = run_parameters$rollavg
    par_df$rollavg_convolution = paste(run_parameters$rollavg_convolution, collapse=", ")
    if (!par_df$rollavg) par_df$rollavg_convolution = ""
    par_df$knownRestingStates = run_parameters$knownRestingStates
    par_df$step_distribution = run_parameters$dist$step
    par_df$angle_distribution = run_parameters$dist$angle
    par_df$covariants= deparse1(run_parameters$covariants)
    par_df$step1_mean = run_parameters$Par0$step[1]
    par_df$step1_std = run_parameters$Par0$step[4]
    par_df$step2_mean = run_parameters$Par0$step[2]
    par_df$step2_std = run_parameters$Par0$step[5]
    par_df$step3_mean = run_parameters$Par0$step[3]
    par_df$step3_std = run_parameters$Par0$step[6]
    par_df$angle1_mean = run_parameters$Par0$angle[1]
    par_df$angle1_std = run_parameters$Par0$angle[4]
    par_df$angle2_mean = run_parameters$Par0$angle[2]
    par_df$angle2_std = run_parameters$Par0$angle[5]
    par_df$angle3_mean = run_parameters$Par0$angle[3]
    par_df$angle3_std = run_parameters$Par0$angle[6]
    par_df$angle1_fixed_mean = run_parameters$fixPar$angle[1]
    par_df$angle1_fixed_std = run_parameters$fixPar$angle[4]
    par_df$angle2_fixed_mean = run_parameters$fixPar$angle[2]
    par_df$angle2_fixed_std = run_parameters$fixPar$angle[5]
    par_df$angle3_fixed_mean = run_parameters$fixPar$angle[3]
    par_df$angle3_fixed_std = run_parameters$fixPar$angle[6]

    return(par_df)
}

hmm_result_to_data.frame <- function(run) {
    result_par = data.frame(ID = 0)
    result_par$ID <- strsplit(as.character(run$data$ID[1]), split = "-")[[1]][1]
    result_par$step1_mean = run$mle$step[1,1]
    result_par$step1_std = run$mle$step[2,1]
    result_par$step2_mean = run$mle$step[1,2]
    result_par$step2_std = run$mle$step[2,2]
    result_par$step3_mean = run$mle$step[1,3]
    result_par$step3_std = run$mle$step[2,3]
    result_par$angle1_mean = run$mle$angle[1,1]
    result_par$angle1_std = run$mle$angle[2,1]
    result_par$angle2_mean = run$mle$angle[1,2]
    result_par$angle2_std = run$mle$angle[2,2]
    result_par$angle3_mean = run$mle$angle[1,3]
    result_par$angle3_std = run$mle$angle[2,3]
    result_par$angle3_std = run$mle$angle[2,3]
    result_par$loglikelihood= -run$mod$minimum

    return(result_par)
}

runs_to_csv <- function(run_index, parameters_df, results_df, output_csv_file) {
    table = cbind(run_index, parameters_df, results_df)

    if(!file.exists(output_csv_file)) {
        write.csv(table, output_csv_file, row.names = FALSE)
    }
    else {
        write.table(table, output_csv_file, sep = ",", append = TRUE, 
                    quote = TRUE, col.names = FALSE, row.names = FALSE)
    }
}





runs_to_pdf <- function(run_index, parameters_df, results_df, data_hmm, exec_time, output_dir, output_pdf_file, show_performance_indicators = FALSE) {
  # Génère un PDF résumant l’exécution du modèle HMM via rmarkdown
  
  # Vérification de parameters_df
  if (nrow(parameters_df) != 1) stop("Erreur : parameters_df doit contenir une seule ligne.")
  
  title = parameters_df$model
  if (parameters_df$resampling_ratio > 1) title = paste0(title,", rééchantillonné à ",parameters_df$resampling_ratio,"x")
  if (parameters_df$rollavg_convolution != "") title = paste0(title,", lissé")
  if (parameters_df$covariants != ~1) title = paste0(title,", covariable(s)")
  if (parameters_df$knownRestingStates) title = paste0(title,", états connus")
  
  if(show_performance_indicators) { 
    aligned_data = readRDS(paste0(data_dir,"Cayolle_2022_aligned.rds"))
    
    # Vérification des colonnes communes avant fusion
    common_cols <- intersect(names(data_hmm), names(aligned_data))
    if (!all(c("ID", "time") %in% common_cols)) stop("Erreur : Les colonnes ID et time sont manquantes dans les données.")
    
    # Sélection des colonnes nécessaires
    data_hmm <- data_hmm %>% dplyr::select(-c(step, angle, x, y, hour))
    aligned_data$ODBA = sqrt(aligned_data$actX^2 + aligned_data$actY^2)
    aligned_data <- aligned_data %>% dplyr::select(-c(lat, lon, Altitude, alpage, species, race, x, y, actX, actY))
    colnames(aligned_data)[1] <- "time"
    aligned_data$time <- as.POSIXct(aligned_data$time, origin = '1970-01-01')
    aligned_data$common_date <- as.POSIXct(aligned_data$common_date, origin = '1970-01-01')
    
    data_hmm = merge(data_hmm, aligned_data[, common_cols, drop = FALSE], by = c("ID", "time"))
    aligned_data = aligned_data[!(aligned_data$ID %in% unique(data_hmm$ID)),]
    aligned_data$state = NA
    aligned_data$state_proba = NA
    
    data_hmm = rbind(data_hmm, aligned_data)
    rm(aligned_data)
    data_hmm$time = data_hmm$common_date
    data_hmm$common_date = NULL
  }
  
  # Vérification de la structure de results_df avant d'envoyer à RMarkdown
  if (length(names(results_df)) != ncol(results_df)) {
    stop("Erreur : Le nombre de noms de colonnes dans results_df ne correspond pas au nombre de colonnes réelles.")
  }
  
  # Vérification avant exécution du rendu
  print(dim(results_df))
  print(names(results_df))
  
  # Génération du rapport PDF avec RMarkdown
  rmarkdown::render("Functions/HMM_fit_template.Rmd", output_file = output_pdf_file)
}



parameters_to_rds <- function(run_parameters, output_rds_file) {
    if(!file.exists(output_rds_file)) {
        saveRDS(run_parameters, file = output_rds_file)
    }
    else {
        param_list = readRDS(output_rds_file)
        param_list[[length(param_list)+1]] <- run_parameters
        saveRDS(param_list, file = output_rds_file)
    }
}



viterbi_trajectory_to_rds <- function(data_hmm, output_file, individual_info_file) {
  cat("[INFO] Adaptation et sauvegarde des trajectoires HMM en cours...\n"); flush.console()
  
  if (!file.exists(individual_info_file)) {
    stop("[ERREUR] Fichier `individual_info_file` introuvable : ", individual_info_file)
  }
  individual_info <- read.csv(individual_info_file, header = TRUE, stringsAsFactors = FALSE)
  
  # --- DROP step/angle uniquement s'ils existent ---
  drop_cols <- intersect(c("step", "angle"), names(data_hmm))
  data_save <- as.data.frame(data_hmm[, setdiff(names(data_hmm), drop_cols), drop = FALSE])
  
  # --- Harmonisation de state ---
  stateNames <- c("Repos", "Paturage", "Deplacement")
  if (is.numeric(data_save$state) || is.integer(data_save$state)) {
    data_save$state <- factor(stateNames[data_save$state], levels = stateNames)
  } else {
    # si déjà factor/char, on garde mais on force l’ordre si possible
    data_save$state <- factor(as.character(data_save$state), levels = stateNames)
  }
  
  # Nettoyage minimal
  data_save <- data_save[!is.na(data_save$x), ]
  
  # Merge avec métadonnées (si tu en as besoin)
  merged_data <- merge(data_save, individual_info, by.x = "ID", by.y = "Collier", all.x = TRUE)
  
  # Assure les colonnes finales
  final_cols <- c("ID","time","x","y","hour","state","state_proba","alpage","Espece","Race")
  for (nm in setdiff(final_cols, names(merged_data))) merged_data[[nm]] <- NA
  
  final_data <- merged_data[, final_cols]
  names(final_data)[names(final_data)=="Espece"] <- "species"
  names(final_data)[names(final_data)=="Race"]   <- "race"
  
  saveRDS(final_data, file = output_file)
  cat("[INFO] Sauvegarde réussie dans : ", output_file, "\n")
  flush.console()
}

















# Utility functions

scale_step_parameters_to_resampling_ratio <- function(run_parameters, alpage, sampling_parameters) {
  sampling_period <- sampling_parameters$sampling_period  
  scaling_factor <- sampling_parameters$param_scaling_factor
  
  print(paste0("[INFO] Ajustement des paramètres basé sur un échantillonnage de ", sampling_period, 
               " min avec un facteur de conversion de ", scaling_factor))
  
  if (alpage == "Combe-Madame") {
    print(paste0("[INFO] Réduction des transitions rapides pour minimiser la dominance du pâturage sur l'alpage : ", alpage))
    
    run_parameters$Par0$step[2] <- 0.8 * scaling_factor * run_parameters$Par0$step[2]
    run_parameters$Par0$step[5] <- 0.8 * sqrt(scaling_factor) * run_parameters$Par0$step[5]
    run_parameters$Par0$step[3] <- 0.8 * scaling_factor * run_parameters$Par0$step[3]
    run_parameters$Par0$step[6] <- 0.8 * sqrt(scaling_factor) * run_parameters$Par0$step[6]
  } else {
    print(paste0("[INFO] Application des ajustements standards pour l'alpage : ", alpage))
    
    run_parameters$Par0$step[2] <- scaling_factor * run_parameters$Par0$step[2]
    run_parameters$Par0$step[5] <- sqrt(scaling_factor) * run_parameters$Par0$step[5]
    run_parameters$Par0$step[3] <- scaling_factor * run_parameters$Par0$step[3]
    run_parameters$Par0$step[6] <- sqrt(scaling_factor) * run_parameters$Par0$step[6]
  }
  
  print("[INFO] Ajustement terminé.")
  return(run_parameters)
}

















### Fonction parallèle modifiée

  # Paralelized wrapper for hmm_fit
  # Fit a momentuHMM hmm on one several individuals’ trajectories and save the resulting figures.
  # INPUTS :
  #   data : a data.frame containing a trajectory from a unique individual described as an ID, time, x and y fields.
  #   runPar : a list of parameters to run the model with (see the parameters_to_data_frame function for an extensive list of parameters)
  #   individual_info_file : path to the csv file containing information about every individual ID, must contain "ID", "Alpage" and "Periode d’echantillonnage" columns.
  # OUTPUT : a list of the individual’s momentuHMM objects. Each momentuHMM object’s $data field contains the original trajectory (or sub-trajectories), ordered by time,
  #          along with the viterbi states sequence, the state probabilities and the original ID (only the first ID is retained)
  
par_HMM_fit <- function(data, run_parameters_list, ncores,
                        individual_info_file, sampling_table,
                        output_dir, pdf_dir) {              # <-- pdf_dir ici
  cat("[INFO] Démarrage de l'exécution parallèle de momentuHMM...\n"); flush.console()
  startTime <- Sys.time()
  clus <- makeCluster(ncores, outfile = "")
  
  # objets/export minimal + pdf_dir
  clusterExport(clus, list("data","run_parameters_list","individual_info_file",
                           "sampling_table","pdf_dir"), envir = environment())
  
  # IMPORTANT : sourcer les fonctions HMM côté workers
  clusterEvalQ(clus, {
    options(warn = -1)
    suppressPackageStartupMessages(library(tidyverse))
    library(lubridate); library(momentuHMM); library(adehabitatLT)
    library(sf); library(sp); library(terra)
    source("Functions/Functions_utility.R")
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    source("Functions/Functions_HMM_fitting.R")  
    options(warn = 0)
    NULL
  })
  
  results <- parLapply(clus, unique(data$ID), function(ID) {
    # log temporaire
    log_file <- file.path(tempdir(), paste0("log_", ID, ".txt"))  # <-- tempdir()
    sink(log_file, append = TRUE, split = TRUE)
    
    cat(paste0("[INFO] Traitement de l'individu ID: ", ID, "\n")); flush.console()
    alpage <- get_individual_alpage(ID, individual_info_file)
    cat(paste0("[INFO] Alpage associé: ", alpage, "\n"))
    
    sampling_period <- sampling_table$sampling_period[sampling_table$ID == ID, drop = TRUE]
    run_parameters  <- run_parameters_list[[ID]]
    
    res <- tryCatch(
      hmm_fit(data[data$ID == ID, ],
              runPar = run_parameters,
              pdf_dir = pdf_dir,                # <-- on passe pdf_dir tel quel
              sampling_period = sampling_period),
      error = function(e) {
        cat(paste0("[ERREUR] Problème détecté pour ID: ", ID, " - Message: ", e$message, "\n"))
        flush.console()
        NULL
      }
    )
    
    cat(paste0("[INFO] Fin du traitement pour ID: ", ID, "\n")); flush.console()
    sink()  # stop redirection
    return(list(result = res, log_file = log_file))  # <-- on retourne le chemin du log
  })
  
  stopCluster(clus)
  endTime <- Sys.time()
  cat(paste0("[INFO] Exécution terminée. Durée totale : ",
             round(difftime(endTime, startTime, units='mins'), 2), " minutes.\n"))
  
  # afficher puis supprimer les logs
  for (elt in results) {
    lf <- elt$log_file
    if (!is.null(lf) && file.exists(lf)) {
      cat("\n---- Logs de ", basename(lf), " ----\n", sep = "")
      cat(paste(readLines(lf, warn = FALSE), collapse = "\n"), "\n")
      unlink(lf, force = TRUE)  # <-- suppression
    }
  }
  
  # ne retourner que les objets HMM
  return(lapply(results, `[[`, "result"))
}

  


### Fonction de fitting modifiée
# Fit a momentuHMM hmm on one individual’s sub-trajectories and save the resulting figures.
# INPUTS :
#   data : a data.frame containing a trajectory from a unique individual described as an ID, time, x and y fields.
#   runPar : a list of parameters to run the model with (see the parameters_to_data_frame function for an extensive list of parameters)
#   alpage_directory : a directory to save the resulting figures of this individual’s hmm fit
# OUTPUT : the momemtuHMM object, with a $data field containing the original trajectory (or sub-trajectories), ordered by time,
#          along with the viterbi states sequence, the state probabilities and the original ID (only the first ID is retained)
  
  
hmm_fit <- function(data, runPar, pdf_dir, sampling_period) {  # <-- pdf_dir
  ID <- data$ID[1]
  cat(paste0("[INFO] Démarrage de l'ajustement du modèle HMM pour l'individu ID: ",
             ID, " avec une période d'échantillonnage de ", sampling_period, " secondes.\n"))
  flush.console()
  
  cat("[INFO] Régularisation des trajectoires en cours...\n"); flush.console()
  data_hmm <- regularise_trajectories(data, sampling_period)
  
  if (runPar$rollavg) {
    cat("[INFO] Application de lissage par moyenne mobile...\n"); flush.console()
    data_hmm <- rolling_averaging_trajectories(data_hmm, conv = runPar$rollavg_convolution)
  }
  
  cat("[INFO] Rééchantillonnage des trajectoires en cours...\n"); flush.console()
  data_hmm <- resample_trajectories(data_hmm, runPar$resampling_ratio, runPar$resampling_first_index)
  
  cat("[INFO] Préparation des données pour le modèle HMM...\n"); flush.console()
  data_hmm <- prepare_hmm_trajectories(data_hmm)
  
  knownStates <- rep(NA, nrow(data_hmm))
  if ("hour" %in% colnames(data_hmm) && runPar$knownRestingStates) {
    cat("[INFO] Identification des états de repos connus...\n"); flush.console()
    knownStates[(data_hmm$hour > 3 & data_hmm$hour < 3.5) | (data_hmm$hour > 20.5 & data_hmm$hour < 21)] <- 1
  }
  
  cat("[INFO] Ajustement du modèle HMM en cours...\n"); flush.console()
  stateNames <- c("Repos", "Pâturage", "Déplacement")
  run <- fitHMM(
    data_hmm, nbStates = 3, dist = runPar$dist, DM = runPar$DM, Par0 = runPar$Par0,
    estAngleMean = list(angle = TRUE), fixPar = runPar$fixPar, stateNames = stateNames,
    knownStates = knownStates, formula = runPar$covariants, optMethod = "Nelder-Mead"
  )
  
  cat("[INFO] Modèle HMM ajusté avec succès.\n"); flush.console()
  
  cat("[INFO] Calcul des probabilités d'état...\n"); flush.console()
  run$data$state <- viterbi(run)
  sp <- stateProbs(run)
  run$data$state_proba <- sp[cbind(seq_len(nrow(sp)), run$data$state)]
  
  # --- PDF exactement dans pdf_dir ---
  dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
  cat(paste0("[INFO] Génération des graphiques pour ID: ", ID, "\n")); flush.console()
  plot_results(run, file.path(pdf_dir, ID))     # <-- pas de sous-dossier ajouté
  
  run$data <- run$data[!is.na(run$data$x), ]
  run$data <- run$data[order(run$data$time), ]
  run$data$ID <- ID
  
  cat("[INFO] Ajustement HMM terminé pour ID: ", ID, "\n"); flush.console()
  return(run)
}







# Fonction : Adapte les paramètres aux Temps d'échantillonnage

get_sampling_parameters <- function(sampling_period) {
  print(paste0("[INFO] Période d'échantillonnage définie à ", sampling_period, " minutes."))
  
  if (sampling_period < 10) {
    resampling_ratio <- ceiling(10 / sampling_period)  # Ajustement à 10 min
    param_scaling_factor <- 5  # Facteur basé sur 2 min vers 10 min
    print("[INFO] Un rééchantillonnage est nécessaire : resampling_ratio adaptée pour 10 min")
  } else {
    resampling_ratio <- 1  # Pas de rééchantillonnage
    param_scaling_factor <- sampling_period / 2  # Échelle basée sur 2 min
    print("[INFO] Aucun rééchantillonnage nécessaire : resampling_ratio = 1")
  }
  
  return(list(sampling_period = sampling_period, resampling_ratio = resampling_ratio, param_scaling_factor = param_scaling_factor))
}





