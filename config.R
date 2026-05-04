### CONFIGURATION DU PROJET ###

#Installed.Package
if (!require("renv")) install.packages("renv")
library(renv)
deps <- renv::dependencies()
packages <- unique(deps$Package)

installed_pkgs <- installed.packages()[, "Package"]
missing_pkgs <- setdiff(packages, installed_pkgs)

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, dependencies = TRUE, repos = "https://cran.rstudio.com/")
}

#sapply(packages, require, character.only = TRUE)





# Définition des chemins 
root_dir <- getwd()  # Récupère le chemin du projet automatiquement
data_dir <- file.path(root_dir, "data")
output_dir <- file.path(root_dir, "outputs")
raster_dir <- file.path(root_dir, "raster")
functions_dir <- file.path(root_dir, "Functions")

if (! dir.exists(data_dir)) dir.create(data_dir)
if (! dir.exists(output_dir)) dir.create(output_dir)
if (! dir.exists(raster_dir)) dir.create(raster_dir)
if (! dir.exists(functions_dir)) dir.create(functions_dir)




# Chargement des librairies
library(tidyverse) # inclut ggplot2, dplyr, etc.
library(lubridate)
library(sf)
library(sp)
library(terra)
library(viridisLite)

# Chargement automatique des fonctions
# Charger automatiquement les fichiers de fonction
function_files <- list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)

# Vérifier si des fichiers existent avant de les sourcer
if (length(function_files) > 0) {
  for (file in function_files) {
    print(paste("Sourcing:", file))  # Debugging: Affiche le fichier en cours de chargement
    tryCatch(
      source(file),
      error = function(e) {
        message(paste("Erreur lors du chargement de :", file))
        message(e)
      }
    )
  }
} else {
  warning("Aucun fichier de fonction trouvé dans Functions/")
}
sapply(function_files, source)

# Paramètres globaux

ncores <- parallel::detectCores() -4 # Utilisation optimale des cœurs CPU
