

# This function checks and corrects a CSV file to ensure consistency.
# - Detects whether the separator is a comma or semicolon and standardizes it to commas.
# - Ensures date columns ("date_pose", "date_retrait") include seconds if missing.
# - Converts numeric columns with commas as decimal separators ("proportion_jour_allume", "taille_troupeau") into proper numeric format.
# - Rewrites the corrected CSV in place and returns the cleaned dataframe.


check_and_correct_csv <- function(csv_path) {
  # Vérification du séparateur
  first_line <- readLines(csv_path, n = 1)
  nb_comma <- length(strsplit(first_line, ",")[[1]]) - 1
  nb_semicolon <- length(strsplit(first_line, ";")[[1]]) - 1
  
  if (nb_semicolon > nb_comma) {
    df <- read.table(csv_path, sep = ";", header = TRUE, stringsAsFactors = FALSE)
    # On réécrit en virgules
    write.csv(df, file = csv_path, row.names = FALSE)
    # On relit en virgules
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
  } else {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
  }
  
  # Fonction qui ajoute simplement ":00" si le format est "jj/mm/aaaa HH:MM"
  add_seconds_if_missing <- function(date_str) {
    # On ne fait rien si la cellule est vide ou NA
    if (is.na(date_str) || date_str == "") return(date_str)
    
    # Si la date est au format "jj/mm/aaaa HH:MM" (sans secondes), on ajoute ":00"
    # Regex : ^\d{2}/\d{2}/\d{4} \d{1,2}:\d{2}$
    # Exemple : "20/06/2022 10:30" => "20/06/2022 10:30:00"
    if (grepl("^\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}$", date_str)) {
      date_str <- paste0(date_str, ":00")
    }
    # Sinon, on laisse tel quel (même si c'est déjà HH:MM:SS)
    return(date_str)
  }
  
  # Application à date_pose et date_retrait
  if ("date_pose" %in% names(df)) {
    df$date_pose <- sapply(df$date_pose, add_seconds_if_missing, USE.NAMES = FALSE)
  }
  if ("date_retrait" %in% names(df)) {
    df$date_retrait <- sapply(df$date_retrait, add_seconds_if_missing, USE.NAMES = FALSE)
  }
  
  # Correction proportion_jour_allume (selon le nom exact de la colonne)
  if ("proportion_jour_allume" %in% names(df)) {
    df$proportion_jour_allume <- gsub(",", ".", df$proportion_jour_allume)
    df$proportion_jour_allume <- as.numeric(df$proportion_jour_allume)
  }
  
  # Correction taille_troupeau
  if ("taille_troupeau" %in% names(df)) {
    df$taille_troupeau <- gsub(",", ".", df$taille_troupeau)
    df$taille_troupeau <- as.numeric(df$taille_troupeau)
  }
  
  # Réécriture au même endroit
  write.csv(df, file = csv_path, row.names = FALSE)
  
  return(df)
}


