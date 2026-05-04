

# This function checks and corrects a CSV file to ensure consistency.
# - Detects whether the separator is a comma or semicolon and standardizes it to commas.
# - Ensures date columns ("date_pose", "date_retrait") include seconds if missing.
# - Converts numeric columns with commas as decimal separators ("proportion_jour_allume", "taille_troupeau") into proper numeric format.
# - Rewrites the corrected CSV in place and returns the cleaned dataframe.

check_and_correct_csv <- function(csv_path) {
  first_line <- readLines(csv_path, n = 1)
  nb_comma <- length(strsplit(first_line, ",")[[1]]) - 1
  nb_semicolon <- length(strsplit(first_line, ";")[[1]]) - 1
  
  if (nb_semicolon > nb_comma) {
    df <- read.table(csv_path, sep = ";", header = TRUE, stringsAsFactors = FALSE)
    write.csv(df, file = csv_path, row.names = FALSE)
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
  } else {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
  }
  
  add_seconds_if_missing <- function(date_str) {
    if (is.na(date_str) || date_str == "") return(date_str)
    
    if (grepl("^\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}$", date_str)) {
      date_str <- paste0(date_str, ":00")
    }
    
    return(date_str)
  }
  
  if ("date_pose" %in% names(df)) {
    df$date_pose <- sapply(df$date_pose, add_seconds_if_missing, USE.NAMES = FALSE)
  }
  
  if ("date_retrait" %in% names(df)) {
    df$date_retrait <- sapply(df$date_retrait, add_seconds_if_missing, USE.NAMES = FALSE)
  }
  
  if ("proportion_jour_allume" %in% names(df)) {
    df$proportion_jour_allume <- gsub(",", ".", df$proportion_jour_allume)
    df$proportion_jour_allume <- as.numeric(df$proportion_jour_allume)
  }
  
  if ("taille_troupeau" %in% names(df)) {
    df$taille_troupeau <- gsub(",", ".", df$taille_troupeau)
    df$taille_troupeau <- as.numeric(df$taille_troupeau)
  }
  
  # Correction période d'échantillonnage
  if ("Periode_echantillonnage" %in% names(df)) {
    df$Periode_echantillonnage <- gsub(",", ".", df$Periode_echantillonnage)
    df$Periode_echantillonnage <- as.numeric(df$Periode_echantillonnage)
  }
  
  write.csv(df, file = csv_path, row.names = FALSE)
  
  return(df)
}

