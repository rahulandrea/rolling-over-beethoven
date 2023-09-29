# setwd(Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturarbeit/Analyse/general-anas)

# (1) Kombinierter Datenrahmen für to 25 Akkorde
# kopiert aus oval-harmonic-ana-2g.R

# Liste der gewünschten .tsv-Dateien erstellen
tsv_files <- list.files(pattern = "[0-9]{2}-[0-9].tsv")

# Liste von Datenrahmen erstellen, um Daten zu speichern
data_list <- list()
# Schleife durch die ausgewählten .tsv-Dateien und Daten in der Liste speichern
for (file in tsv_files) {
  # Daten aus der Datei lesen
  data <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
  data_list[[file]] <- data
}
# Alle Spaltenüberschriften aus allen Dateien sammeln
all_column_names <- unique(unlist(lapply(data_list, names)))
# Fehlende Spalten werden hinzugefügt und mit Leerzeichen gefüllt
combine_data <- function(data_list, all_column_names) {
  combined_data <- data.frame()
  for (data in data_list) {
    missing_cols <- setdiff(all_column_names, names(data))
    for (col in missing_cols) {
      data[[col]] <- ""
    }
    combined_data <- rbind(combined_data, data)
  }
  return(combined_data)
}
# Datenrahmen kombinieren
final_combined_data <- combine_data(data_list, all_column_names)
# Neue Spalte "lfd_nr" mit durchlaufender Nummerierung hinzufügen
final_combined_data$lfd_nr <- seq_len(nrow(final_combined_data))
# Nachbearbeitung der Daten, um "FALSE" in "F" umzuwandeln
# (Grundsätzlich wird in R "F" als Abk für "FALSE" verwendet)
final_combined_data[final_combined_data == "FALSE"] <- "F"
# Die Reihenfolge der Spalten in der Tabelle ändern
oval_harmonic_tab <- final_combined_data[, c("lfd_nr", all_column_names)]
# Ggf. Filter: oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == 1)

# (2) for-Schleife zum Übergänge zählen 

# Annahme: Du hast bereits deine große Tabelle als 'df' geladen.

# Initialisiere die Parameter 'minor', 'major' und 'segments'
minor_seg <- 0
major_seg <- 0

# Schleife, um die Tabelle Zeile für Zeile zu durchlaufen
for (i in 1:(nrow(oval_harmonic_tab) - 1)) {
  current_globalkey <- as.character(oval_harmonic_tab$globalkey[i])
  next_globalkey <- as.character(oval_harmonic_tab$globalkey[i + 1])
  
  current_localkey <- as.character(oval_harmonic_tab$localkey[i])
  next_localkey <- as.character(oval_harmonic_tab$localkey[i + 1])
  
  current_localkey_is_minor <- oval_harmonic_tab$localkey_is_minor[i]
  next_localkey_is_minor <- oval_harmonic_tab$localkey_is_minor[i + 1]
  
  # Fall (2a): Alle Werte sind gleich, setze die Schleife fort
  if (current_globalkey == next_globalkey && 
      current_localkey == next_localkey && 
      current_localkey_is_minor == next_localkey_is_minor) {
    next
  }
  
  # Fall (2b): Ein Wert ist unterschiedlich, überprüfe 'localkey_is_minor'
  if (next_localkey_is_minor == 0) {
    major_seg <- major_seg + 1
  } else if (next_localkey_is_minor == 1) {
    minor_seg <- minor_seg + 1
  }
}

# Berechne die Gesamtsegmente
tot_segments <- minor_seg + major_seg

# Ausgabe der Ergebnisse
cat("Minor:", minor_seg, "\n")
cat("Major:", major_seg, "\n")
cat("Segments:", tot_segments, "\n")
