# Mit "globalkey_is_minor == 0" Filter
# Liste der gewünschten .tsv-Dateien erstellen
tsv_files <- list.files(pattern = "[0-9]{2}-[0-9].tsv")
  
  #c("01-1.tsv", "01-2.tsv", "01-3.tsv")  # Füge hier deine Dateinamen hinzu

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

# Funktion zum Kombinieren der Daten

#(1) Spalten werden weggelassen (mit drop = FALSE aus dem Datenrahmen entfernt)
#combine_data <- function(data_list, all_column_names) {
#  combined_data <- data.frame()
#  for (data in data_list) {
#    data <- data[, all_column_names, drop = FALSE]
#    combined_data <- rbind(combined_data, data)
#  }
#  return(combined_data)
#}

#(2) Fehlende Spalten werden hinzugefügt und mit Leerzeichen gefüllt
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
final_combined_data <- final_combined_data[, c("lfd_nr", all_column_names)]

# Nur Einträge wo "globalkey_is_minor" == 0

final_combined_data_maj <- subset(final_combined_data, globalkey_is_minor == 0)

# Speichere die kombinierte Tabelle in einer Datei
write.table(final_combined_data_maj, "combined_data-oval-filter-maj.tsv", sep = "\t", row.names = FALSE, quote = FALSE, na = "")
