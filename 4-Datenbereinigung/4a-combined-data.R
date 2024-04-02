# 4-Datenbereinigung : 4a-combined-data
# last review on 2024-01-26 by RAG

# Dieses Prog. liest annotierte Stücke (im .tsv Format) ein und kombiniert sie zu einer grossen .tsv Tabelle. 
# Folgende Sonderheiten werden beachtet:
# (1) Interpretation der Einträge als Text und nicht Werte 
#         (im Annotationsstandard wird ‘%’ für einen halbverminderten Akkord benutzt, 
#          dasselbe Zeichen wird in R zur Kommentierung benutzt)
# (2) Unterschiedliche Spaltenüberschriften (und dadurch fehlende Spalten)

#setwd("/Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturaarbeit")

# Liste der gewünschten .tsv-Dateien erstellen
# Dateien benamst nach [KOM]-[A][xx]-M[x].tsv 
#   wobei KOM für das Komponistenkürzel steht (LVB für L. v. Beethoven, WAM für W. A. Mozart),
#   A für die Stückart (S für Sonate, Q für Streicherquartett) mit der üblichen Nummerierung (xx aus 00-99),
#   M für den Satz mit Nummer (x aus 0-9)
# Für weiteres siehe README
# Fall nur einzelne gewünscht: c("WAM-01-1.tsv", "LVB-01-1.tsv", "LVB-01-2.tsv")
tsv_files <- list.files(path = "Daten", pattern = "[A-Z]{3}-S[0-9]{2}-M[0-9].tsv")

# Liste von Datenrahmen erstellen, um Daten zu speichern
data_list <- list()

# Schleife durch die ausgewählten .tsv-Dateien und Daten in der Liste 
for (file in tsv_files) {
  # (1) Daten aus der Datei lesen # Dabei Tabelleneinträge STRIKT als Text verstehen
  data <- read.table(file.path("Daten", file), header = TRUE, sep = "\t", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, colClasses = "character")
  data_list[[file]] <- data
}

# Alle Spaltenüberschriften aus allen Dateien sammeln
all_column_names <- unique(unlist(lapply(data_list, names)))


# (2) Funktion zum Kombinieren der Daten
#   (2a) Spalten werden weggelassen (mit drop = FALSE aus dem Datenrahmen entfernt)
#combine_data <- function(data_list, all_column_names) {
#  combined_data <- data.frame()
#  for (data in data_list) {
#    data <- data[, all_column_names, drop = FALSE]
#    combined_data <- rbind(combined_data, data)
#  }
#  return(combined_data)
#}
#   (2b) Fehlende Spalten werden hinzugefügt und mit Leerzeichen gefüllt
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
final_combined_data <- final_combined_data[, c("lfd_nr", all_column_names)]


# Speichere die kombinierte Tabelle in einer Datei
write.table(final_combined_data, "Ergebnisse/4a-combined-data.tsv", sep = "\t", row.names = FALSE, quote = FALSE, na = "")