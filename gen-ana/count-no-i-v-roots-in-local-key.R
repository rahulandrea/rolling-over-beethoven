# Zähle Anz. Akkorde mit Grundton "I", "V" in Dur und "i", "V", "v" in Moll

# Arbeitsverzeichnis festsetzen
# setwd("/Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturaarbeit/Analyse/general-ana")

# Teils kopiert aus harmonic-ana-5x-final

# Kombinierter Datenrahmen der Rohdaten mit Filtern nach Dur-Abschnitten
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
# Ggf. Filter (Nach lokaler Tonart: 0 bed. Dur / 1 bed. Moll):
oval_harmonic_tab_maj <- subset(oval_harmonic_tab, localkey_is_minor == 0)
oval_harmonic_tab_min <- subset(oval_harmonic_tab, localkey_is_minor == 1)

# Häufigkeiten zählen
# Allg. Akkord X (gross/klein schreibung wichtih!) in maj
# print(sum(grepl("^X$", oval_harmonic_tab_maj$numeral)))

# (1) Zähle Einträge, die mit "I" beginnen in lokal Dur
no_I_maj <- sum(grepl("^I$", oval_harmonic_tab_maj$numeral))

# (2) Zähle Einträge, die mit "V" beginnen in lokal Dur
no_V_maj <- sum(grepl("^V$", oval_harmonic_tab_maj$numeral))

# (3) Zähle Einträge, die mit "i" beginnen in lokal Moll
no_i_min <- sum(grepl("^i$", oval_harmonic_tab_min$numeral))

# (2) Zähle Einträge, die mit "V" beginnen in lokal Moll
no_V_min <- sum(grepl("^V$", oval_harmonic_tab_min$numeral))

# Ausgabe
print("lokal Dur: Grundakkord I")
print(no_I_maj)
print("lokal Dur: Grundakkord V")
print(no_V_maj)

print("lokal Moll: Grundakkord i")
print(no_i_min)
print("lokal Moll: Grundakkord V")
print(no_V_min)
