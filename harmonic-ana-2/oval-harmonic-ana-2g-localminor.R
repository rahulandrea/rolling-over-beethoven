# Erstellen oval_harmonic_tab (ggf. mit Filter) und Rang vs. Häufigkeit Plot in einem
# in diesem Fall gefiltert nach "localkey_is_minor == 1" dh. lokalen Dur Akkorden

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
final_combined_data <- final_combined_data[, c("lfd_nr", all_column_names)]

# Ggf. Filter: Nur Einträge wo "globalkey_is_minor" == 1
oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == 1)

# Speichere die kombinierte Tabelle in einer Datei # Dateinamen anpassen!!
# write.table(final_combined_data_maj, "combined_data-oval-filter-maj.tsv", sep = "\t", row.names = FALSE, quote = FALSE, na = "")

# Erstelle eine Tabelle mit den verschiedenen Eintragstexten und ihren absoluten Häufigkeiten
chord_freq <- table(oval_harmonic_tab$chord)

# Umwandeln der Tabelle in einen Datenrahmen
chord_freq_df <- data.frame(chord = names(chord_freq), absolute_frequency = as.vector(chord_freq))

# Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
chord_freq_df <- chord_freq_df[order(chord_freq_df$absolute_frequency, decreasing = TRUE), ]

# Berechne relative Häufigkeiten
chord_freq_df$relative_frequency <- chord_freq_df$absolute_frequency / sum(chord_freq_df$absolute_frequency)

# Rang erstellen
chord_freq_df$rank <- 1:nrow(chord_freq_df)

# Erstelle ein gestapeltes Punktdiagramm (Dot Plot)
library(ggplot2)
ggplot(chord_freq_df, aes(x = rank, y = relative_frequency)) +
  geom_point(shape = 1, size = 1, color = "hotpink") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  #  scale_y_continuous(labels = scales::label_number()) +
  labs(x = "chords_rank", y = "rel_freq", title = "rank vs frequency (minor chords)") +
  theme_minimal()

# Anz Datenpunkte
print(chord_freq_df[nrow(chord_freq_df), "rank"])

# Speichere das Diagramm als PNG-Datei
ggsave("rank_vs_freq_minor2-1200dpi.png", plot = last_plot(), width = 8, height = 6, dpi = 1200)