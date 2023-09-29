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
# Ggf. Filter: 
oval_harmonic_tab <- subset(oval_harmonic_tab, localkey_is_minor == 0)

# chord_freq_df zur Ermittlung der Top 25 und rel_freq (Übernommen aus oval-harmonic-ana-4b)
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

# Zeilen löscehn, in denen die Spalte "chord" leer ist
chord_freq_df <- subset(chord_freq_df, !chord == "")

# 25 häufigsten Akkorde ermitteln (aus dem chord_freq_df)
top_chords <- head(chord_freq_df$chord, 25)

# print(top_chords)

# (1.5) Matrix erstellen
transition_matrix <- matrix(0, nrow = length(top_chords), ncol = length(top_chords), dimnames = list(top_chords, top_chords))

# (2) for-Schleife zum Übergänge zählen (Übergangszähler kopiert aus oval-harmonic-ana-4b)

# Schleife durch die .tsv-Dateien um Prog. unten durchzulaufen
# Wir haben oben schon die Liste der .tsv-Dateien erstellt
for (file in tsv_files) {
  
  # Daten aus spezifischer Tabelle laden
  specific_harmonic_tab <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
  
  # Übergänge zählen und in die Matrix eintragen
  for (i in 1:(length(specific_harmonic_tab$chord) - 1)) {
    current_chord <- specific_harmonic_tab$chord[i]
    next_chord <- specific_harmonic_tab$chord[i + 1]
    
    # Erstes if prüft, dass weder current_chord noch next_chord leer sind
    # Error: subscript out of bounds
    if (current_chord != "" && next_chord != "") {
      if (specific_harmonic_tab$localkey_is_minor[i] == 0 && specific_harmonic_tab$localkey_is_minor[i + 1] == 0) { # Filter
        if (current_chord %in% top_chords && next_chord %in% top_chords) {
          transition_matrix[current_chord, next_chord] <- transition_matrix[current_chord, next_chord] + 1
        }
      }
    }
  }
}

# (3) Matrix anpassen (Übernommen aus oval-harmonic-ana-4b)

# Matrix mit transitions/abs_freq
# In der row steht der vorangehende Akkord (A)
# Es wird p(A -> B) durch p(A) geteilt

for (i in 1:nrow(transition_matrix)) {
  for (j in 1:ncol(transition_matrix)) {
#    print(transition_matrix[i,j])
#    print(rownames(transition_matrix)[i])
#    print(chord_freq_df[chord_freq_df$chord == rownames(transition_matrix)[i], "absolute_frequency"])
    transition_matrix[i,j] <- transition_matrix[i,j] / chord_freq_df[chord_freq_df$chord == rownames(transition_matrix)[i], "absolute_frequency"]
  }  
}

# (4) Finalisierung und Heatmap

# Umwandlung in Dataframe
transition_df <- as.data.frame(as.table(transition_matrix))
# Var1 = A # Var2 = B

# # Funktion zur Rundung auf drei Stellen nach dem Komma
# round_to_three <- function(x) {
#   return(round(x, 3))
# }
# 
# # Funktion, um den gewünschten Text für jeden Eintrag zu generieren
# generate_text <- function(entry) {
#   if (entry %in% chord_freq_df$chord) {
#     rounded_frequency <- round_to_three(chord_freq_df[chord_freq_df$chord == entry, "relative_frequency"])
#     return(paste0(entry, " (", as.character(rounded_frequency), ")"))
#   } else {
#     return(entry)
#   }
# }
# 
# # Änderungen in Var1 anwenden
# transition_df$Var1 <- sapply(transition_df$Var1, generate_text)

# Heatmap erstellen
library(ggplot2)
ggplot(data = transition_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#215CAF") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text=element_text(size=11),
    axis.title=element_text(size=13),
    legend.text=element_text(size=11)
  #  axis.line.x=element_line(color ="#4D4D4D"),
  #  axis.line.y=element_line(color ="#4D4D4D"),
  #  plot.margin=margin(0, -50, 0, -50, "pt") #top,right,bottom,left
  ) +
  coord_fixed(ratio=1) +
  labs(
#    title = "transition probab A to B (major chords)",
    fill = "rel. Häufigkeit",
    x = "B",
    y = "A") +
  geom_text(
    data = subset(transition_df, Freq != 0),
    aes(label = sub("^0\\.", ".", as.character(round(Freq, 3)))), 
    size = 2.5,
    angle = 45)

# Speichere das Diagramm als PNG-Datei # Hierfür geom_text size auf 2 ändern
ggsave("transition_probab_major-480dpi-final-2-5.png", plot = last_plot(), width = 8, height = 7, dpi = 480)
