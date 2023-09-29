# Zähl Übergänge mit  vers. Akkord

# Übernommen aus oval-harm-ana-5x

# Tongeschlecht d Abschnitte # 0 = Dur, 1 = Moll
g <- 0


# (0.a) Kombinierter Datenrahmen der Rohdaten mit Filtern nach Dur-Abschnitten
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
oval_harmonic_tab <- subset(oval_harmonic_tab, localkey_is_minor == g)

all_chords <- unique(oval_harmonic_tab$chord)
all_chords <- all_chords[!sapply(all_chords, function(x) x == "")]

# (0.b) Absolute Häufigkeit d. Akkorde
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
# Zeilen löschen, in denen die Spalte "chord" leer ist
chord_freq_df <- subset(chord_freq_df, !chord == "")


# (B.1.a) Matrix mit Übergangshäufigkeiten (harmonic-ana-5d (Stichproben))
# Matrix erstellen
B_raw_transition_matrix <- matrix(0, nrow = length(all_chords), ncol = length(all_chords), dimnames = list(all_chords, all_chords))
# for-Schleife zum Übergänge zählen
# Schleife durch die .tsv-Dateien durchlaufen Prog. unten # Oben wurde schon die Liste der .tsv-Dateien erstellt
for (file in tsv_files) {
  # Daten aus spezifischer Tabelle laden
  B_specific_harmonic_tab <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
  # Übergänge zählen und in die Matrix eintragen
  for (i in 1:(length(B_specific_harmonic_tab$chord) - 1)) {
    current_chord <- B_specific_harmonic_tab$chord[i]
    next_chord <- B_specific_harmonic_tab$chord[i + 1]
    # Prüft, dass weder current_chord noch next_chord leer sind
    if (current_chord != "" && next_chord != "") {
      # Stellt Bed. an Dur- bzw. Moll-Abschnitt 
      if (B_specific_harmonic_tab$localkey_is_minor[i] == g && B_specific_harmonic_tab$localkey_is_minor[i + 1] == g) {
        # Fordert, dass current_chord und next_chord in der Matrix sind 
        if (current_chord %in% all_chords && next_chord %in% all_chords) {
          B_raw_transition_matrix[current_chord, next_chord] <- B_raw_transition_matrix[current_chord, next_chord] + 1
        }
      }
    }
  }
}

# (B.1.b) Matrix aufräumen (harmonic-ana-5d (Stichproben))
# Zeilen und Spalten mit 0 löschen # Evtl. Probleme mit Stichproben
B_transition_matrix <- B_raw_transition_matrix
B_transition_matrix <- B_transition_matrix[rowSums(B_transition_matrix != 0) > 0,]
B_transition_matrix <- B_transition_matrix[,colSums(B_transition_matrix != 0) > 0]
# Rel. Häufigkeit des Übergangs in die Felder: Es wird p(A -> B) durch p(A) geteilt
# for (i in 1:nrow(B_transition_matrix)) {
#   for (j in 1:ncol(B_transition_matrix)) {
#     B_transition_matrix[i,j] <- B_transition_matrix[i,j] / chord_freq_df[chord_freq_df$chord == rownames(B_transition_matrix)[i], "absolute_frequency"]
#   }  
# }

transition_df <- as.data.frame(as.table(B_transition_matrix))

print("Anz Zeilen:")
print(nrow(transition_df))

print(sum(transition_df$Freq))

### INFO ### 
# Matrix [Spalte, Zeile] ist wie folgt aufgebaut:
# - In der row (Spalte) steht der aktuelle Akkord (A)
# - In der column (Zeile) steht der kommende Akkord (B)
# - In den Feldern steht die Übergangswahrscheinlichkeit p(A -> B)

# Subset erstellen, das die Bedingung erfüllt (Var1 oder Var2 beginnt mit "I")
# Achtung!: Darunter zählen auch II, III, IV (müssen wieder abgezogen werden)
transitions_with_I_df <- subset(transition_df, grepl("^I", Var1) | grepl("^I", Var2))
freq_tr_w_I <- sum(transitions_with_I_df$Freq)
print("Übergänge mit I:")
print(freq_tr_w_I)

# Subset erstellen, das die Bedingung erfüllt (Var1 beginnt mit "I" und Var2 beginnt mit "V")
# Achtung!: wie oben zählen mehr dazu (und hier sowohl ind var1 als auch on var2)
transitions_with_X_df <- subset(transition_df, grepl("^I", Var1) & grepl("^V", Var2))
freq_tr_w_X <- sum(transitions_with_X_df$Freq)
print("Übergänge I zu V:")
print(freq_tr_w_X)
