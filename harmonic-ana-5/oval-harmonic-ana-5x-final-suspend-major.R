# harmonic-ana-5x-final-vorhalt: Mittlere Entropie zufälliger Stichproben (Häufigkeitsgewichtet)
# verglichen mit der mittleren Entropie der Akkorde mit Vorhalt

# Arbeitsverzeichnis festsetzen
setwd("/Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturaarbeit/Analyse/harmonic-ana-5/data")

library(dplyr)
library(ggplot2)
library(extrafont)
library(extrafontdb)

# Nimm harmonic-ana-5c-suspend (A) und harmonic-ana-5d-samples (B)
# und plotte in dieselbe Grafik (C)
# (0) wird für (A) und (B) gleich gebraucht

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
oval_harmonic_tab <- subset(oval_harmonic_tab, localkey_is_minor == 0)

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
      if (B_specific_harmonic_tab$localkey_is_minor[i] == 0 && B_specific_harmonic_tab$localkey_is_minor[i + 1] == 0) {
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
for (i in 1:nrow(B_transition_matrix)) {
  for (j in 1:ncol(B_transition_matrix)) {
    B_transition_matrix[i,j] <- B_transition_matrix[i,j] / chord_freq_df[chord_freq_df$chord == rownames(B_transition_matrix)[i], "absolute_frequency"]
  }  
}

### INFO ### 
# Matrix [Spalte, Zeile] ist wie folgt aufgebaut:
# - In der row (Spalte) steht der aktuelle Akkord (A)
# - In der column (Zeile) steht der kommende Akkord (B)
# - In den Feldern steht die Übergangswahrscheinlichkeit p(A -> B)

# (B.2) Entropien berechnen (harmonic-ana-5d (Stichproben))

# Normalisierungsfaktor
B_norm_factor <- ncol(B_transition_matrix) 

B_relevant_chords <- rownames(B_transition_matrix)
# Tab erstellen um Daten einzufügen
B_entropy_df <- data.frame(
  Entropy = numeric(length(B_relevant_chords)),
  NormEntropy = numeric(length(B_relevant_chords))
)
rownames(B_entropy_df) <- B_relevant_chords

for (chrd in B_relevant_chords){
  start_chord <- chrd
  entropy <- 0
  # Schleife um Entropie zu berechnen
  for (j in 1:ncol(B_transition_matrix)) {
    frq <- B_transition_matrix[start_chord, j] # rel. Häufigkeit d. Übergangs
    #     print(frq)
    if (frq == 0) { # Muss Fälle frq = 0 aussortieren da sonst fehler beim teilen durch 0
      summand <- 0
    }
    else {
      summand <- frq*log(1/frq, 2) # Einzelner Summand
    }
    entropy <- entropy + summand # Aufaddieren zu Summe
  }
  B_entropy_df[chrd, "Entropy"] <- entropy
  B_norm_entropy <- entropy / log(B_norm_factor, 2) # Standardisieren
  B_entropy_df[chrd, "NormEntropy"] <- B_norm_entropy
}

#print(entropy_df)
#print(mean(entropy_df$Entropy))
#print(mean(entropy_df$NormEntropy))

# (B.3.a) Proben nehmen (harmonic-ana-5d (Stichproben))

proben_umfang <- 100 # Grösse (Umfang) d. Probe
anz_proben <- 10000 # Anz. Proben

# Tab Mittelwerte der Proben (MWdP) einzufügen
B_proben_entr_df <- data.frame(
  Mittl_NormEntropie = rep(NA, anz_proben)
)
rownames(B_proben_entr_df) <- 1:anz_proben

for (i in 1:anz_proben){
  
  # nonempty_oval_harmonic_tab <- subset(oval_harmonic_tab, !chord == "")
  # probe <- sample(nonempty_harmonic_tab$chord, n)
  probe <- sample( #Erstellt Probe
    all_chords, 
    proben_umfang,
    replace = T, 
    prob = chord_freq_df$relative_frequency)
  
  chrxd_entr_ls <- list() # Leere Liste, in die NormEntr eingetragen werden
  ls_index <- 0  # Index zu Eintragen an richtiger Stelle
  for (chrxd in probe){
    ls_index <- ls_index + 1
    chrxd_normentr <- B_entropy_df[chrxd, "NormEntropy"] # Suche nach Akkord der Stichprobe in der Entropientabelle 
    chrxd_entr_ls[[ls_index]] <- chrxd_normentr # Eintrag in Liste
  }
  
  B_proben_mittel <- mean(unlist(chrxd_entr_ls)) # Mittelwert d. Probe berrechnen
  B_proben_entr_df[i, "Mittl_NormEntropie"] <- B_proben_mittel # in MWdP Tabelle notieren
}

# print(B_proben_entr_df)


# (A.1.a) Matrix mit Übergangshäufigkeiten (harmonic-ana-5c (Vorhalte))

# Matrix erstellen
A_raw_transition_matrix <- matrix(0, nrow = length(all_chords), ncol = length(all_chords), dimnames = list(all_chords, all_chords))
# for-Schleife zum Übergänge zählen
# Schleife durch die .tsv-Dateien durchlaufen Prog. unten # Oben wurde schon die Liste der .tsv-Dateien erstellt
for (file in tsv_files) {
  # Daten aus spezifischer Tabelle laden
  A_specific_harmonic_tab <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
  # Übergänge zählen und in die Matrix eintragen
  for (i in 1:(length(A_specific_harmonic_tab$chord) - 1)) {
    current_chord <- A_specific_harmonic_tab$chord[i]
    next_chord <- A_specific_harmonic_tab$chord[i + 1]
    # Prüft, dass weder current_chord noch next_chord leer sind
    if (current_chord != "" && next_chord != "") {
      # Stellt Bed. an Dur- bzw. Moll-Abschnitt 
      if (A_specific_harmonic_tab$localkey_is_minor[i] == 0 && A_specific_harmonic_tab$localkey_is_minor[i + 1] == 0) {
        # Stellt Bed., dass current_chord einen Vorhalt hat
        if (!is.na(A_specific_harmonic_tab$changes[i]) && A_specific_harmonic_tab$changes[i] != "" && A_specific_harmonic_tab$changes[i] != 0 && !any(grepl("\\+", A_specific_harmonic_tab$changes[i]))) {
          # print(A_specific_harmonic_tab$changes[i])
          # Fordert, dass current_chord und next_chord in der Matrix sind 
          if (current_chord %in% all_chords && next_chord %in% all_chords) {
            A_raw_transition_matrix[current_chord, next_chord] <- A_raw_transition_matrix[current_chord, next_chord] + 1
          }
        }
      }
    }
  }
}

# (A.1.b) Matrix aufräumen (harmonic-ana-5c (Vorhalte))

# Zeilen und Spalten mit 0 löschen
A_transition_matrix <- A_raw_transition_matrix
A_transition_matrix <- A_transition_matrix[rowSums(A_transition_matrix != 0) > 0,]
A_transition_matrix <- A_transition_matrix[,colSums(A_transition_matrix != 0) > 0]
# Rel. Häufigkeit des Übergangs in die Felder: Es wird p(A -> B) durch p(A) geteilt
for (i in 1:nrow(A_transition_matrix)) {
  for (j in 1:ncol(A_transition_matrix)) {
    A_transition_matrix[i,j] <- A_transition_matrix[i,j] / chord_freq_df[chord_freq_df$chord == rownames(A_transition_matrix)[i], "absolute_frequency"]
  }  
}

### INFO ### 
# Matrix [Spalte, Zeile] ist wie folgt aufgebaut:
# - In der row (Spalte) steht der aktuelle Akkord (A)
# - In der column (Zeile) steht der kommende Akkord (B)
# - In den Feldern steht die Übergangswahrscheinlichkeit p(A -> B)

# (A.2) Entropien berrechnen (harmonic-ana-5c (Vorhalte))

A_norm_factor <- ncol(A_transition_matrix) # Normalisierungsfaktor

A_relevant_chords <- rownames(A_transition_matrix)
# Tab erstellen um Daten einzufügen
A_entropy_df <- data.frame(
  Entropy = numeric(length(A_relevant_chords)),
  NormEntropy = numeric(length(A_relevant_chords))
)
rownames(A_entropy_df) <- A_relevant_chords

for (chrd in A_relevant_chords){
  start_chord <- chrd
  entropy <- 0
  # Schleife um Entropie zu berechnen
  for (j in 1:ncol(A_transition_matrix)) {
    frq <- A_transition_matrix[start_chord, j] # rel. Häufigkeit d. Übergangs
    #     print(frq)
    if (frq == 0) { # Muss Fälle frq = 0 aussortieren da sonst fehler beim teilen durch 0
      summand <- 0
    }
    else {
      summand <- frq*log(1/frq, 2) # Einzelner Summand
    }
    entropy <- entropy + summand # Aufaddieren zu Summe
  }
  A_entropy_df[chrd, "Entropy"] <- entropy
  A_norm_entropy <- entropy / log(A_norm_factor, 2) # Standardisieren
  A_entropy_df[chrd, "NormEntropy"] <- A_norm_entropy
}

A_entropy_mean <- mean(A_entropy_df$NormEntropy)
# print(mean(A_entropy_df$Entropy))
# print(mean(A_entropy_df$NormEntropy))

# Die Werte brauch ich:
#write.table(B_proben_entr_df, "B-sus-maj.tsv", sep = "\t", row.names = FALSE)

#print(A_entropy_mean)


# (C) Plotten

# Beispielliste mit Werten zwischen 0 und 1
B_plot_ls <- B_proben_entr_df$Mittl_NormEntropie

# Intervalle festlegen
intervalle <- seq(0, 1, by = 0.005)

# Diagramm erstellen
# loadfonts()
# choose_font(c("XCharter"), quiet = T)

ggplot(data.frame(x = B_plot_ls), aes(x = x)) +
  # Dartsellung von (B)
  geom_histogram(
    aes(y = after_stat(count)),
    breaks = intervalle,
    fill = "#7A9DCF") +
  # Darstellung von (A)
  geom_vline(
    xintercept = A_entropy_mean,
    color = "#215CAF",
    linetype = "solid",
    linewidth = 2) +
  # Allgemeines
  scale_x_continuous(limits = c(0, 0.35)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1400)) +
# xlim(0, 0.4) +
# ylim(0, 1250) +
  labs(
#   title = "Entropie zufälliger Stichproben",
    x = "Norm. bed. Entropie", 
    y = "Stichproben") +
  annotate(
    geom="text",
    x=110,
    y=0.07,
    label=paste("H =",round(A_entropy_mean, 4)),
    size=8) +
  #theme_minimal()
  theme(
  axis.line.x=element_line(color ="#4D4D4D"),
  axis.text=element_text(size=24),
  axis.title=element_text(size=24),
  plot.margin=margin(15, 0, 5, 5, "pt") #t,r,b,l
  #   element_text(family = XCharter)
  )

# Speichere das Diagramm als PNG-Datei
ggsave("entropy_final_suspended-480dpi-67.png", plot = last_plot(), width = 6, height = 8, dpi = 480)
