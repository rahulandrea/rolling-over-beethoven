# ana2-BG-Übergänge: ana2a-BG-transitions-heatmap.R
# last review on 2024-03-29 by RAG

# BESCHREIBUNG UPDATEN!!
# Zuallererst werden die zu untersuchenden Dateien festgelegt.
#     (0a) Es besteht die Möglichkeit, nach Tongeschlecht der Tonartabschnitten zu filtern
# (A-B) Dieses Prog. nutzt "ana1a-UG-rank-freq.R" um für beide Datensätze ein Dataframe zu erstellen, welches jedem
#       Akkord (chord) einen Häufigkeitsrang (rank) und die relative Häufigkeit (relative_frequency) zuordnet
#     (A) "4a-combined-data.R" um eine grosse .tsv Tabelle über alle Stücke und Sätze zu erhalten
#     (B) Erstellt ein Rang-Häufigkeits-Dataframes
# (C) Anschliessend werden die 25 häufigsten Akkorde (top_chords) herausgefiltert und eine 25x25 Matrix erstellt.
# (D) Über eine Schleife werden für jedes Stück einzeln die Akkordübergänge gezählt (würde man die grosse Tab. über
#     alle Stücke nehmen, würde man auch Übergänge zwischen dem Anfangs- und Endakkord zweier Sätze miteinberechnen).
# (E) Ersetzt die abs. Übergangshäufigkeiten in der Matrix durch die relativen 
#     und wandelt die Matrix in ein Dataframe um
# (F) Weiter ist es möglich sich eine Heatmap der Übergänge zwischen dern 25 häufigsten Akkorden ausgeben zu lassen.

#setwd()


# Liste der zu untersuchenden .tsv-Dateien erstellen
# Dateien benamst nach [KOM]-[A][xx]-M[x].tsv 
#   wobei KOM für das Komponistenkürzel steht (LVB für L. v. Beethoven, WAM für W. A. Mozart),
#   A für die Stückart (S für Sonate, Q für Streicherquartett) mit der üblichen Nummerierung (xx aus 00-99),
#   M für den Satz mit Nummer (x aus 0-9)
# Für weiteres siehe README
# Falls nur einzelne gewünscht: c("WAM-S01-M1.tsv", "LVB-S01-M1.tsv", "LVB-S01-M2.tsv")

tsv_files_base <- list.files(path = "Daten", pattern = "LVB-S[0-9]{2}-M[0-9].tsv")
tsv_files_dev1 <- list.files(path = "Daten", pattern = "LVB-Q[0-9]{2}-M[0-9].tsv")


# (0a)
#   Zu untersuchendes Tongeschlecht festlegen (damit beim Auslesen von base und dev1 das selbe Tonggeschlecht 
#   rausgefiltert wird): Siehe dazu (BASE)/(B), (DEV1)/(B) bzw. (DEV2)/(B) 
#   Für Dur: tongeschlecht <- 0, für Moll: tongeschlecht <- 1
tongeschlecht <- 1


# BASE
    # (A)
        
        # Liste von Datenrahmen erstellen, um Daten zu speichern
        data_list <- list()
        
        # Schleife durch die ausgewählten .tsv-Dateien und Daten in der Liste speichern
        for (file in tsv_files_base) {
          # (1) Daten aus der Datei lesen
          data <- read.table(file.path("Daten", file), header = TRUE, sep = "\t", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, colClasses = "character")
          data_list[[file]] <- data
        }
        
        # Alle Spaltenüberschriften aus allen Dateien sammeln
        all_column_names <- unique(unlist(lapply(data_list, names)))
        
        # (2) Funktion zum Kombinieren der Daten
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
    
        # Tongeschlecht-Filter
        oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == tongeschlecht)
    
    # (B) 
        
        # Erstelle eine Tabelle mit den verschiedenen Akkorden und ihren absoluten Häufigkeiten
        chord_freq <- table(oval_harmonic_tab$chord)
        
        # Umwandeln der Tabelle in einen Datenrahmen
        chord_freq_df_base <- data.frame(chord = names(chord_freq), absolute_frequency = as.vector(chord_freq))
        
        # Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
        chord_freq_df_base <- chord_freq_df_base[order(chord_freq_df_base$absolute_frequency, decreasing = TRUE), ]
        
        # Rang erstellen
        chord_freq_df_base$rank <- 1:nrow(chord_freq_df_base)
        
        # Zeilen löscehn, in denen die Spalte "chord" leer ist
        chord_freq_df_base <- subset(chord_freq_df_base, !chord == "")
  
          
# DEV 1
    # (A)
        
        # Liste von Datenrahmen erstellen, um Daten zu speichern
        data_list <- list()
        
        # Schleife durch die ausgewählten .tsv-Dateien und Daten in der Liste speichern
        for (file in tsv_files_dev1) {
          # (1) Daten aus der Datei lesen
          data <- read.table(file.path("Daten", file), header = TRUE, sep = "\t", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, colClasses = "character")
          data_list[[file]] <- data
        }
        
        # Alle Spaltenüberschriften aus allen Dateien sammeln
        all_column_names <- unique(unlist(lapply(data_list, names)))
        
        # (2) Funktion zum Kombinieren der Daten
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
        
        # Tongeschlecht-Filter
        oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == tongeschlecht)
        
    # (B) 
        
        # Erstelle eine Tabelle mit den verschiedenen Akkorden und ihren absoluten Häufigkeiten
        chord_freq <- table(oval_harmonic_tab$chord)
        
        # Umwandeln der Tabelle in einen Datenrahmen
        chord_freq_df_dev1 <- data.frame(chord = names(chord_freq), absolute_frequency = as.vector(chord_freq))
        
        # Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
        chord_freq_df_dev1 <- chord_freq_df_dev1[order(chord_freq_df_dev1$absolute_frequency, decreasing = TRUE), ]
        
        # Rang erstellen
        chord_freq_df_dev1$rank <- 1:nrow(chord_freq_df_dev1)
        
        # Zeilen löscehn, in denen die Spalte "chord" leer ist
        chord_freq_df_dev1 <- subset(chord_freq_df_dev1, !chord == "")
    
    
# (C): Erstellt 25x25 Matrix mit den 25 häufigsten Akkorden
    
    top_chords <- head(chord_freq_df_base$chord, 25)
    transition_matrix_base <- matrix(0, nrow = length(top_chords), ncol = length(top_chords), dimnames = list(top_chords, top_chords))
    transition_matrix_dev1 <- matrix(0, nrow = length(top_chords), ncol = length(top_chords), dimnames = list(top_chords, top_chords))
    transition_matrix_delta <- matrix(0, nrow = length(top_chords), ncol = length(top_chords), dimnames = list(top_chords, top_chords))
    
# BASE    
    # (D)
        
        # Schleife durch die .tsv-Dateien um Prog. unten durchzulaufen
        # Wir haben oben schon die Liste der .tsv-Dateien erstellt
        for (file in tsv_files_base) {
          
          # Daten aus spezifischer Tabelle laden
          specific_harmonic_tab <- read.table(file.path("Daten", file), header = TRUE, sep = "\t", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, colClasses = "character")
          
          # Übergänge zählen und in die Matrix eintragen
          for (i in 1:(length(specific_harmonic_tab$chord) - 1)) {
            current_chord <- specific_harmonic_tab$chord[i]
            next_chord <- specific_harmonic_tab$chord[i + 1]
            
            # Erste Bed. prüft, dass weder current_chord noch next_chord leer sind
            # Error: subscript out of bounds
            if (current_chord != "" && next_chord != "") {
              if (specific_harmonic_tab$localkey_is_minor[i] == tongeschlecht && specific_harmonic_tab$localkey_is_minor[i + 1] == tongeschlecht) { # Filter
                if (current_chord %in% top_chords && next_chord %in% top_chords) {
                  transition_matrix_base[current_chord, next_chord] <- transition_matrix_base[current_chord, next_chord] + 1
                }
              }
            }
          }
        }
        
        
    # (E): Absolute durch relative Übergangshäufigkeit ersetzen in der Matrix und Umwandlung in Dataframe
        
        # In der row steht der vorangehende Akkord (A), in column steht der folgende Akkord (B)
        # Die rel. Häufigkeit berechnen wir durch: p(A -> B)/p(A)
        
        for (i in 1:nrow(transition_matrix_base)) {
          for (j in 1:ncol(transition_matrix_base)) {
            #    print(transition_matrix[i,j])
            #    print(rownames(transition_matrix)[i])
            #    print(chord_freq_df[chord_freq_df$chord == rownames(transition_matrix)[i], "absolute_frequency"])
            transition_matrix_base[i,j] <- transition_matrix_base[i,j] / chord_freq_df_base[chord_freq_df_base$chord == rownames(transition_matrix_base)[i], "absolute_frequency"]
          }  
        }
    
        # Umwandlung in Dataframe
        transition_df_base <- as.data.frame(as.table(transition_matrix_base))
        
        
# DEV1    
    # (D)
        
        # Schleife durch die .tsv-Dateien um Prog. unten durchzulaufen
        # Wir haben oben schon die Liste der .tsv-Dateien erstellt
        for (file in tsv_files_dev1) {
          
          # Daten aus spezifischer Tabelle laden
          specific_harmonic_tab <- read.table(file.path("Daten", file), header = TRUE, sep = "\t", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, colClasses = "character")
          
          # Übergänge zählen und in die Matrix eintragen
          for (i in 1:(length(specific_harmonic_tab$chord) - 1)) {
            current_chord <- specific_harmonic_tab$chord[i]
            next_chord <- specific_harmonic_tab$chord[i + 1]
            
            # Erste Bed. prüft, dass weder current_chord noch next_chord leer sind
            # Error: subscript out of bounds
            if (current_chord != "" && next_chord != "") {
              if (specific_harmonic_tab$localkey_is_minor[i] == tongeschlecht && specific_harmonic_tab$localkey_is_minor[i + 1] == tongeschlecht) { # Filter
                if (current_chord %in% top_chords && next_chord %in% top_chords) {
                  transition_matrix_dev1[current_chord, next_chord] <- transition_matrix_dev1[current_chord, next_chord] + 1
                }
              }
            }
          }
        }
        
        
    # (E): Absolute durch relative Übergangshäufigkeit ersetzen in der Matrix und Umwandlung in Dataframe
        
        # In der row steht der vorangehende Akkord (A), in column steht der folgende Akkord (B)
        # Die rel. Häufigkeit berechnen wir durch: p(A -> B)/p(A)
        
        for (i in 1:nrow(transition_matrix_dev1)) {
          for (j in 1:ncol(transition_matrix_dev1)) {
            #    print(transition_matrix[i,j])
            #    print(rownames(transition_matrix)[i])
            #    print(chord_freq_df[chord_freq_df$chord == rownames(transition_matrix)[i], "absolute_frequency"])
            transition_matrix_dev1[i,j] <- transition_matrix_dev1[i,j] / chord_freq_df_dev1[chord_freq_df_dev1$chord == rownames(transition_matrix_dev1)[i], "absolute_frequency"]
          }  
        }
        
        # Umwandlung in Dataframe
        transition_df_dev1 <- as.data.frame(as.table(transition_matrix_dev1))
        
        
# (F) Kombination
        
for (i in 1:nrow(transition_matrix_delta)) {
  for (j in 1:ncol(transition_matrix_delta)) {
    transition_matrix_delta[i,j] <- transition_matrix_base[i,j] - transition_matrix_dev1[i,j]
  }
}
        
transition_df_delta <- as.data.frame(as.table(transition_matrix_delta))

# (F)
    
    # Farben Dur: #7A9DCF, #215CAF / Moll: #D48681, #B7352D
    # LVB-S: #007894, LVB-Q: #627313, WAM-S: #A7117A
    if (tongeschlecht == 0){
      farbe1 <- "#215CAF"
      farbe1_light <- "#7A9DCF"
      farbe2 <- "#007894"
      farbe2_light <- "#66AFC0"
      farbe3 <- "#627313"
      farbe3_light <- "#A1AB71"
      mark1 <- "#B7352D"
      mark1_light <- "#D48681"
      mark2 <- "#A7117A"
      mark2_light <- "#CA6CAE"
      
    } else if (tongeschlecht == 1){
      farbe1 <- "#B7352D"
      farbe1_light <- "#D48681"
      farbe2 <- "#A7117A"
      farbe2_light <- "#CA6CAE"
      farbe3 <- "#8E6713"
      farbe3_light <- "#BBA471"
      mark1 <- "#215CAF"
      mark1_light <<- "#7A9DCF"
      mark2 <- "#007894"
      mark2_light <- "#66AFC0"
    }
    
    
    # Plot
    library(ggplot2)
    
    ggplot(data = transition_df_delta, aes(x = Var2, y = Var1, fill = Freq)) +
      
      geom_tile() +
      
      scale_fill_gradient2(low = "#B7352D", mid = "white", high = "#627313") +
      
      theme_minimal() +
      
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      
      coord_fixed(ratio=1) +
      
      labs(#title = "probability of transition from chord A to B",
           #subtitle = "in Ludwig van Beethovens Streicherquartetten",
           fill = "Differenz",
           x = "Akkord B",
           y = "Akkord A") +
      
      geom_text(
        data = subset(transition_df_delta, Freq != 0),
        aes(label = sub( "^0\\.", ".", as.character(round(Freq, 3)))), 
        size = 2,
        angle = 45)
    
    
    # Speichere das Diagramm als PNG-Datei # EMPFOHLEN: geom_text(.. , size = 2)
    ggsave("ana2b-BG-heatmap-difference_LVBS-LVBQ-minor-480dpi.png", plot = last_plot(), path = "Ergebnisse", width = 8, height = 8, dpi = 480)
