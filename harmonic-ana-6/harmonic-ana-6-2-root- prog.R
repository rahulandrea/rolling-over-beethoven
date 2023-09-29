# Aus gen-anas/count-transitions.R

setwd("/Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturaarbeit/Analyse/harmonic-ana-6/data")


    # Zähl Übergänge mit  vers. Akkord
    
    # Übernommen aus oval-harm-ana-5x
    
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
    #oval_harmonic_tab <- oval_harmonic_tab <- subset(oval_harmonic_tab, localkey_is_minor == 0 || localkey_is_minor == 1)
    
    all_numerals <- unique(oval_harmonic_tab$numeral)
    all_numerals <- all_numerals[!sapply(all_numerals, function(x) x == "")]
    
    # (0.b) Absolute Häufigkeit d. Akkorde
    # Erstelle eine Tabelle mit den verschiedenen Eintragstexten und ihren absoluten Häufigkeiten
    numeral_freq <- table(oval_harmonic_tab$numeral)
    # Umwandeln der Tabelle in einen Datenrahmen
    numeral_freq_df <- data.frame(numeral = names(numeral_freq), absolute_frequency = as.vector(numeral_freq))
    # Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
    numeral_freq_df <- numeral_freq_df[order(numeral_freq_df$absolute_frequency, decreasing = TRUE), ]
    # Berechne relative Häufigkeiten
    numeral_freq_df$relative_frequency <- numeral_freq_df$absolute_frequency / sum(numeral_freq_df$absolute_frequency)
    # Rang erstellen
    numeral_freq_df$rank <- 1:nrow(numeral_freq_df)
    # Zeilen löschen, in denen die Spalte "numeral" leer ist
    numeral_freq_df <- subset(numeral_freq_df, !numeral == "")
    
    
    # (B.1.a) Matrix mit Übergangshäufigkeiten (harmonic-ana-5d (Stichproben))
    # Matrix erstellen
    raw_transition_matrix <- matrix(0, nrow = length(all_numerals), ncol = length(all_numerals), dimnames = list(all_numerals, all_numerals))
    # for-Schleife zum Übergänge zählen
    # Schleife durch die .tsv-Dateien durchlaufen Prog. unten # Oben wurde schon die Liste der .tsv-Dateien erstellt
    for (file in tsv_files) {
      # Daten aus spezifischer Tabelle laden
      specific_harmonic_tab <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
      # Übergänge zählen und in die Matrix eintragen
      for (i in 1:(length(specific_harmonic_tab$numeral) - 1)) {
        current_numeral <- specific_harmonic_tab$numeral[i]
        next_numeral <- specific_harmonic_tab$numeral[i + 1]
        # Prüft, dass weder current_numeral noch next_numeral leer sind
        if (current_numeral != "" && next_numeral != "") {
          # Fordert, dass current_numeral und next_numeral in der Matrix sind 
          if (current_numeral %in% all_numerals && next_numeral %in% all_numerals) {
            raw_transition_matrix[current_numeral, next_numeral] <- raw_transition_matrix[current_numeral, next_numeral] + 1
          }
        }
      }
    }
    
    # (B.1.b) Matrix aufräumen (harmonic-ana-5d (Stichproben))
    # Zeilen und Spalten mit 0 löschen # Evtl. Probleme mit Stichproben
    transition_matrix <- raw_transition_matrix
    # transition_matrix <- transition_matrix[rowSums(B_transition_matrix != 0) > 0,]
    # transition_matrix <- transition_matrix[,colSums(B_transition_matrix != 0) > 0]
    # # Rel. Häufigkeit des Übergangs in die Felder: Es wird p(A -> B) durch p(A) geteilt
    # for (i in 1:nrow(B_transition_matrix)) {
    #   for (j in 1:ncol(B_transition_matrix)) {
    #     B_transition_matrix[i,j] <- B_transition_matrix[i,j] / numeral_freq_df[numeral_freq_df$numeral == rownames(B_transition_matrix)[i], "absolute_frequency"]
    #   }  
    # }
    
    transition_df <- as.data.frame(as.table(transition_matrix))
    
    print("Anz Zeilen:")
    print(nrow(transition_df))
    
    print(sum(transition_df$Freq))
    
    ### INFO ### 
    # Matrix [Spalte, Zeile] ist wie folgt aufgebaut:
    # - In der row (Spalte) steht der aktuelle Akkord (A)
    # - In der column (Zeile) steht der kommende Akkord (B)
    # - In den Feldern steht die Übergangswahrscheinlichkeit p(A -> B)
    
