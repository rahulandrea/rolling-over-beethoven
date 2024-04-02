# ana3-BG-Entropie: ana3c-BG-compare-entropy-reduced.R
# last review on 2024-03-28 by RAG

# (0) Zuallererst wird der Untersuchungsgegenstand und Parameter festgelegt:
#     (0a) Die zu untersuchenden Dateien: Es können zwei Gruppen von Daten verglichen werden: 
#          Eine Basisgruppe (base) und eine Vergleichsgruppe (dev1)
#     (0c) Es besteht die Möglichkeit, nach Tongeschlecht der Tonartabschnitten zu filtern
#     (0d) Hier können Umfang und Anzahl der Stichproben für die Hypothesentests festgelegt werden
# (A-B) Dieses Prog. nutzt "ana1c-UG-rank-freq.R" um ein Dataframe zu erstellen, welches jedem
#       Akkord (numeral) einen Häufigkeitsrang (rank) und die relative Häufigkeit (relative_frequency) zuordnet
#       Dies wird je für die Basisgruppe (base) und die Vergleichsgruppe (dev1) durchlaufen.
#     (A) "4a-combined-data.R" um eine grosse .tsv Tabelle über alle Stücke und Sätze zu erhalten
#     (B) Erstellt ein Rang-Häufigkeits-Dataframes
# (C) Es werden die beiden Rang-Häufigkeits-Dataframes der Basisgruppe (base) und Vergleichsgruppe (dev1) werden
#     zu einem gesamthaften kombiniert. Jeweilige Erweiterung der Zeilen..

# Weiter besteht dieses Prog. aus zwei Hauptteilen:
# (1) Nimmt Stichproben aus allen Akkorden und berechnet die norm. bed. Entropie für diese
#     (für einen Hinweis zur Laufzeit und Effizienz siehe den Bericht "ana3-Bericht.pdf")
#     (1a) Erstellt eine umfassende Matrix aller Akkordübergänge
#     (1b) Berechnet Entropie für alle Akkorde und Normierung dieser. Erstellt ein Dataframe, welches jedem 
#          Akkord (numeral) seine bed. Entropie (entropy) und seine norm. bed. Entropie zuordnet (norm_entropy)
#     (1c) Wählt Stichproben von Akkorden (!! Umfang und Anzahl können unter (0d) festgelegt werden) und berechnet
#          den Mittelwert der Entropie der Stichprobenakkorde. Eintrag in Tabelle für jede Stichprobe.
# (2) Berechnet die mittlere norm. bed. Entropie aller Akkorde mit Akkordeigenschaft 
#     (!! Akkordeigenschaft kann unter (0b) festgelegt werden)
#     (2a) Erstellt eine Matrix aller Akkordübergänge, die von einem Akkord mit Akkordeigenschaft ausgehen
#     (2b) Berechnet Entropie für die Akkorde mit Akkordeigenschaft und Normierung dieser. Erstellt ein Dataframe,
#          welches jedem Akkord (numeral) seine bed. Entr. (entropy) und norm. bed. Entr. zuordnet (norm_entropy)
#     (2c) Berechnet den Mittelwert

# (D) Berechnet den p-Wert
# (E) Zuletzt ist es möglich ein Diagramm mit den mittl. norm. bed. Entr. der Stichproben und der mittl. 
#     norm. bed. Entr. der Akkorde mit Akkordeigenschaft ausgeben zu lassen. Die Intervalle, in welchen die
#     Stichproben für die Darstellung aufaddiert werden, können hier angepasst werden.

setwd()


# (0)

# (0a)
# Liste der zu untersuchenden .tsv-Dateien erstellen
# Dateien benamst nach [KOM]-[A][xx]-M[x].tsv 
#   wobei KOM für das Komponistenkürzel steht (LVB für L. v. Beethoven, WAM für W. A. Mozart),
#   A für die Stückart (S für Sonate, Q für Streicherquartett) mit der üblichen Nummerierung (xx aus 00-99),
#   M für den Satz mit Nummer (x aus 0-9)
# Für weiteres siehe README
# Falls nur einzelne gewünscht: c("WAM-S01-M1.tsv", "LVB-S01-M1.tsv", "LVB-S01-M2.tsv")

tsv_files_base <- list.files(path = "Daten", pattern = "LVB-S[0-9]{2}-M[0-9].tsv")
tsv_files_dev1 <- list.files(path = "Daten", pattern = "LVB-Q[0-9]{2}-M[0-9].tsv")


# (0c)
# Zu untersuchendes Tongeschlecht festlegen
# Für Dur: tongeschlecht <- 0, für Moll: tongeschlecht <- 1
tongeschlecht <- 0

# (0d)
# Festlegen des Umfangs einer Probe und der Anzahl Proben 
proben_umfang <- 100 # Grösse (Umfang) d. Probe
anz_proben <- 10000 # Anz. Proben


#start_time <- Sys.time()

# (BASE)
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
  
  
  # Filter nach Tongeschlecht der Tonartabschnitte:
  oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == tongeschlecht)
  
  
  # (B) 
  
  # Erstelle eine Tabelle mit den verschiedenen Akkorden und ihren absoluten Häufigkeiten
  numeral_freq_base <- table(oval_harmonic_tab$numeral)
  
  # Umwandeln der Tabelle in einen Datenrahmen
  numeral_freq_df_base <- data.frame(numeral = names(numeral_freq_base), absolute_frequency = as.vector(numeral_freq_base))
  
  # Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
  numeral_freq_df_base <- numeral_freq_df_base[order(numeral_freq_df_base$absolute_frequency, decreasing = TRUE), ]
  
  # Berechne relative Häufigkeiten
  numeral_freq_df_base$relative_frequency <- numeral_freq_df_base$absolute_frequency / sum(numeral_freq_df_base$absolute_frequency)
  
  # Rang erstellen
  numeral_freq_df_base$rank <- 1:nrow(numeral_freq_df_base)
  
  # Zeilen löscehn, in denen die Spalte "numeral" leer ist
  numeral_freq_df_base <- subset(numeral_freq_df_base, !numeral == "")
  
  all_numerals_base <- unique(numeral_freq_df_base$numeral)


# (DEV1)
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
    
    
    # Filter nach Tongeschlecht der Tonartabschnitte: 
    oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == tongeschlecht)
    
  
  # (B) 
  
    # Erstelle eine Tabelle mit den verschiedenen Akkorden und ihren absoluten Häufigkeiten
    numeral_freq_dev1 <- table(oval_harmonic_tab$numeral)
    
    # Umwandeln der Tabelle in einen Datenrahmen
    numeral_freq_df_dev1 <- data.frame(numeral = names(numeral_freq_dev1), absolute_frequency = as.vector(numeral_freq_dev1))
    
    # Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
    numeral_freq_df_dev1 <- numeral_freq_df_dev1[order(numeral_freq_df_dev1$absolute_frequency, decreasing = TRUE), ]
    
    # Berechne relative Häufigkeiten
    numeral_freq_df_dev1$relative_frequency <- numeral_freq_df_dev1$absolute_frequency / sum(numeral_freq_df_dev1$absolute_frequency)
    
    # Rang erstellen
    numeral_freq_df_dev1$rank <- 1:nrow(numeral_freq_df_dev1)
  
    # Zeilen löscehn, in denen die Spalte "numeral" leer ist
    numeral_freq_df_dev1 <- subset(numeral_freq_df_dev1, !numeral == "")
    
    all_numerals_dev1 <- unique(numeral_freq_df_dev1$numeral)


# (C)
  
  # Liste aller Akkorde
  all_numerals <- unique(c(numeral_freq_df_base$numeral, numeral_freq_df_dev1$numeral))
  
  # Erstellen des Gesamtdataframes
  numeral_freq_df <- data.frame(numeral = all_numerals)
  
  # Hinzufügen der Einträge aus den jeweiligen Dataframes
  
  # mit numeral_freq_df_base
  numeral_freq_df$absolute_frequency_base <- ifelse(numeral_freq_df$numeral %in% numeral_freq_df_base$numeral, numeral_freq_df_base$absolute_frequency[match(numeral_freq_df$numeral, numeral_freq_df_base$numeral)], 0)
  numeral_freq_df$relative_frequency_base <- numeral_freq_df$absolute_frequency_base / sum(numeral_freq_df$absolute_frequency_base)
  #ifelse(numeral_freq_df$numeral %in% numeral_freq_df_base$numeral, numeral_freq_df_base$relative_frequency[match(numeral_freq_df$numeral, numeral_freq_df_base$numeral)], 0)
  
  numeral_freq_df$rank_base <- rank(-numeral_freq_df$absolute_frequency_base, na.last = TRUE, ties.method = "first")
  #ifelse(numeral_freq_df$numeral %in% numeral_freq_df_base$numeral, numeral_freq_df_base$rank[match(numeral_freq_df$numeral, numeral_freq_df_base$numeral)], 9999)
  
  # mit numeral_freq_df_dev1
  numeral_freq_df$absolute_frequency_dev1 <- ifelse(numeral_freq_df$numeral %in% numeral_freq_df_dev1$numeral, numeral_freq_df_dev1$absolute_frequency[match(numeral_freq_df$numeral, numeral_freq_df_dev1$numeral)], 0)
  numeral_freq_df$relative_frequency_dev1 <- numeral_freq_df$absolute_frequency_dev1 / sum(numeral_freq_df$absolute_frequency_dev1)
  #ifelse(numeral_freq_df$numeral %in% numeral_freq_df_dev1$numeral, numeral_freq_df_dev1$relative_frequency[match(numeral_freq_df$numeral, numeral_freq_df_dev1$numeral)], 0)
  
  numeral_freq_df$rank_dev1 <- rank(-numeral_freq_df$absolute_frequency_dev1, na.last = TRUE, ties.method = "first")
  #ifelse(numeral_freq_df$numeral %in% numeral_freq_df_dev1$numeral, numeral_freq_df_dev1$rank[match(numeral_freq_df$numeral, numeral_freq_df_dev1$numeral)], NA)
  
  # Bereinigung des Datenrahmen von numeral = ""
  numeral_freq_df <- subset(numeral_freq_df, !is.na(numeral) & numeral != "")


# BASE (1)

  # (1a) Umfassende Matrix aller Akkordübergänge

    # Matrix erstellen
    base_transition_matrix <- matrix(0, nrow = length(all_numerals_base), ncol = length(all_numerals_base), dimnames = list(all_numerals_base, all_numerals_base))
    
    # Schleife durch die .tsv-Dateien
    for (file in tsv_files_base) {
      
      # Daten aus spezifischer Tabelle laden
      specific_harmonic_tab <- read.table(file.path("Daten", file), header = TRUE, sep = "\t", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, colClasses = "character")
      
      # Übergänge zählen und in die Matrix eintragen
      for (i in 1:(length(specific_harmonic_tab$numeral) - 1)) {
        current_numeral <- specific_harmonic_tab$numeral[i]
        next_numeral <- specific_harmonic_tab$numeral[i + 1]
        
        # Erste Bed. prüft, dass weder current_numeral noch next_numeral leer sind
        # Error: subscript out of bounds
        if (current_numeral != "" && next_numeral != "") {
          if (specific_harmonic_tab$localkey_is_minor[i] == tongeschlecht && specific_harmonic_tab$localkey_is_minor[i + 1] == tongeschlecht) { # Filter
            if (current_numeral %in% all_numerals_base && next_numeral %in% all_numerals_base) {
              base_transition_matrix[current_numeral, next_numeral] <- base_transition_matrix[current_numeral, next_numeral] + 1
            }
          }
        }
      }
    }
    
    # Matrix aufräumen: Zeilen und Spalten mit 0 löschen
    base_transition_matrix <- base_transition_matrix[rowSums(base_transition_matrix != 0) > 0,]
    base_transition_matrix <- base_transition_matrix[,colSums(base_transition_matrix != 0) > 0]
    
    # In der row steht der vorangehende Akkord (A), in column steht der folgende Akkord (B)
    # Die rel. Häufigkeit berechnen wir dur: p(A -> B)/p(A)
    for (i in 1:nrow(base_transition_matrix)) {
      for (j in 1:ncol(base_transition_matrix)) {
        base_transition_matrix[i,j] <- base_transition_matrix[i,j] / numeral_freq_df[numeral_freq_df$numeral == rownames(base_transition_matrix)[i], "absolute_frequency_base"]
      }  
    }
    

  # (1b) Berechnung Entropie aller Akkorde und Normierung

    # Normierungskonstante
    norm_konst <- log2(ncol(base_transition_matrix))
    
    # Dataframe erstellen um für jeden Akkord mit seiner Entropie einzufügen
    relevant_numerals <- rownames(base_transition_matrix)
    entropy_df_base <- data.frame(
      entropy = numeric(length(relevant_numerals))
    )
    
    rownames(entropy_df_base) <- relevant_numerals
    
    # Berechnung der Entropie für jeden Akkord
    for (c in relevant_numerals){
      
      start_numeral <- c
      entropy <- 0
      
      for (j in 1:ncol(base_transition_matrix)) {
        transition_freq <- base_transition_matrix[start_numeral, j] # rel. Häufigkeit d. Übergangs
        
        if (transition_freq == 0) { # Aussortieren der Fälle, in den es keinen Übergang gibt
          summand <- 0
        }
        else {
          summand <- transition_freq*log2(1/transition_freq) # Einzelner Summand
        }
        entropy <- entropy + summand # Aufaddieren zu Summe
      }
      entropy_df_base[c, "entropy"] <- entropy
    }
    
    # Normierung
    entropy_df_base$norm_entropy <- entropy_df_base$entropy / norm_konst
    
    # Wahrscheinlichkeitsgewichteter Mittelwert (Erwatungswert)
    for (g in 1:length(entropy_df_base$norm_entropy)){
      entropy_df_base$rel_freq[g] <- numeral_freq_df$relative_frequency_base[g]
    }
    
    expected_entropy_base <- sum(entropy_df_base$norm_entropy * entropy_df_base$rel_freq)

    entropy_df_base$numeral <- rownames(entropy_df_base)
    
    
# (1c) Nimmt Stichproben (!! Umfang und Anzahl können unter (0d) festgelegt werden)

    # Dataframe mit Mittelwerten der norm. bed. Entropie der Stichprobenakkorde
    sample_df_base <- data.frame(
      sample_mean = rep(NA, anz_proben)
    )
    rownames(sample_df_base) <- 1:anz_proben
    
    for (i in 1:anz_proben){
      
      probe <- sample( #Erstellt Probe
        entropy_df_base$numeral, 
        proben_umfang,
        replace = T, 
        prob = entropy_df_base$rel_freq)
      
      sample_entropy <- list() # Liste, in welche norm. bed. Entropie aller Akkorde der Stichprobe eingetragen wird
      ls_index <- 0  # Index zu Eintragen an richtiger Stelle
      
      for (d in probe){
        ls_index <- ls_index + 1
        c_norm_entropy <- entropy_df_base[d, "norm_entropy"] # Suche nach Akkord aus der Stichprobe in der Entropientabelle 
        sample_entropy[[ls_index]] <- c_norm_entropy # Eintrag in Liste
      }
      
      sample_df_base[i, "sample_mean"] <- mean(unlist(sample_entropy)) # Eintrag in Dataframe
    }
    


    
# DEV1 (2)
    
  # (2a) Umfassende Matrix aller Akkordübergänge
    
    # Matrix erstellen
    dev1_transition_matrix <- matrix(0, nrow = length(all_numerals_dev1), ncol = length(all_numerals_dev1), dimnames = list(all_numerals_dev1, all_numerals_dev1))
    
    # Schleife durch die .tsv-Dateien
    for (file in tsv_files_dev1) {
      
      # Daten aus spezifischer Tabelle laden
      specific_harmonic_tab <- read.table(file.path("Daten", file), header = TRUE, sep = "\t", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, colClasses = "character")
      
      # Übergänge zählen und in die Matrix eintragen
      for (i in 1:(length(specific_harmonic_tab$numeral) - 1)) {
        current_numeral <- specific_harmonic_tab$numeral[i]
        next_numeral <- specific_harmonic_tab$numeral[i + 1]
        
        # Erste Bed. prüft, dass weder current_numeral noch next_numeral leer sind
        # Error: subscript out of bounds
        if (current_numeral != "" && next_numeral != "") {
          if (specific_harmonic_tab$localkey_is_minor[i] == tongeschlecht && specific_harmonic_tab$localkey_is_minor[i + 1] == tongeschlecht) { # Filter
            if (current_numeral %in% all_numerals_dev1 && next_numeral %in% all_numerals_dev1) {
              dev1_transition_matrix[current_numeral, next_numeral] <- dev1_transition_matrix[current_numeral, next_numeral] + 1
            }
          }
        }
      }
    }
    
    # Matrix aufräumen: Zeilen und Spalten mit 0 löschen
    dev1_transition_matrix <- dev1_transition_matrix[rowSums(dev1_transition_matrix != 0) > 0,]
    dev1_transition_matrix <- dev1_transition_matrix[,colSums(dev1_transition_matrix != 0) > 0]
    
    # In der row steht der vorangehende Akkord (A), in column steht der folgende Akkord (B)
    # Die rel. Häufigkeit berechnen wir dur: p(A -> B)/p(A)
    for (i in 1:nrow(dev1_transition_matrix)) {
      for (j in 1:ncol(dev1_transition_matrix)) {
        dev1_transition_matrix[i,j] <- dev1_transition_matrix[i,j] / numeral_freq_df[numeral_freq_df$numeral == rownames(dev1_transition_matrix)[i], "absolute_frequency_dev1"]
      }  
    }
    
    
  # (2b) Berechnung Entropie aller Akkorde und Normierung
    
    # Normierungskonstante
    norm_konst <- log2(ncol(dev1_transition_matrix))
    
    # Dataframe erstellen um für jeden Akkord mit seiner Entropie einzufügen
    relevant_numerals <- rownames(dev1_transition_matrix)
    entropy_df_dev1 <- data.frame(
      entropy = numeric(length(relevant_numerals))
    )
    
    rownames(entropy_df_dev1) <- relevant_numerals
    
    # Berechnung der Entropie für jeden Akkord
    for (c in relevant_numerals){
      
      start_numeral <- c
      entropy <- 0
      
      for (j in 1:ncol(dev1_transition_matrix)) {
        transition_freq <- dev1_transition_matrix[start_numeral, j] # rel. Häufigkeit d. Übergangs
        
        if (transition_freq == 0) { # Aussortieren der Fälle, in den es keinen Übergang gibt
          summand <- 0
        }
        else {
          summand <- transition_freq*log2(1/transition_freq) # Einzelner Summand
        }
        entropy <- entropy + summand # Aufaddieren zu Summe
      }
      entropy_df_dev1[c, "entropy"] <- entropy
    }
    
    # Normierung
    entropy_df_dev1$norm_entropy <- entropy_df_dev1$entropy / norm_konst
    
    # Wahrscheinlichkeitsgewichteter Mittelwert (Erwatungswert)
    for (h in 1:length(entropy_df_dev1$norm_entropy)){
      entropy_df_dev1$rel_freq[h] <- numeral_freq_df$relative_frequency_dev1[h]
    }
    
    expected_entropy_dev1 <- sum(entropy_df_dev1$norm_entropy * entropy_df_dev1$rel_freq)
    
    entropy_df_dev1$numeral <- rownames(entropy_df_dev1)
    
  # (2c) Nimmt Stichproben (!! Umfang und Anzahl können unter (0d) festgelegt werden)
    
    # Dataframe mit Mittelwerten der norm. bed. Entropie der Stichprobenakkorde
    sample_df_dev1 <- data.frame(
      sample_mean = rep(NA, anz_proben)
    )
    rownames(sample_df_dev1) <- 1:anz_proben
    
    for (i in 1:anz_proben){
      
      probe <- sample( #Erstellt Probe
        entropy_df_dev1$numeral, 
        proben_umfang,
        replace = T, 
        prob = entropy_df_dev1$rel_freq)
      
      sample_entropy <- list() # Liste, in welche norm. bed. Entropie aller Akkorde der Stichprobe eingetragen wird
      ls_index <- 0  # Index zu Eintragen an richtiger Stelle
      
      for (d in probe){
        ls_index <- ls_index + 1
        c_norm_entropy <- entropy_df_dev1[d, "norm_entropy"] # Suche nach Akkord aus der Stichprobe in der Entropientabelle 
        sample_entropy[[ls_index]] <- c_norm_entropy # Eintrag in Liste
      }
      
      sample_df_dev1[i, "sample_mean"] <- mean(unlist(sample_entropy)) # Eintrag in Dataframe
    }
    
    
# (D)

    # p-Werte
        
    u_M_base <- sum(sample_df_base$sample_mean < expected_entropy_dev1, na.rm = T)
    o_M_base <- sum(sample_df_base$sample_mean > expected_entropy_dev1, na.rm = T)
    p_M_base <- (2/anz_proben) * min(u_M_base, o_M_base)
    
    u_M_dev1 <- sum(sample_df_dev1$sample_mean < expected_entropy_base, na.rm = T)
    o_M_dev1 <- sum(sample_df_dev1$sample_mean > expected_entropy_base, na.rm = T)
    p_M_dev1 <- (2/anz_proben) * min(u_M_dev1, o_M_dev1)
    

# (D)

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

names(sample_df_base) <- "sample_mean_base"
names(sample_df_dev1) <- "sample_mean_dev1"

sample_df <- cbind(sample_df_base, sample_df_dev1)


# Intervalle festlegen, in welchen die Stichprobenergebnisse in der Diagrammdarstellung aufaddiert werden
intervalle <- seq(0, 1, by = 0.0025)

# Überlappungen
overlap_df <- data.frame(interval = intervalle, overlap = 0*(1:length(intervalle)))

for (interval in intervalle) {
  
  print(interval)
  
  height_hist_base <- sum(sample_df$sample_mean_base >= interval & sample_df$sample_mean_base < interval+0.005)
  height_hist_dev1 <- sum(sample_df$sample_mean_dev1 >= interval & sample_df$sample_mean_dev1 < interval+0.005)
  
  # print(height_hist_base)
  # print(height_hist_dev1)
  
  height_overlap <- min(height_hist_base, height_hist_dev1)
  print(height_overlap)
  
  overlap_df$overlap[which(overlap_df$interval == interval)] <- height_overlap
}


library(ggplot2)

ggplot(data.frame(sample_mean_base = sample_df$sample_mean_base, sample_mean_dev1 = sample_df$sample_mean_dev1),
       aes(x = sample_mean_base)) +
  
  # Darstellung Stichproben #base
  geom_histogram(
    aes(y = after_stat(count)),
    breaks = intervalle,
    fill = farbe1,
    alpha = 0.6) +
  
  # Darstellung Stichproben #dev1
  geom_histogram(
    aes(x = sample_mean_dev1, y = after_stat(count)),
    breaks = intervalle,
    fill = farbe2,
    alpha = 0.6) +
  
  # # Darstellung Überlappungsbereiche
  # geom_bar(
  #   data = overlap_df,
  #   aes(x = interval, y = overlap),
  #   stat = "identity",
  #   fill = "red") +
  
  # Darstellung Mittelwert der Akkorde #base
  geom_vline(
    xintercept = expected_entropy_base,
    color = farbe1,
    linetype = "solid",
    linewidth = 2) +
  
  # Darstellung Mittelwert der Akkorde #dev1
  geom_vline(
    xintercept = expected_entropy_dev1,
    color = farbe2,
    linetype = "solid",
    linewidth = 2) +
  
  scale_x_continuous(limits = c(0.4, 0.6), breaks = seq(0.4, 0.6, by = 0.1)) +
  
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
  
  labs(
    # subtitle = "Mittel der norm. bed. Entropien der Akkorde mit Merkmal verglichen mit den Mitteln der norm. bed. Entropien zufälliger Stichproben",
    title = paste("µ_S =",round(expected_entropy_base, 4),"/ π_S =",round(p_M_base,4), "\n µ_Q =",round(expected_entropy_dev1, 4),"/ π_Q =",round(p_M_dev1,4)),
    x = "Norm. bed. Entropie", 
    y = "Stichproben") +
  
  theme(
    axis.line.x = element_line(color = "#4D4D4D"),
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 24),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(5, 15, 5, 5, "pt") #t,r,b,l
  )


# Speichere das Diagramm als PNG-Datei
ggsave("ana3c-BG-compare-entropy-reduced_LVBS-LVBQ-major-480dpi.png", plot = last_plot(), path = "Ergebnisse", width = 8, height = 5, dpi = 480)
