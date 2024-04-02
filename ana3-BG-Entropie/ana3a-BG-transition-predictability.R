# ana3-BG-Entropie: ana3a-BG-transition-predictability.R
# last review on 2024-03-24 by RAG

# (0) Zuallererst wird der Untersuchungsgegenstand und Parameter festgelegt:
#     (0a) Die zu untersuchenden Dateien
#     (0b) Die zu untersuchende Akkordeigenschaft
#     (0c) Es besteht die Möglichkeit, nach Tongeschlecht der Tonartabschnitten zu filtern
#     (0d) Hier können Umfang und Anzahl der Stichproben für den Hypothesentest festgelegt werden
# (A-B) Dieses Prog. nutzt "ana1a-UG-rank-freq.R" um ein Dataframes zu erstellen, welches jedem
#       Akkord (chord) einen Häufigkeitsrang (rank) und die relative Häufigkeit (relative_frequency) zuordnet
#     (A) "4a-combined-data.R" um eine grosse .tsv Tabelle über alle Stücke und Sätze zu erhalten
#     (B) Erstellt ein Rang-Häufigkeits-Dataframes

# Weiter besteht dieses Prog. aus zwei Hauptteilen:
# (1) Nimmt Stichproben aus allen Akkorden und berechnet die norm. bed. Entropie für diese
#     (für einen Hinweis zur Laufzeit und Effizienz siehe den Bericht "ana3-Bericht.pdf")
#     (1a) Erstellt eine umfassende Matrix aller Akkordübergänge
#     (1b) Berechnet Entropie für alle Akkorde und Normierung dieser. Erstellt ein Dataframe, welches jedem 
#          Akkord (chord) seine bed. Entropie (entropy) und seine norm. bed. Entropie zuordnet (norm_entropy)
#     (1c) Wählt Stichproben von Akkorden (!! Umfang und Anzahl können unter (0d) festgelegt werden) und berechnet
#          den Mittelwert der Entropie der Stichprobenakkorde. Eintrag in Tabelle für jede Stichprobe.
# (2) Berechnet die mittlere norm. bed. Entropie aller Akkorde mit Akkordeigenschaft 
#     (!! Akkordeigenschaft kann unter (0b) festgelegt werden)
#     (2a) Erstellt eine Matrix aller Akkordübergänge, die von einem Akkord mit Akkordeigenschaft ausgehen
#     (2b) Berechnet Entropie für die Akkorde mit Akkordeigenschaft und Normierung dieser. Erstellt ein Dataframe,
#          welches jedem Akkord (chord) seine bed. Entr. (entropy) und norm. bed. Entr. zuordnet (norm_entropy)
#     (2c) Berechnet den Mittelwert

# (C) Berechnet den p-Wert
# (D) Zuletzt ist es möglich ein Diagramm mit den mittl. norm. bed. Entr. der Stichproben und der mittl. 
#     norm. bed. Entr. der Akkorde mit Akkordeigenschaft ausgeben zu lassen. Die Intervalle, in welchen die
#     Stichproben für die Darstellung aufaddiert werden, können hier angepasst werden.


setwd("/Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturaarbeit")


# (0)

  # (0a)
    # Liste der zu untersuchenden .tsv-Dateien erstellen
    # Dateien benamst nach [KOM]-[A][xx]-M[x].tsv 
    #   wobei KOM für das Komponistenkürzel steht (LVB für L. v. Beethoven, WAM für W. A. Mozart),
    #   A für die Stückart (S für Sonate, Q für Streicherquartett) mit der üblichen Nummerierung (xx aus 00-99),
    #   M für den Satz mit Nummer (x aus 0-9)
    # Für weiteres siehe README
    # Falls nur einzelne gewünscht: c("WAM-S01-M1.tsv", "LVB-S01-M1.tsv", "LVB-S01-M2.tsv")
    
    tsv_files <- list.files(path = "Daten", pattern = "LVB-Q[0-9]{2}-M[0-9].tsv")
  
  # (0b)  
    # Zu untersuchende Akkordeigenschaft festlegen
    # Für Vorhalte: chord_feature <- Vorh, Für Umkehrungen: chord_feature <- Inv,
    # Für Alterationen: chord_feature <- Alt, Für Orgelpunkte: chord_feature <- OrgP,
    # Tonikalisierungen: chord_feature <- Tonik
    
    chord_feature <- "Vorh"
    
  # (0c)
    # Zu untersuchendes Tongeschlecht festlegen
    # Für Dur: tongeschlecht <- 0, für Moll: tongeschlecht <- 1
    tongeschlecht <- 1

  # (0d)
    # Festlegen des Umfangs einer Probe und der Anzahl Proben 
    proben_umfang <- 100 # Grösse (Umfang) d. Probe
    anz_proben <- 10000 # Anz. Proben
    
    
  # Funktion über die Akkordeigenschaft

    if (chord_feature == "Vorh"){
      feature_filter <- function(i){
        if (!is.na(specific_harmonic_tab$changes[i]) && specific_harmonic_tab$changes[i] != "" && specific_harmonic_tab$changes[i] != 0 && !any(grepl("\\+", specific_harmonic_tab$changes[i]))){
          1
        } else {
          0
        }
      }
    } else if (chord_feature == "Inv"){
      feature_filter <- function(i){
        if (!is.na(specific_harmonic_tab$figbass[i]) && specific_harmonic_tab$figbass[i] != "" && specific_harmonic_tab$figbass[i] != 0 && specific_harmonic_tab$figbass[i] != 7){
          1
        } else {
          0
        }
      }
    } else if (chord_feature == "Alt"){
      feature_filter <- function(i){
        if (substr(specific_harmonic_tab$chord[i],1,1) == "#" || substr(specific_harmonic_tab$chord[i],1,1) == "b"){
          1
        } else {
          0
        }
      }
    } else if (chord_feature == "OrgP"){
      feature_filter <- function(i){
        if (!is.na(specific_harmonic_tab$pedal[i]) && specific_harmonic_tab$pedal[i] != "" && specific_harmonic_tab$pedal[i] != 0){
          1
        } else {
          0
        }
      }
    } else if (chord_feature == "Tonik"){
      feature_filter <- function(i){
        if (!is.na(specific_harmonic_tab$relativeroot[i]) && specific_harmonic_tab$relativeroot[i] != ""){
          1
        } else {
          0
        }
      }
    } else {
      stop("unknown value for variable chord_feature (see line 45)")
    }

#start_time <- system.time()
    
# (A)
    
    # Liste von Datenrahmen erstellen, um Daten zu speichern
    data_list <- list()
    
    # Schleife durch die ausgewählten .tsv-Dateien und Daten in der Liste speichern
    for (file in tsv_files) {
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
    chord_freq_df <- data.frame(chord = names(chord_freq), absolute_frequency = as.vector(chord_freq))
    
    # Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
    chord_freq_df <- chord_freq_df[order(chord_freq_df$absolute_frequency, decreasing = TRUE), ]
    
    # Berechne relative Häufigkeiten
    chord_freq_df$relative_frequency <- chord_freq_df$absolute_frequency / sum(chord_freq_df$absolute_frequency)
    
    # Rang erstellen
    chord_freq_df$rank <- 1:nrow(chord_freq_df)
    
    # Zeilen löscehn, in denen die Spalte "chord" leer ist
    chord_freq_df <- subset(chord_freq_df, !chord == "")

    all_chords <- unique(chord_freq_df$chord)
    
# (1)

  # (1a) Umfassende Matrix aller Akkordübergänge
    
    # Matrix erstellen
    all_transition_matrix <- matrix(0, nrow = length(all_chords), ncol = length(all_chords), dimnames = list(all_chords, all_chords))
    
    # Schleife durch die .tsv-Dateien
    for (file in tsv_files) {
      
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
            if (current_chord %in% all_chords && next_chord %in% all_chords) {
              all_transition_matrix[current_chord, next_chord] <- all_transition_matrix[current_chord, next_chord] + 1
            }
          }
        }
      }
    }
    
    # Matrix aufräumen: Zeilen und Spalten mit 0 löschen
    all_transition_matrix <- all_transition_matrix[rowSums(all_transition_matrix != 0) > 0,]
    all_transition_matrix <- all_transition_matrix[,colSums(all_transition_matrix != 0) > 0]
    
    # In der row steht der vorangehende Akkord (A), in column steht der folgende Akkord (B)
    # Die rel. Häufigkeit berechnen wir dur: p(A -> B)/p(A)
    for (i in 1:nrow(all_transition_matrix)) {
      for (j in 1:ncol(all_transition_matrix)) {
        all_transition_matrix[i,j] <- all_transition_matrix[i,j] / chord_freq_df[chord_freq_df$chord == rownames(all_transition_matrix)[i], "absolute_frequency"]
      }  
    }
    
    
  # (1b) Berechnung Entropie aller Akkorde und Normierung
    
    # Normierungskonstante
    norm_konst <- log2(ncol(all_transition_matrix))
    
    # Dataframe erstellen um für jeden Akkord mit seiner Entropie einzufügen
    relevant_chords <- rownames(all_transition_matrix)
    entropy_df <- data.frame(
      entropy = numeric(length(relevant_chords))
    )
    
    rownames(entropy_df) <- relevant_chords
    
    # Berechnung der Entropie für jeden Akkord
    for (c in relevant_chords){
      
      start_chord <- c
      entropy <- 0
      
      for (j in 1:ncol(all_transition_matrix)) {
        transition_freq <- all_transition_matrix[start_chord, j] # rel. Häufigkeit d. Übergangs

        if (transition_freq == 0) { # Aussortieren der Fälle, in den es keinen Übergang gibt
          summand <- 0
        }
        else {
          summand <- transition_freq*log2(1/transition_freq) # Einzelner Summand
        }
        entropy <- entropy + summand # Aufaddieren zu Summe
      }
      entropy_df[c, "entropy"] <- entropy
    }
    
    # Normierung
    entropy_df$norm_entropy <- entropy_df$entropy / norm_konst

    
  # (1c) Nimmt Stichproben (!! Umfang und Anzahl können unter (0d) festgelegt werden)

    # Dataframe mit Mittelwerten der norm. bed. Entropie der Stichprobenakkorde
    sample_df <- data.frame(
      sample_mean = rep(NA, anz_proben)
    )
    rownames(sample_df) <- 1:anz_proben
    
    for (i in 1:anz_proben){
      
      probe <- sample( #Erstellt Probe
        all_chords, 
        proben_umfang,
        replace = T, 
        prob = chord_freq_df$relative_frequency)
      
      sample_entropy <- list() # Liste, in welche norm. bed. Entropie aller Akkorde der Stichprobe eingetragen wird
      ls_index <- 0  # Index zu Eintragen an richtiger Stelle
      
      for (d in probe){
        ls_index <- ls_index + 1
        c_norm_entropy <- entropy_df[d, "norm_entropy"] # Suche nach Akkord aus der Stichprobe in der Entropientabelle 
        sample_entropy[[ls_index]] <- c_norm_entropy # Eintrag in Liste
      }
      
      sample_df[i, "sample_mean"] <- mean(unlist(sample_entropy)) # Eintrag in Dataframe
    }
    
    
# (2)
    
  # (2a) Matrix aller Akkordübergänge, die von einem Akkord mit Akkordeigenschaft ausgehen
    #      (!! Akkordeigenschaft kann unter (0b) festgelegt werden)
    
    # Matrix erstellen
    feat_transition_matrix <- matrix(0, nrow = length(all_chords), ncol = length(all_chords), dimnames = list(all_chords, all_chords))
    
    # Schleife durch die .tsv-Dateien
    for (file in tsv_files) {
      
      # Daten aus spezifischer Tabelle laden
      specific_harmonic_tab <- read.table(file.path("Daten", file), header = TRUE, sep = "\t", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, colClasses = "character")
      
      # Übergänge zählen und in die Matrix eintragen
      for (i in 1:(length(specific_harmonic_tab$chord) - 1)) {
        current_chord <- specific_harmonic_tab$chord[i]
        next_chord <- specific_harmonic_tab$chord[i + 1]
        
        # Erste Bed. prüft, dass weder current_chord noch next_chord leer sind
        # Error: subscript out of bounds
        if (current_chord != "" && next_chord != "") {
          if (specific_harmonic_tab$localkey_is_minor[i] == tongeschlecht && specific_harmonic_tab$localkey_is_minor[i + 1] == tongeschlecht) { # Filter Tongeschlecht
            if (feature_filter(i) == 1){ # Filter Akkordeigenschaft
              if (current_chord %in% all_chords && next_chord %in% all_chords) {
                feat_transition_matrix[current_chord, next_chord] <- feat_transition_matrix[current_chord, next_chord] + 1
              }
            }
          }
        }
      }
    }
    
    # Matrix aufräumen: Zeilen und Spalten mit 0 löschen
    feat_transition_matrix <- feat_transition_matrix[rowSums(feat_transition_matrix != 0) > 0,]
    feat_transition_matrix <- feat_transition_matrix[,colSums(feat_transition_matrix != 0) > 0]
    
    # In der row steht der vorangehende Akkord (A), in column steht der folgende Akkord (B)
    # Die rel. Häufigkeit berechnen wir dur: p(A -> B)/p(A)
    for (i in 1:nrow(feat_transition_matrix)) {
      for (j in 1:ncol(feat_transition_matrix)) {
        feat_transition_matrix[i,j] <- feat_transition_matrix[i,j] / chord_freq_df[chord_freq_df$chord == rownames(feat_transition_matrix)[i], "absolute_frequency"]
      }  
    }
    
    
  # (2b) Berechnung Entropie aller Akkorde und Normierung
    
    # Normierungskonstante
    norm_konst_feat <- log2(ncol(feat_transition_matrix))
    
    # Dataframe erstellen um für jeden Akkord mit seiner Entropie einzufügen
    relevant_chords_feat <- rownames(feat_transition_matrix)
    feat_entropy_df <- data.frame(
      entropy = numeric(length(relevant_chords_feat))
    )
    
    rownames(feat_entropy_df) <- relevant_chords_feat
    
    # Berechnung der Entropie für jeden Akkord
    for (c in relevant_chords_feat){
      
      start_chord <- c
      entropy <- 0
      
      for (j in 1:ncol(feat_transition_matrix)) {
        transition_freq <- feat_transition_matrix[start_chord, j] # rel. Häufigkeit d. Übergangs
        
        if (transition_freq == 0) { # Aussortieren der Fälle, in den es keinen Übergang gibt
          summand <- 0
        }
        else {
          summand <- transition_freq*log2(1/transition_freq) # Einzelner Summand
        }
        entropy <- entropy + summand # Aufaddieren zu Summe
      }
      feat_entropy_df[c, "entropy"] <- entropy
    }
    
    # Normierung
    feat_entropy_df$norm_entropy <- feat_entropy_df$entropy / norm_konst_feat
    
    
  # (2c) Mittelwert
    entropy_mean_feat_raw <- mean(feat_entropy_df$norm_entropy)
    entropy_mean_feat <- mean(subset(feat_entropy_df$norm_entropy, feat_entropy_df$norm_entropy > 0))
    
    # Erwartungswert (Wahrscheinlichkeitsgewichteter Mittelwert)
    feat_entropy_df$rel_freq <- rowSums(feat_transition_matrix)/ sum(feat_transition_matrix)
    
    entropy_expected_feat <- sum(feat_entropy_df$norm_entropy * feat_entropy_df$rel_freq)
    
      
# (D)
    
    # p-Wert
    u_M <- sum(sample_df$sample_mean < entropy_mean_feat, na.rm = T)
    o_M <- sum(sample_df$sample_mean > entropy_mean_feat, na.rm = T)
    p_M <- (2/anz_proben) * min(u_M, o_M)
    

# end_time <- system.time()
# laufzeit <- end_time - start_time
# print(laufzeit)

# (E)
    
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
    
    # Intervalle festlegen, in welchen die Stichprobenergebnisse in der Diagrammdarstellung aufaddiert werden
    intervalle <- seq(0, 1, by = 0.005)
    
    
    library(ggplot2)
    
    ggplot(data.frame(x = sample_df), aes(x = sample_mean)) +
      
      # Darstellung Stichproben
      geom_histogram(
        aes(y = after_stat(count)),
        breaks = intervalle,
        fill = farbe2_light) +
      
      # Darstellung Erwartungswert (wahrscheinlichkeitsgew. Mittelwert)
      geom_vline(
        xintercept = entropy_mean_feat,
        color = farbe2,
        linetype = "solid",
        linewidth = 2) +
      
      # #  Mittelwert der Akkorde mit Eigenschaft
      # geom_vline(
      #   xintercept = entropy_mean_feat,
      #   color = farbe2,
      #   linetype = "solid",
      #   linewidth = 2) +
      # 
      # # unkorr. Mittelwert
      # geom_vline(
      #   xintercept = entropy_mean_feat_raw,
      #   color = mark1,
      #   linetype = "solid",
      #   linewidth = 2) +
      
      scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.2)) +
      
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1400)) +

      labs(
        # subtitle = "Mittel der norm. bed. Entropien der Akkorde mit Merkmal verglichen mit den Mitteln der norm. bed. Entropien zufälliger Stichproben",
        title = paste("Vorhalte \n µ_M =",round(entropy_mean_feat, 4),"\n π_M =",round(p_M,4)),
        x = "Norm. bed. Entropie", 
        y = "Stichproben") +
      
      theme(
        axis.line.x=element_line(color = "#4D4D4D"),
        plot.title =element_text(size = 24, hjust = 0.5),
        axis.text=element_text(size = 24),
        axis.title=element_text(size = 24),
        #axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        plot.margin=margin(5, 15, 5, 5, "pt") #t,r,b,l
      )

    
    # Speichere das Diagramm als PNG-Datei
    ggsave("ana3a-BG-transition-predictability_LVBQ-Vorh-minor-480dpi.png", plot = last_plot(), path = "Ergebnisse/In-Skript", width = 4, height = 7, dpi = 480)


# # Bed. Vorhalt
# if (!is.na(A_specific_harmonic_tab$changes[i]) && A_specific_harmonic_tab$changes[i] != "" && A_specific_harmonic_tab$changes[i] != 0 && !any(grepl("\\+", A_specific_harmonic_tab$changes[i]))) {}
#   
# # Bed. Inv ("figbass" ungleich 7)
# if (!is.na(A_specific_harmonic_tab$figbass[i]) && A_specific_harmonic_tab$figbass[i] != "" && A_specific_harmonic_tab$figbass[i] != 0 && A_specific_harmonic_tab$figbass[i] != 7) {}
#     
# # Bed. Alt
# if (substr(A_specific_harmonic_tab$chord[i],1,1) == "#" || substr(A_specific_harmonic_tab$chord[i],1,1) == "b") {}
#   print(substr(A_specific_harmonic_tab$chord[i],1,1))
#       
# # Bed. OrgP
# if (!is.na(A_specific_harmonic_tab$pedal[i]) && A_specific_harmonic_tab$pedal[i] != "" && A_specific_harmonic_tab$pedal[i] != 0) {}
#         
# # Bed. Tonik
# if (!is.na(A_specific_harmonic_tab$relativeroot[i]) && A_specific_harmonic_tab$relativeroot[i] != "") {}