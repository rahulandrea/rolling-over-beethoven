# ana4-BG-Symmetrie: ana4a-BG-transition-symmetry.R
# last review on 2024-03-29 by RAG

# (0) Zuallererst wird der Untersuchungsgegenstand und Parameter festgelegt:
#     (0a) Die zu untersuchenden Dateien
#     (0b) Es besteht die Möglichkeit, nach Tongeschlecht der Tonartabschnitten zu filtern
#     (0c) Hier können Umfang und Anzahl der Stichproben für den Hypothesentest festgelegt werden

# BESCHREIBUNG ÜBERARBEITEN

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

    tsv_files <- list.files(path = "Daten", pattern = "LVB-S[0-9]{2}-M[0-9].tsv")

  # (0b)
    # Zu untersuchendes Tongeschlecht festlegen
    # Für Dur: tongeschlecht <- 0, für Moll: tongeschlecht <- 1
    tongeschlecht <- 0

  # (0c)
    # Festlegen des Umfangs einer Probe und der Anzahl Proben 
    proben_umfang <- 100 # Grösse (Umfang) d. Probe
    anz_proben <- 2000 # Anz. Proben


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
    
    # Dataframe mit allen Übergängen erstellen

    all_transition_df <- as.data.frame(as.table(all_transition_matrix))
    #only_for_pab <- as.data.frame(as.table(transition_matrix))
    #transition_df$pab <- only_for_pab$Freq

    # Einträge als Text verstehen
    all_transition_df$Var1 <- as.character(all_transition_df$Var1)
    all_transition_df$Var2 <- as.character(all_transition_df$Var2)
    
    bigramm_df <- all_transition_df
    # Übergänge in denselben Akkord ignorieren (sind per Def. ausgeschlossen)
    bigramm_df <- bigramm_df[bigramm_df$Var1 != bigramm_df$Var2, , drop = FALSE]

    # Genauso Übergänge mit Wahrscheinlichkeit 0
    bigramm_df <- subset(bigramm_df, Freq != 0)
    bigramm_df <- bigramm_df[!(bigramm_df$Freq == 0 & !(paste(bigramm_df$Var2, bigramm_df$Var1) %in% paste(all_transition_df$Var1, all_transition_df$Var2))), ]

    # Lfd Nr hinzufügen
    bigramm_df$lfd_nr <- seq_len(nrow(bigramm_df))
    
    
    
# (2) Stichproben nehmen
    sample_df <- data.frame(
      sample_mean = rep(NA, anz_proben)
    )
    
    rownames(sample_df) <- 1:anz_proben
    
    for (i in 1:anz_proben){
      
      probe <- sample( #Erstellt Probe
        bigramm_df$lfd_nr, 
        proben_umfang,
        replace = T, 
        prob = bigramm_df$Freq)
      
      sample_symmetry <- list() # Liste, in welche norm. bed. Entropie aller Akkorde der Stichprobe eingetragen wird
      ls_index <- 0  # Index zu Eintragen an richtiger Stelle
    
      for (d in probe){
        ls_index <- ls_index + 1
        
        direct <- d
        # Sucht die entsprechende Zeile heraus
        ges_zeile <- bigramm_df %>%
          filter(lfd_nr == d)
        
        # Extrahiert die Werte für Var1 und Var2 aus der Zeile
        Wert_A <- ges_zeile$Var1
        Wert_B <- ges_zeile$Var2
        
        # Sucht nach der komplementären Zeile
        komplement_zeile <- bigramm_df %>%
          filter(Var1 == Wert_B, Var2 == Wert_A)
        reverse <- komplement_zeile$lfd_nr
        
        # Berechnet die Symmetrie gem. Definition
        bigramm_symetrie <- min(bigramm_df$Freq[direct], bigramm_df$Freq[reverse])
        
        sample_symmetry[[ls_index]] <- bigramm_symetrie
      }
      
      sample_df[i, "sample_mean"] <- mean(unlist(sample_symmetry))
    }
    
    
    # (2b) Mittelwert
    symmetry_mean_expected <- mean(sample_df$sample_mean)
    
    
# (C)
    
    # p-Wert
    u_g <- sum(sample_df$sample_mean < 1, na.rm = T)
    o_g <- sum(sample_df$sample_mean > 1, na.rm = T)
    p_g <- (2/anz_proben) * min(u_g, o_g)
    

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
    
    # Intervalle festlegen, in welchen die Stichprobenergebnisse in der Diagrammdarstellung aufaddiert werden
    intervalle <- seq(0, 1, by = 0.005)
    
    
    library(ggplot2)
    
    ggplot(data.frame(x = sample_df), aes(x = sample_mean)) +
      
    # Darstellung Stichproben
    geom_histogram(
      aes(y = after_stat(count)),
      breaks = intervalle,
      fill = farbe1_light) +
      
    # Darstellung Erwartungswert (wahrscheinlichkeitsgew. Mittelwert)
    geom_vline(
      xintercept = 1,
      color = farbe2,
      linetype = "solid",
      linewidth = 1) +
      
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
    
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      
    scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
      
    labs(
      # subtitle = "Mittel der norm. bed. Entropien der Akkorde mit Merkmal verglichen mit den Mitteln der norm. bed. Entropien zufälliger Stichproben",
      title = paste("Symmetrie von Bigrammen \n sym(g) =",round(symmetry_mean_expected, 4),"\n π_g =",round(p_g,4)),
      x = "Bigramm-Symmetrien", 
      y = "Stichproben") +
      
    theme(
      axis.line.x=element_line(color = "#4D4D4D"),
      plot.title =element_text(size = 24, hjust = 0.5),
      axis.text=element_text(size = 24),
      axis.title=element_text(size = 24),
      plot.margin=margin(5, 15, 5, 5, "pt") #t,r,b,l
    )
    
  # Speichere das Diagramm als PNG-Datei
  ggsave("ana4a-BG-transition-symmetry_LVBS-major-480dpi.png", plot = last_plot(), path = "Ergebnisse", width = 6, height = 4, dpi = 480)
    
    
