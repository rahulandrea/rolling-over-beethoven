# Hypothesentest: Symmetrie der Akkordübergänge
# H0: sym_g = 1 / H1: sym_g < 1

# (1) Übergangswahrscheinlichkeiten Matrix -> transition_df
# (2) Filter nach Bed.: c1 != c2 && p(c1 -> c2) != 0 && p(c2 -> c1) != 0
# (3) Wähle Stichproben (k =50 000) à 100 Bigramme
# (4) Für alle Stichproben Mittel d. Bigramm-Symmetrie berrechnen
# (5) ggplot

library(ggplot2)
library(dplyr)

setwd("/Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturaarbeit/Analyse/harmonic-ana-6/data")

# Globale Parameter
g <- 0 # Filter lokale Tonart nach Tongeschlecht # 0 = Dur, 1 = Moll

# (1) Aus harmonic-ana-5x
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
    raw_transition_matrix <- matrix(0, nrow = length(all_chords), ncol = length(all_chords), dimnames = list(all_chords, all_chords))
    # for-Schleife zum Übergänge zählen
    # Schleife durch die .tsv-Dateien durchlaufen Prog. unten # Oben wurde schon die Liste der .tsv-Dateien erstellt
    for (file in tsv_files) {
      # Daten aus spezifischer Tabelle laden
      specific_harmonic_tab <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
      # Übergänge zählen und in die Matrix eintragen
      for (i in 1:(length(specific_harmonic_tab$chord) - 1)) {
        current_chord <- specific_harmonic_tab$chord[i]
        next_chord <- specific_harmonic_tab$chord[i + 1]
        # Prüft, dass weder current_chord noch next_chord leer sind
        if (current_chord != "" && next_chord != "") {
          # Stellt Bed. an Dur- bzw. Moll-Abschnitt 
          if (specific_harmonic_tab$localkey_is_minor[i] == g && specific_harmonic_tab$localkey_is_minor[i + 1] == g) {
            # Fordert, dass current_chord und next_chord in der Matrix sind 
            if (current_chord %in% all_chords && next_chord %in% all_chords) {
              raw_transition_matrix[current_chord, next_chord] <- raw_transition_matrix[current_chord, next_chord] + 1
            }
          }
        }
      }
    }
    
    ### INFO ### 
    # Matrix [Spalte, Zeile] ist wie folgt aufgebaut:
    # - In der row (Spalte) steht der aktuelle Akkord (A)
    # - In der column (Zeile) steht der kommende Akkord (B)
    # - In den Feldern steht die Übergangswahrscheinlichkeit p(A -> B)
    
    # Umwandlung in Dataframe # Var1 = A,  Var2 = B, Freq = abs Übergangshäufigkeit A -> B
    transition_matrix <- raw_transition_matrix
    # Rel. Häufigkeit des Übergangs in die Felder: Es wird p(A -> B) durch p(A) geteilt
    for (i in 1:nrow(transition_matrix)) {
      for (j in 1:ncol(transition_matrix)) {
        transition_matrix[i,j] <- transition_matrix[i,j] / chord_freq_df[chord_freq_df$chord == rownames(transition_matrix)[i], "absolute_frequency"]
      }  
    }
    
    transition_df <- as.data.frame(as.table(raw_transition_matrix))
    only_for_pab <- as.data.frame(as.table(transition_matrix))
    transition_df$pab <- only_for_pab$Freq
    
# (2)
    transition_df$Var1 <- as.character(transition_df$Var1)
    transition_df$Var2 <- as.character(transition_df$Var2)
    
    filtered_transition_df <- transition_df[transition_df$Var1 != transition_df$Var2, , drop = FALSE]
    
    # Behalte Zeilen, wo Freq nicht gleich 0 ist
    filtered_transition_df <- subset(filtered_transition_df, Freq != 0)
    
    # Entferne Zeilen, wo Freq gleich 0 ist und es keine komplementäre Zeile gibt
    filtered_transition_df <- filtered_transition_df[!(filtered_transition_df$Freq == 0 & 
                                                         !(paste(filtered_transition_df$Var2, 
                                                                 filtered_transition_df$Var1) %in% 
                                                             paste(transition_df$Var1, 
                                                                   transition_df$Var2))), ]
    
    # Füge ldf_nr zum Dataframe hinzu
    bigramm_df <- filtered_transition_df
    bigramm_df$lfd_nr <- seq_len(nrow(bigramm_df))
    
# (3) und (4) Aus harmonic-ana-5x
    proben_umfang <- 100 # Grösse (Umfang) d. Probe
    anz_proben <- 2000 # Anz. Proben
    
    # Stichprobe
    stichprobe_df <- data.frame(
      mittlere_symmetrie = rep(NA, anz_proben)
    )
    
    rownames(stichprobe_df) <- 1:anz_proben
    
    # Schleife für die einzelne Stichprobe
    for (i in 1:anz_proben){
      probe <- sample(
        bigramm_df$lfd_nr, 
        proben_umfang,
        replace = T, 
        prob = bigramm_df$Freq)
     
      sym_ls <- list() # Leere Liste, in die Symmetrie eingetragen wird
      ls_index <- 0  # Index
      for (bigramm in probe){ # "bigramm" ist die Nr. des Bigramms im bigramm_df
        ls_index <- ls_index + 1
        
        direction_1 <- bigramm
            # Filtern Sie das Dataframe, um die Zeile mit dem gesuchten lfd-Wert zu erhalten
            gesuchte_zeile <- bigramm_df %>%
              filter(lfd_nr == bigramm)
            # Extrahieren Sie die Werte für Var1 und Var2 aus der gefundenen Zeile
            Wert_A <- gesuchte_zeile$Var1
            Wert_B <- gesuchte_zeile$Var2
            # Suchen Sie nach der komplementären Zeile
            komplementaere_zeile <- bigramm_df %>%
              filter(Var1 == Wert_B, Var2 == Wert_A)
        direction_2 <- komplementaere_zeile$lfd_nr
        
        bigramm_symetrie <- min(bigramm_df$pab[direction_1], bigramm_df$pab[direction_2])
        
        sym_ls[[ls_index]] <- bigramm_symetrie
      }
      
      proben_mittel <- mean(unlist(sym_ls))
      stichprobe_df[i, "mittlere_symmetrie"] <- proben_mittel
    }
    
# (5)
    # Intervalle festlegen
    intervalle <- seq(0, 1, by = 0.005)
    
    u <- sum(stichprobe_df$mittlere_symmetrie == 1, na.rm = T)
    p <- u/anz_proben
    # Diagramm erstellen
    ggplot(data.frame(x = stichprobe_df), aes(x = mittlere_symmetrie)) +
      # Dartsellung von (B)
      geom_histogram(
        aes(y = after_stat(count)),
        breaks = intervalle,
        fill = "#7A9DCF") +
      # Darstellung von (A)
      geom_vline(
        xintercept = 1,
        color = "#215CAF",
        linetype = "solid",
        linewidth = 2) +
      # Allgemeines
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
      labs(
      # title = paste("Alterationen \n µ_M =",round(A_entropy_mean, 4),"\n π_M =",round(p,4)),
        x = "sym_g", 
        y = "Stichproben") +
      #theme_minimal()
      #annotate(
      #  geom="text",
      #  x=110,
      #  y=0.07,
      #  label=paste("R^2 =",round(r_squared, 4)),
      #  size=8)+
      theme(
        axis.line.x=element_line(color ="#4D4D4D"),
        plot.title =element_text(size=24,hjust = 0.5),
      # axis.text.y = element_blank(),
      # axis.title.y = element_blank(),
        axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        plot.margin=margin(10, 10, 5, 10, "pt") #t,r,b,l
      )
    
    # Speichere das Diagramm als PNG-Datei
    #ggsave("entropy_x_graph_final-480dpi-alt-major.png", plot = last_plot(), width = 2.7, height = 7, dpi = 480)
    
      
   
    
    
    