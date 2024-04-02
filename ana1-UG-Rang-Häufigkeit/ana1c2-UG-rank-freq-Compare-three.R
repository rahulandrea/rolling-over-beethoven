# ana1-UG-Rang-Häufigkeit: ana1c-UG-rank-freq-Compare-three.R
# last review on 2024-04-15 by RAG

# Zuallererst werden die zu untersuchenden Dateien festgelegt. Es können drei Gruppen von Daten verglichen werden: 
# Eine Basisgruppe (base) und zwei Vergleichsgruppe (dev1 und dev2)
# (A-C) Dieses Prog. nutzt "ana1a-UG-rank-freq.R" um zwei Dataframes (für jeden Datensatz) zu erstellen, welches
#       jedem Akkord (chord) einen Häufigkeitsrang (rank) und die relative Häufigkeit (relative_frequency) zuordnet
#       Dies wird je für die Basisgruppe (base) und die Vergleichsgruppen (dev1 und dev2) durchlaufen.
#     (A) "4a-combined-data.R" um eine grosse .tsv Tabelle über alle Stücke und Sätze zu erhalten
#     (B) Es besteht die Möglichkeit, nach Tongeschlecht der Tonartabschnitten zu filtern
#     (C) Erstellt die Rang-Häufigkeits-Dataframes je Datensatz
# 
# (X) Es werden die beiden Rang-Häufigkeits-Dataframes der Basisgruppe (base) und Vergleichsgruppen (dev1 und dev2) 
#     werden zu einem gesamthaften kombiniert. Orientiertung an der Basisgruppe. Jeweilige Erweiterung der Zeilen..
#
#     ..
# (Y)  Weiter ist es möglich die erhaltenen Werte im Dataframe mit der Mandelbrot-Zipf-Kurve zu vergleichen
# (D) Es ist möglich sich ein Diagramm ausgeben zu lassen

setwd("/Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturaarbeit")


# Liste der zu untersuchenden .tsv-Dateien erstellen
# Dateien benamst nach [KOM]-[A][xx]-M[x].tsv 
#   wobei KOM für das Komponistenkürzel steht (LVB für L. v. Beethoven, WAM für W. A. Mozart),
#   A für die Stückart (S für Sonate, Q für Streicherquartett) mit der üblichen Nummerierung (xx aus 00-99),
#   M für den Satz mit Nummer (x aus 0-9)
# Für weiteres siehe README
# Falls nur einzelne gewünscht: c("Daten/WAM-S01-M1.tsv", "Daten/LVB-S01-M1.tsv", "Daten/LVB-S01-M2.tsv")

#tsv_files <- list.files(path = "Daten", pattern = "[A-Z]{3}-S[0-9]{2}-M[0-9].tsv")
tsv_files_base <- list.files(path = "Daten", pattern = "LVB-S[0-9]{2}-M[0-9].tsv")
tsv_files_dev1 <- list.files(path = "Daten", pattern = "LVB-Q[0-9]{2}-M[0-9].tsv")
tsv_files_dev2 <- list.files(path = "Daten", pattern = "LVB-S(1[6-9]|2[0-6])-M[0-9].tsv")

all_files <- c(tsv_files_base, tsv_files_dev1, tsv_files_dev2)

# Zu untersuchendes Tongeschlecht festlegen (damit beim Auslesen von base und dev1 das selbe Tonggeschlecht 
# rausgefiltert wird): Siehe dazu (BASE)/(B), (DEV1)/(B) bzw. (DEV2)/(B) 
# Für Dur: tongeschlecht <- 0, für Moll: tongeschlecht <- 1
tongeschlecht <- 1


# (BASE)
    
    # (A): source("Vers-B/4-Datenbereinigung/4a-combined-data.R")
    
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
    
    
    # (B) Filter nach Tongeschlecht der Tonartabschnitte:
        oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == tongeschlecht)
    
    
    # (C) 
        
        # Erstelle eine Tabelle mit den verschiedenen Akkorden und ihren absoluten Häufigkeiten
        chord_freq_base <- table(oval_harmonic_tab$chord)
        
        # Umwandeln der Tabelle in einen Datenrahmen
        chord_freq_df_base <- data.frame(chord = names(chord_freq_base), absolute_frequency = as.vector(chord_freq_base))
        
        # Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
        chord_freq_df_base <- chord_freq_df_base[order(chord_freq_df_base$absolute_frequency, decreasing = TRUE), ]
        
        # Berechne relative Häufigkeiten
        chord_freq_df_base$relative_frequency <- chord_freq_df_base$absolute_frequency / sum(chord_freq_df_base$absolute_frequency)
        
        # Rang erstellen
        chord_freq_df_base$rank <- 1:nrow(chord_freq_df_base)


# (DEV1)
        
        # (A): source("Vers-B/4-Datenbereinigung/4a-combined-data.R")
        
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
        
        
        # (B) Filter nach Tongeschlecht der Tonartabschnitte: 
        oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == tongeschlecht)
        
        
        # (C) 
        
        # Erstelle eine Tabelle mit den verschiedenen Akkorden und ihren absoluten Häufigkeiten
        chord_freq_dev1 <- table(oval_harmonic_tab$chord)
        
        # Umwandeln der Tabelle in einen Datenrahmen
        chord_freq_df_dev1 <- data.frame(chord = names(chord_freq_dev1), absolute_frequency = as.vector(chord_freq_dev1))
        
        # Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
        chord_freq_df_dev1 <- chord_freq_df_dev1[order(chord_freq_df_dev1$absolute_frequency, decreasing = TRUE), ]
        
        # Berechne relative Häufigkeiten
        chord_freq_df_dev1$relative_frequency <- chord_freq_df_dev1$absolute_frequency / sum(chord_freq_df_dev1$absolute_frequency)
        
        # Rang erstellen
        chord_freq_df_dev1$rank <- 1:nrow(chord_freq_df_dev1)
        

# (DEV2)
        
        # (A): source("Vers-B/4-Datenbereinigung/4a-combined-data.R")
        
        # Liste von Datenrahmen erstellen, um Daten zu speichern
        data_list <- list()
        
        # Schleife durch die ausgewählten .tsv-Dateien und Daten in der Liste speichern
        for (file in tsv_files_dev2) {
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
        
        
        # (B) Filter nach Tongeschlecht der Tonartabschnitte: 
        oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == tongeschlecht)
        
        
        # (C) 
        
        # Erstelle eine Tabelle mit den verschiedenen Akkorden und ihren absoluten Häufigkeiten
        chord_freq_dev2 <- table(oval_harmonic_tab$chord)
        
        # Umwandeln der Tabelle in einen Datenrahmen
        chord_freq_df_dev2 <- data.frame(chord = names(chord_freq_dev2), absolute_frequency = as.vector(chord_freq_dev2))
        
        # Sortiere den Datenrahmen nach absteigender absoluter Häufigkeit
        chord_freq_df_dev2 <- chord_freq_df_dev2[order(chord_freq_df_dev2$absolute_frequency, decreasing = TRUE), ]
        
        # Berechne relative Häufigkeiten
        chord_freq_df_dev2$relative_frequency <- chord_freq_df_dev2$absolute_frequency / sum(chord_freq_df_dev2$absolute_frequency)
        
        # Rang erstellen
        chord_freq_df_dev2$rank <- 1:nrow(chord_freq_df_dev2)
        
        
# (X) 
    # Liste aller Akkorde
    all_chords <- unique(c(chord_freq_df_base$chord, chord_freq_df_dev1$chord, chord_freq_df_dev2$chord))
    
    # Erstellen des Gesamtdataframes
    chord_freq_df <- data.frame(chord = all_chords)
        
    # Hinzufügen der Einträge aus den jeweiligen Dataframes
      
      # mit chord_freq_df_base
      chord_freq_df$absolute_frequency_base <- ifelse(chord_freq_df$chord %in% chord_freq_df_base$chord, chord_freq_df_base$absolute_frequency[match(chord_freq_df$chord, chord_freq_df_base$chord)], 0) + 1
      chord_freq_df$relative_frequency_base <- chord_freq_df$absolute_frequency_base / sum(chord_freq_df$absolute_frequency_base)
                                               #ifelse(chord_freq_df$chord %in% chord_freq_df_base$chord, chord_freq_df_base$relative_frequency[match(chord_freq_df$chord, chord_freq_df_base$chord)], 0)
      
      chord_freq_df$rank_base <- rank(-chord_freq_df$absolute_frequency_base, na.last = TRUE, ties.method = "first")
                                 #ifelse(chord_freq_df$chord %in% chord_freq_df_base$chord, chord_freq_df_base$rank[match(chord_freq_df$chord, chord_freq_df_base$chord)], 9999)

      # mit chord_freq_df_dev1
      chord_freq_df$absolute_frequency_dev1 <- ifelse(chord_freq_df$chord %in% chord_freq_df_dev1$chord, chord_freq_df_dev1$absolute_frequency[match(chord_freq_df$chord, chord_freq_df_dev1$chord)], 0) + 1
      chord_freq_df$relative_frequency_dev1 <- chord_freq_df$absolute_frequency_dev1 / sum(chord_freq_df$absolute_frequency_dev1)
                                               #ifelse(chord_freq_df$chord %in% chord_freq_df_dev1$chord, chord_freq_df_dev1$relative_frequency[match(chord_freq_df$chord, chord_freq_df_dev1$chord)], 0)
      
      chord_freq_df$rank_dev1 <- rank(-chord_freq_df$absolute_frequency_dev1, na.last = TRUE, ties.method = "first")
                                 #ifelse(chord_freq_df$chord %in% chord_freq_df_dev1$chord, chord_freq_df_dev1$rank[match(chord_freq_df$chord, chord_freq_df_dev1$chord)], NA)

      # mit chord_freq_df_dev2
      chord_freq_df$absolute_frequency_dev2 <- ifelse(chord_freq_df$chord %in% chord_freq_df_dev2$chord, chord_freq_df_dev2$absolute_frequency[match(chord_freq_df$chord, chord_freq_df_dev2$chord)], 0) + 1
      chord_freq_df$relative_frequency_dev2 <- chord_freq_df$absolute_frequency_dev2 / sum(chord_freq_df$absolute_frequency_dev2)
                                               #ifelse(chord_freq_df$chord %in% chord_freq_df_dev2$chord, chord_freq_df_dev2$relative_frequency[match(chord_freq_df$chord, chord_freq_df_dev2$chord)], 0)
      
      chord_freq_df$rank_dev2 <- rank(-chord_freq_df$absolute_frequency_dev2, na.last = TRUE, ties.method = "first")
                                 #ifelse(chord_freq_df$chord %in% chord_freq_df_dev2$chord, chord_freq_df_dev2$rank[match(chord_freq_df$chord, chord_freq_df_dev2$chord)], NA)
      
    # Xi und Umkehrfunkt.
    
    xi <- function(x) {
      return((log10(x) + 2.5 ) / 25)
    }
    
    xi_minus <- function(x) {
      return(10^(25*x - 2.5))
    }
      
      
    # Delta zwischen relative_frequency_base und relative_frequency_dev1 bzw. relative_frequency_dev2
    # und normalisiertes Delta
    chord_freq_df$delta_relative_frequency_dev1 <- chord_freq_df$relative_frequency_base - chord_freq_df$relative_frequency_dev1
    #chord_freq_df$delta_relative_frequency_dev1_norm <- chord_freq_df$delta_relative_frequency_dev1/chord_freq_df$relative_frequency_base
    chord_freq_df$delta_relative_frequency_dev1_xi_minus <- xi_minus(chord_freq_df$delta_relative_frequency_dev1)
    
    chord_freq_df$delta_relative_frequency_dev2 <- chord_freq_df$relative_frequency_base - chord_freq_df$relative_frequency_dev2
    #chord_freq_df$delta_relative_frequency_dev2_norm <- chord_freq_df$delta_relative_frequency_dev2/chord_freq_df$relative_frequency_base
    chord_freq_df$delta_relative_frequency_dev2_xi_minus <- xi_minus(chord_freq_df$delta_relative_frequency_dev2)
    
    # Bereinigung des Datenrahmen von chord = ""
    chord_freq_df <- subset(chord_freq_df, !is.na(chord) & chord != "")
    
    
# # (C2) Mandelbrot-Zipf-Kurve auf Parameter a,b,c optimieren an die Daten
#     
#     # Definition der Funktion
#     zipf_function <- function(r, a, b, c) {
#       a / ((b + r)^c)
#     }
#     
#     # Angenommene Startwerte für a, b und c
#     initial_params <- list(a = 1, b = 1, c = 1)
#     
#     # Führe die nichtlineare Regression durch
#     zipf_fit <- nls(chord_freq_df$relative_frequency ~ zipf_function(chord_freq_df$rank, a, b, c), 
#                     data = chord_freq_df, 
#                     start = initial_params)
#     
#     # Ergebnisse
#     summary(zipf_fit)
#     
#     # Generiere Werte für f(r) basierend auf den geschätzten Parametern
#     zipf_values <- zipf_function(seq(1, max(chord_freq_df$rank)), coef(zipf_fit)[["a"]], coef(zipf_fit)[["b"]], coef(zipf_fit)[["c"]])
#     # print(zipf_values)
#     
#     # Erstelle ein neues Datenrahmen mit den generierten Werten
#     zipf_curve_data <- data.frame(rank_df = seq(1, max(chord_freq_df$rank)), freq_df = zipf_values)
#     
#     # Anz Datenpunkte
#     # print(chord_freq_df[nrow(chord_freq_df), "rank"])
#     
#     # Berechne die beobachteten und geschätzten Werte
#     real_values <- chord_freq_df$relative_frequency
#     function_values <- fitted(zipf_fit)
#     
#     # Berechne Summe der Residuenquadrate (sqr) und Summe der Abweichungsquadrate (sqt)
#     sqr <- sum((real_values - function_values)^2)
#     
#     mean_real <- mean(real_values)
#     sqt <- sum((real_values - mean_real)^2)
#     
#     # Berechne das Bestimmtheitsmass (r2)
#     r_quad <- 1 - (sqr / sqt)
#     print(paste("R^2 =", r_quad))
    

# (D) Ausgabe Rang-Häufigkeits Diagramm
    
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
    
    # Bearbeitung der zweiten y-Achsen Skala
    library(scales)
    
    # Plot
    library(ggplot2)

    # test_df <- data.frame(
    #   a = seq(1, 100, length.out = 100),
    #   b = seq(0.00005, 0.185, length.out = 100),
    #   c = seq(-0.13, 0.82, length.out = 100)
    # )
    # 
    # test_df$d <- 10^(-2.5 + 6*(test_df$c))
    # 
    ggplot(chord_freq_df, aes(x = rank_base, y = relative_frequency_base)) +
      
      #TEST
      # geom_point(data = test_df, aes(x = a, y = c),
      #            shape = 1, size = 1, color = farbe1_light) +
      # geom_line(data = test_df, aes(x = a, y = c),
      #           linewidth = .5, color = farbe1) +
      # 
      # geom_point(data = test_df, aes(x = a, y = d),
      #            shape = 1, size = 1, color = farbe2_light) +
      # geom_line(data = test_df, aes(x = a, y = d),
      #           linewidth = .5, color = farbe2) +
      
      
      
      
      #DEV1
      geom_point(data = chord_freq_df, aes(x = rank_base, y = relative_frequency_dev1),
                 shape = 1.5, size = 1, color = farbe2_light) +
      geom_line(data = chord_freq_df, aes(x = rank_base, y = relative_frequency_dev1),
                linewidth = 1, color = farbe2) +
      
      #BASE
      geom_point(data = chord_freq_df, aes(x = rank_base, y = relative_frequency_base),
                 shape = 1.5, size = 1, color = farbe1_light) +
      geom_line(data = chord_freq_df, aes(x = rank_base, y = relative_frequency_base),
                linewidth = 1, color = farbe1) +
      
      #DELTA DEV1
      # geom_point(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev1),
      #            shape = 1, size = 1, color = mark1_light) +
      # geom_line(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev1),
      #           linewidth = .5, color = mark1) +
      
      #DELTA DEV1 XI MINUS
      #geom_point(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev1_xi_minus),
      #           shape = 1.5, size = 1, color = mark1_light) +
      geom_line(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev1_xi_minus),
                linewidth = 1, color = mark1) +
      
      #DEV2
      # geom_point(data = chord_freq_df, aes(x = rank_base, y = relative_frequency_dev2),
      #            shape = 1, size = 1, color = farbe3_light) +
      # geom_line(data = chord_freq_df, aes(x = rank_base, y = relative_frequency_dev2),
      #           linewidth = .5, color = farbe3) +
      
      #DELTA DEV2 XI MINUS
      # geom_point(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev2_xi_minus),
      #            shape = 1, size = 1, color = mark1_light) +
      # geom_line(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev2_xi_minus),
      #           linewidth = .5, color = mark1) +
      
      
      # # Erstelle Plot für Zipf Funktion
      # geom_line(data = zipf_curve_data, 
      #           aes(x = rank_df, y = freq_df),
      #           linewidth = 2,
      #           color = "#215CAF") +
      
      # Allg. Einstellungen
      scale_y_continuous(trans = "log10", limits = c(0.00005, 0.185)) +
      # , sec.axis = sec_axis(trans = ~ xi(.) )
      # (log10(~ .) + 2.5)/30
      # , sec.axis = sec_axis(trans = ~ log10(.), name ="Norm")
      scale_x_continuous(trans = "log10", limits = c(1, 1000)) +
      labs(x = "Häufigkeitsrang", y = "rel. Häufigkeit")+
          #title = "Rang-Häufigkeit (Dur)",
          #subtitle = "Vergleich aller LvB Klaviersonaten (base) mit seinen frühen (dev1) und mittleren (dev2)") +
      # annotate(geom="text", x=110, y=0.07, label=paste("R^2 =",round(r_quad, 4)),size=8) +
      theme(axis.text=element_text(size=22),
            axis.title=element_text(size=22),
            axis.line.x=element_line(color ="#4D4D4D"),
            axis.line.y=element_line(color ="#4D4D4D"),
            #axis.title.y = element_blank(),
            #axis.text.y = element_blank(),
            #plot.margin=margin(0, 18, 0, 0, "pt")
            )

    # ggplot(chord_freq_df, aes(x = rank, y = relative_frequency)) +
    #   
    #   #DELTA DEV1
    #   geom_point(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev1),
    #              shape = 1, size = 1, color = mark1_light) +
    #   geom_line(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev1),
    #             linewidth = .5, color = mark1) +
    #   
    #   #DELTA DEV2
    #   geom_point(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev2),
    #              shape = 1, size = 1, color = mark2_light) +
    #   geom_line(data = chord_freq_df, aes(x = rank_base, y = delta_relative_frequency_dev2),
    #             linewidth = .5, color = mark2) +
    #   
    #   # Allg. Einstellungen
    #   scale_y_continuous(limits = c(-0.1, 0.1)) +
    #   scale_x_continuous(trans = "log10", limits = c(1, 1000)) +
    #   # labs(x = "Häufigkeitsrang", y = "rel. Häufigkeit",
    #   #      title = "Rang-Häufigkeit (Dur)",
    #   #      subtitle = "Vergleich Ludwig van Beethovens Klaviersonaten (base) mit Wolfgang Amadeus Mozarts Klaviersonaten (dev1) und Ludwig van Beethovens Streicherquartetten (dev2)") +
    #   # annotate(geom="text", x=110, y=0.07, label=paste("R^2 =",round(r_quad, 4)),size=8) +
    #   theme(axis.text=element_text(size=22),
    #         axis.title=element_text(size=22),
    #         axis.line.x=element_line(color ="#4D4D4D"),
    #         axis.line.y=element_line(color ="#4D4D4D"),
    #         #plot.margin=margin(0, 18, 0, 0, "pt")
    #   )
    
    
    # Speichere das Diagramm als PNG-Datei
    ggsave("ana1c-UG-rank-freq_Vgl-LVBS-LVBQ-major-480dpi.png", plot = last_plot(), path = "Ergebnisse/In-Skript", width = 8, height = 5, dpi = 480)