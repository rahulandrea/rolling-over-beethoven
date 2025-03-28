# ana1-UG-Rang-Häufigkeit: ana1b-UG-rank-freq.R
# last review on 2024-01-27 by RAG

# (A-C) Dieses Prog. nutzt "ana1a-UG-rank-freq.R" um ein Dataframe zu erstellt, welches jedem Akkord (chord)
#       einen Häufigkeitsrang (rank) und die relative Häufigkeit (relative_frequency) zuordnet
#     (A) "4a-combined-data.R" um eine grosse .tsv Tabelle über alle Stücke und Sätze zu erhalten
#     (B) Es besteht die Möglichkeit, nach Tongeschlecht der Tonartabschnitten zu filtern
#     ..
# (C2)  Weiter ist es möglich die erhaltenen Werte im Dataframe mit der Mandelbrot-Zipf-Kurve zu vergleichen
# (D) Es ist möglich sich ein Diagramm ausgeben zu lassen

#setwd()

# Liste der zu untersuchenden .tsv-Dateien erstellen
# Dateien benamst nach [KOM]-[A][xx]-M[x].tsv 
#   wobei KOM für das Komponistenkürzel steht (LVB für L. v. Beethoven, WAM für W. A. Mozart),
#   A für die Stückart (S für Sonate, Q für Streicherquartett) mit der üblichen Nummerierung (xx aus 00-99),
#   M für den Satz mit Nummer (x aus 0-9)
# Für weiteres siehe README
# Falls nur einzelne gewünscht: c("Daten/WAM-S01-M1.tsv", "Daten/LVB-S01-M1.tsv", "Daten/LVB-S01-M2.tsv")

tsv_files <- list.files(path = "Daten", pattern = "[A-Z]{3}-S[0-9]{2}-M[0-9].tsv")

# Zu untersuchendes Tongeschlecht festlegen (damit beim Auslesen von base und dev1 das selbe Tonggeschlecht 
# rausgefiltert wird): Siehe dazu (BASE)/(B), (DEV1)/(B) bzw. (DEV2)/(B) 
# Für Dur: tongeschlecht <- 0, für Moll: tongeschlecht <- 1
tongeschlecht <- 1


# (A): source("Vers-B/4-Datenbereinigung/4a-combined-data.R")

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


# (B) Filter nach Tongeschlecht der Tonartabschnitte: 
#   Für Dur: "globalkey_is_minor" == 0, für Moll: "globalkey_is_minor" == 1
    oval_harmonic_tab <- subset(final_combined_data, localkey_is_minor == tongeschlecht)


# (C) 
    
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

    
# (C2) Mandelbrot-Zipf-Kurve auf Parameter a,b,c optimieren an die Daten
    
    # Definition der Funktion
    zipf_function <- function(r, a, b, c) {
      a / ((b + r)^c)
    }
    
    # Angenommene Startwerte für a, b und c
    initial_params <- list(a = 1, b = 1, c = 1)
    
    # Führe die nichtlineare Regression durch
    zipf_fit <- nls(chord_freq_df$relative_frequency ~ zipf_function(chord_freq_df$rank, a, b, c), 
                    data = chord_freq_df, 
                    start = initial_params)
    
    # Ergebnisse
    summary(zipf_fit)
    
    # Generiere Werte für f(r) basierend auf den geschätzten Parametern
    zipf_values <- zipf_function(seq(1, max(chord_freq_df$rank)), coef(zipf_fit)[["a"]], coef(zipf_fit)[["b"]], coef(zipf_fit)[["c"]])
    # print(zipf_values)
    
    # Erstelle ein neues Datenrahmen mit den generierten Werten
    zipf_curve_data <- data.frame(rank_df = seq(1, max(chord_freq_df$rank)), freq_df = zipf_values)
    
    # Anz Datenpunkte
    # print(chord_freq_df[nrow(chord_freq_df), "rank"])
    
    # Berechne die beobachteten und geschätzten Werte
    real_values <- chord_freq_df$relative_frequency
    function_values <- fitted(zipf_fit)
    
    # Berechne Summe der Residuenquadrate (sqr) und Summe der Abweichungsquadrate (sqt)
    sqr <- sum((real_values - function_values)^2)
    
    mean_real <- mean(real_values)
    sqt <- sum((real_values - mean_real)^2)
    
    # Berechne das Bestimmtheitsmass (r2)
    r_quad <- 1 - (sqr / sqt)
    print(paste("R^2 =", r_quad))
    
    
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
    
    # Plot
    library(ggplot2)
    ggplot(chord_freq_df, aes(x = rank, y = relative_frequency)) +
      # Erstelle ein gestapeltes Punktdiagramm (Dot Plot)
      geom_point(data = chord_freq_df, aes(x = rank, y = relative_frequency),
                 shape = 1, size = 2.5, color = farbe1_light) +
      
      # Erstelle Plot für Zipf Funktion
      geom_line(data = zipf_curve_data, 
                aes(x = rank_df, y = freq_df),
                linewidth = 2,
                color = farbe1) +
      
      # Allg. Einstellungen
      scale_y_continuous(trans = "log10", limits =c(0.00005, 0.18)) +
      scale_x_continuous(trans = "log10", limits = c(1, 1000)) +
      labs(x = "Häufigkeitsrang", y = "rel. Häufigkeit") +
      #    title = "rank vs frequency (major chords)",
      #    subtitle = "Zipf function fitted parameters a, b, c with 'nls'.") +
      annotate(geom="text", x=110, y=0.07, label=paste("R^2 =",round(r_quad, 4)),size=8) +
      theme(axis.text=element_text(size=22),
            axis.title=element_text(size=22),
            axis.line.x=element_line(color ="#4D4D4D"),
            axis.line.y=element_line(color ="#4D4D4D"),
            #axis.title.y = element_blank(),
            #axis.text.y = element_blank(),
            plot.margin=margin(5, 18, 5, 5, "pt")) #t,r,b,l

    # Speichere das Diagramm als PNG-Datei
    ggsave("ana1b-UG-rank-freq-LVBS_minor-480dpi.png", plot = last_plot(), path = "Ergebnisse", width = 8, height = 5, dpi = 480)
