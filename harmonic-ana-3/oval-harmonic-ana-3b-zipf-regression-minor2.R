# (1) Kombinierter Datenrahmen
# kopiert aus oval-harmonic-ana-2g.R

setwd("/Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturaarbeit/Analyse/harmonic-ana-3/data")

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
oval_harmonic_tab <- subset(oval_harmonic_tab, localkey_is_minor == 1)

# Daten dirket aus bestimmter Tabelle laden
# oval_harmonic_tab <- read.table("21-1.tsv", header = TRUE, sep = "\t", fill = TRUE, comment.char = "")

# chord_freq_df (Übernommen aus oval-harmonic-ana-4c)
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
# Zeilen löscehn, in denen die Spalte "chord" leer ist
# chord_freq_df <- subset(chord_freq_df, !chord == "")

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

# Bestimmtheitsmass r2 berechnen
# Berechne die beobachteten und geschätzten Werte
real_values <- chord_freq_df$relative_frequency
function_values <- fitted(zipf_fit)

# Berechne Summe der Residuenquadrate (sqr) und Summe der Abweichungsquadrate (sqt)
sqr <- sum((real_values - function_values)^2)

mean_real <- mean(real_values)
sqt <- sum((real_values - mean_real)^2)

# Berechne das Bestimmtheitsmaß (R^2)
r_squared <- 1 - (sqr / sqt)
print(paste("R^2 =", r_squared))

# Plots
library(ggplot2)
ggplot(chord_freq_df, aes(x = rank, y = relative_frequency)) +
  # Erstelle ein gestapeltes Punktdiagramm (Dot Plot)
  geom_point(data = chord_freq_df, aes(x = rank, y = relative_frequency),
             shape = 1, size = 2.5, color = "#D48681") +
  # Erstelle Plot für Zipf Funktion
  geom_line(data = zipf_curve_data, aes(x = rank_df, y = freq_df),
            linewidth = 2,
            color = "#B7352D") +
  
  # Allg Einstellungen
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", limits = c(1, 1000)) +
  #  scale_y_continuous(labels = scales::label_number()) +
  labs(x = "Häufigkeitsrang", y = "rel. Häufigkeit") + 
  #    title = "rank vs frequency (major chords)",
  #    subtitle = "Zipf function fitted parameters a, b, c with 'nls'.", r_squared) +
  annotate(geom="text", x=110, y=0.07, label=paste("R^2 =",round(r_squared, 4)),size=8) +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22),
        axis.line.x=element_line(color ="#4D4D4D"),
        axis.line.y=element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin=margin(0, 18, 0, 0, "pt"))
  #theme_minimal()

# Speichere das Diagramm als PNG-Datei
ggsave("rank_vs_freq_zipf_minor-480dpi-angep-2.png", plot = last_plot(), width = 8, height = 5, dpi = 480)
