# Daten laden
oval_harmonic_tab <- read.table("21-1.tsv", header = TRUE, sep = "\t", fill = TRUE, comment.char = "")

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
             shape = 1, size = 1, color = "seagreen3") +
  # Erstelle Plot für Zipf Funktion
  geom_line(data = zipf_curve_data, aes(x = rank_df, y = freq_df),
            color = "seagreen4") +
  
  # Allg Einstellungen
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  #  scale_y_continuous(labels = scales::label_number()) +
  labs(x = "chords_rank", y = "rel_freq", 
       title = "rank vs frequency",
       subtitle = "Zipf function fitted parameters a, b, c with 'nls'.", r_squared) +
  theme_minimal()

# Speichere das Diagramm als PNG-Datei
# ggsave("rank_vs_freq_zipf-2400dpi.png", plot = last_plot(), width = 8, height = 8, dpi = 2400)
