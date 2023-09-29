# Daten laden
oval_harmonic_tab <- read.table("21-1.tsv", header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
# oval_harmonic_tab <- read.table("combined_data-oval.tsv", header = TRUE, sep = "\t", fill = TRUE, comment.char = "")

# chord_freq_df zur Ermittlung der Top 25 und rel_freq (Übernommen aus oval-harmonic-ana-2g)
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

# 25 häufigsten Akkorde ermitteln (aus dem chord_freq_df)
top_chords <- head(chord_freq_df$chord, 25)

print(top_chords)

# Matrix erstellen
transition_matrix <- matrix(0, nrow = length(top_chords), ncol = length(top_chords), dimnames = list(top_chords, top_chords))

# Übergänge zählen und in die Matrix eintragen
for (i in 1:(length(oval_harmonic_tab$chord) - 1)) {
  current_chord <- oval_harmonic_tab$chord[i]
  next_chord <- oval_harmonic_tab$chord[i + 1]
  
  if (current_chord %in% top_chords && next_chord %in% top_chords) {
    transition_matrix[current_chord, next_chord] <- transition_matrix[current_chord, next_chord] + 1
  }
}

# Umwandlung in Dataframe
transition_df <- as.data.frame(as.table(transition_matrix))

# Heatmap erstellen
library(ggplot2)
ggplot(data = transition_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "seagreen") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
    axis.text.y = element_text(hjust = 1, vjust = 1),
  ) +
  coord_fixed(ratio=1) +
  labs(title = "transition probab A to B sonata 21-1 (first waldstein mvt)",
       x = "B",
       y = "A")

# Speichere das Diagramm als PNG-Datei
# ggsave("transition_probab_s21m1-1200dpi.png", plot = last_plot(), width = 8, height = 6, dpi = 1200)

