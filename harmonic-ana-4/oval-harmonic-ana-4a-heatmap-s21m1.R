# Daten laden
oval_harmonic_tab <- read.table("21-1.tsv", header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
# oval_harmonic_tab <- read.table("combined_data-oval.tsv", header = TRUE, sep = "\t", fill = TRUE, comment.char = "")

# 25 häufigsten Akkorde ermitteln
top_chords <- head(names(sort(table(oval_harmonic_tab$chord), decreasing = TRUE)), 25)

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
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(hjust = 1, vjust = 1),
  ) +
  labs(title = "transition probab A to B sonata 21-1 (first waldstein mvt)",
       x = "B",
       y = "A")