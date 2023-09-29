# Zähl wieviele Major u. minor chords
# Daten laden
oval_harmonic_tab <- read.table("combined_data-oval.tsv", header = TRUE, sep = "\t", fill = TRUE, comment.char = "")

# Anzahl der Einträge mit localkey_is_minor = 1 zählen
count_minor_1 <- sum(oval_harmonic_tab$localkey_is_minor == 1)

# Anzahl der Einträge mit localkey_is_minor = 0 zählen
count_minor_0 <- sum(oval_harmonic_tab$localkey_is_minor == 0)

# Ausgabe der Ergebnisse
cat("Anzahl der Einträge mit localkey_is_minor = 1:", count_minor_1, "\n")
cat("Anzahl der Einträge mit localkey_is_minor = 0:", count_minor_0, "\n")