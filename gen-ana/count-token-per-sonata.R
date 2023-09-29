# Zählt Akkordtoken in einer .tsv Datei

# Liste der gewünschten .tsv-Dateien erstellen
tsv_files <- list.files(pattern = "[0-9]{2}-[0-9].tsv")

# Liste von Datenrahmen erstellen, um Daten zu speichern
token_table_mvt <- data.frame(sonata = tsv_files, token = numeric(length(tsv_files)))

# Schleife durch die ausgewählten .tsv-Dateien und Daten in der Liste speichern
for (i in 1:length(tsv_files)) {
  file <- tsv_files[i]
  data <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
  how_many_token <- nrow(data)
  token_table_mvt[i, "token"] <- how_many_token
}

print(token_table_mvt)

# Lade die dplyr-Bibliothek
library(dplyr)

# Erstelle eine neue Tabelle mit den aggregierten Werten
token_table_sonatas <- token_table_mvt %>%
  group_by(prefix = substr(sonata, 1, 2)) %>%
  summarize(total_token = sum(token))

# Ausgabe der aggregierten Tabelle
print(token_table_sonatas)
