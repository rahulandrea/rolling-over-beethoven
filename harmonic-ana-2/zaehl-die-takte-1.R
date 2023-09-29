#Zähle die Takte

# Liste der Dateinamen der TSV-Dateien erstellen
tsv_files <- list.files(pattern = "[0-9]{2}-[0-9].tsv")

# Erstellt Datenrahmen
data_list <- list()

# Schleife zur Extraktion
last_values <- c()  # Initialisiere einen leeren Vektor für die letzten Werte

for (file in tsv_files) {
  # Daten aus der Datei lesen
  data <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
  last_value <- tail(data$mn, 1)
  
  last_values <- c(last_values, last_value)  # Füge den letzten Wert zum Vektor hinzu
}

# Summe der letzten Werte berechnen
total_sum <- sum(last_values)

print(total_sum)