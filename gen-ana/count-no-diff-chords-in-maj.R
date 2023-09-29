# Zählt Akkordtoken in einer .tsv Datei

# Liste der gewünschten .tsv-Dateien erstellen
tsv_files <- list.files(pattern = "[0-9]{2}-[0-9].tsv")

# Schleife durch die ausgewählten .tsv-Dateien und Daten in der Liste speichern
for (i in 1:length(tsv_files)) {
  file <- tsv_files[i]
  data <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
  chrds_ls <- list()
  ls_index <- 0
  for (j in 1:length(data$chord)){
    if (data$localkey_is_minor[i] == 0){
      ls_index <- ls_index + 1
      chrd <- data$chord[i]
      chrds_ls[[ls_index]] <- chrd
    }
  }
}

unique_chrds_ls <- unique(chrds_ls)

print("So viele unters. Akkorde in Dur:")
print(length(unique_chrds_ls))
