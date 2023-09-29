#setwd("Users/rahul/Library/CloudStorage/OneDrive-SBL/32-Maturarbeit/Analyse/general-anas")

# Liste der .tsv-Dateien
tsv_files <- list.files(pattern = "[0-9]{2}-[0-9].tsv", full.names = TRUE)

# Initialisiere ein leeres Dataframe, um die Werte zu speichern
#result_df <- data.frame(globalkey = character(0), globalkey_is_minor = integer(0))

in_maj <- 0
in_min <- 0


# Durchlaufe jede .tsv-Datei
for (file in tsv_files) {
  # Lese die erste Zeile der .tsv-Datei
  first_row <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
  # Extrahiere den Wert der Spalte "globalkey"
  #globalkey_value <- first_row$globalkey[1]
  # Extrahiere den Wert der Spalte "globalkey_is_minor"
  globalkey_is_minor_value <- first_row$globalkey_is_minor[1]
  
  if (globalkey_is_minor_value == 0){
    in_maj <- in_maj + 1
  }
  
  else{
    in_min <- in_min + 1
  }
  # Füge die Werte in das Ergebnis-Dataframe ein
  #result_df <- rbind(result_df, data.frame(globalkey = globalkey_value, globalkey_is_minor = globalkey_is_minor_value))
}

# Zeige das Ergebnis-Dataframe
#print(result_df)

#in_major <- sum(result_df$globalkey_is_minor == 0)
#in_minor <- sum(result_df$globalkey_is_minor == 1)

print("In Dur:")
print(in_maj)
print("In Moll:")
print(in_min)

