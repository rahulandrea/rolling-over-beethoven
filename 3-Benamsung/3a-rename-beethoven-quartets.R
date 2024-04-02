# 3-Benamsung: 3a-rename-beethoven-quartets.R
# last review on 2024-01-26 by RAG

# Dieses Programm wandelt die Dateinamen der .tsv Tabellen aus dem Datensatz von
#   Neuwirth et al. im Unterordner ABC/harmonies/ in das Format um, das nötig für 
#   das Einlesen in den Skripten dieser Arbeit ist. 
# (1) Zuerst werden die alle relevanten Dateien aufgelistet.
# (2) Dann werden die Funktionen definiert, welche die relevanten Daten aus dem alten Dateinamen extrahieren.
# (3) Zuletzt werden die neuen Namen konsturiert und die Dateien umbenannt.

# !! Dateien müssen im Arbeitsvereichnis unter ~/Daten liegen
tsv_files <- list.files(path = "Daten", pattern = "n[0-9]{2}op[0-9]{2}-[0-9]_[0-9]{2}.harmonies.tsv|n[0-9]{2}op[0-9]{2}_[0-9]{2}.harmonies.tsv|n[0-9]{2}op[0-9]{3}_[0-9]{2}.harmonies.tsv")
print(tsv_files)

# (1)
  # Liste von Datenrahmen erstellen, um Daten zu speichern
  data_list <- list()


# (2)
  # Funktionen zum extrahieren der Stück- und Satznummer
  extract_no <- function(file_name) {
    extracted_no <- gsub("n([0-9]+)op[0-9]+.*", "\\1", file_name)
  }
  
  extract_mvt <- function(file_name) {
    extracted_mvt <- gsub(".*_([0-9]+)\\.harmonies\\.tsv$", "\\1", file_name)
    return(as.numeric(extracted_mvt))
  }
  
  
# (3)
  # Schleife für alle Dateien in Liste
  for (file in tsv_files) {
    no <- extract_no(file)
    mvt <- extract_mvt(file)
  
  # Neuer Dateiname zusammenstellen
  new_name <- paste0("LVB-Q", no, "-M", mvt, ".tsv")
  
  # Pfade zu altem und neuem Dateinamen
  old_path <- file.path("Daten", file)
  new_path <- file.path("Daten", new_name)
  
  # Umbenennung der Datei (Ersetzten der Pfade)
  file.rename(old_path, new_path)
  }