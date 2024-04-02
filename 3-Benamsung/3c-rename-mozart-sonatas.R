# 3-Benamsung: 3a-rename-beethoven-quartets.R
# last review on 2024-02-25 by RAG

# Dieses Programm wandelt die Dateinamen der .tsv Tabellen aus dem Datensatz von
#   Hentschel et al. im Unterordner mozart_piano_sonatas/harmonies in das Format um,
#   das nötig für das Einlesen in den Skripten dieser Arbeit ist. 
# (1) Zuerst werden die alle relevanten Dateien aufgelistet.
# (2) Dann werden die Funktionen definiert, welche die relevanten Daten aus dem alten Dateinamen extrahieren.
# (3) Zuletzt werden die neuen Namen konsturiert und die Dateien umbenannt.

# !! Dateien müssen im Arbeitsvereichnis unter ~/Daten liegen
tsv_files <- list.files(path = "Daten", pattern = "K[0-9]{3}-[0-9].harmonies.tsv")
print(tsv_files)

# (1)
  # Liste von Datenrahmen erstellen, um Daten zu speichern
  data_list <- list()


# (2)
  # Funktionen zum extrahieren der Stück- und Satznummer
  extract_no <- function(file_name) {
    extracted_no <- gsub("K([0-9]+)-[0-9]+\\.harmonies\\.tsv", "\\1", file_name)
    return(extracted_no)
  }
  
  extract_mvt <- function(file_name) {
    extracted_mvt <- gsub("K[0-9]+-([0-9]+)\\.harmonies\\.tsv", "\\1", file_name)
    return(as.numeric(extracted_mvt))
  }
  
  # (2b)
    # Zuordnung Nummer aus dem Köchelverzeichnis mit Stücknummer der Sonate
    zuordnung_wam_s <- data.frame(
      köchel = c("279", "280", "281", "282", "283", "284", "309", "310", "311", "330", "331", "332", "333", "457", "533", "545", "570", "576"),
      stück = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18")
    )
  
# (3)
  # Schleife für alle Dateien in Liste
  for (file in tsv_files) {
    no <- zuordnung_wam_s$stück[zuordnung_wam_s$köchel == extract_no(file)]
    mvt <- extract_mvt(file)
    
  # Neuer Dateiname zusammenstellen
  new_name <- paste0("WAM-S", no, "-M", mvt, ".tsv")
  
  # Pfade zu altem und neuem Dateinamen
  old_path <- file.path("Daten", file)
  new_path <- file.path("Daten", new_name)
  
  # Umbenennung der Datei (Ersetzten der Pfade)
  file.rename(old_path, new_path)
  }