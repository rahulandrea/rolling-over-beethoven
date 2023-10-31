# Dieses Prog. zählt die Grundtonfortschreitungen. Es wird das "Numeral" (Num.) 
# des aktuellen Akkord mit dem Num. des nächsten Akkord verglichen und damit das Intervall zw. den 
# Grundtönen berechnet. Dies für jeden Sonatensatz einzeln (daher die gr. Schleife, die durch alle 
# Sonaten (in "tsv_files" durchläuft))

setwd("~/Library/CloudStorage/OneDrive-SBL/32-Maturaarbeit/Analyse/harmonic-ana-6/data")

# Liste der gewünschten .tsv-Dateien erstellen
tsv_files <- list.files(pattern = "[0-9]{2}-[0-9].tsv")
# tsv_files <- "01-1.tsv"

# Zur Berrechung des Grundtonintervalls
num_val_df <- data.frame(
  num = c("I", "i", "II", "ii", "III", "iii", "IV", "iv", "V", "v", "VI", "vi", "VII", "vii"),
  val = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7)
)

# Leeres Dataframe wo Anz. der jeweiligen Grundtonfortschfreitungen eingetragen werden
gtf_df <- data.frame(
  gtf_krzl = c("A","B","C","D","E","F","G"),
  grundtonfortschreitung = c(1,2,3,4,5,6,7),
  freq_abs = c(0,0,0,0,0,0,0)  # Leere Spalte für freq_abs
)

for (file in tsv_files) {
  
  # Daten aus spezifischer Tabelle laden
  specific_harmonic_tab <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")
  
  # Übergänge zählen und in die Matrix eintragen
  for (i in 1:(length(specific_harmonic_tab$numeral) - 1)) {
    current_num <- specific_harmonic_tab$numeral[i]
    next_num <- specific_harmonic_tab$numeral[i + 1]
    
    # Prüfe, dass nicht mit "#" oder "b" beginnt
    if (substring(current_num, 1, 1) == "#") {
      current_num <- substring(current_num, 2)
    }
    if (substring(next_num, 1, 1) == "#"){
      next_num <- substring(next_num, 2)
    }
    
    if (substring(current_num, 1, 1) == "b") {
      current_num <- substring(current_num, 2)
    }
    if (substring(next_num, 1, 1) == "b"){
      next_num <- substring(next_num, 2)
    }
    
    # Erstes if prüft, dass weder current_num noch next_num leer sind
    # Error: subscript out of bounds
    if (current_num != "" && next_num != "") {
      mvt_val <- abs( num_val_df$val[num_val_df$num == current_num] - num_val_df$val[num_val_df$num == next_num] ) + 1

      if (num_val_df$val[num_val_df$num == current_num] > num_val_df$val[num_val_df$num == next_num]){
        mvt_val <- 9 - abs(mvt_val)
      }else{
        mvt_val <- abs(mvt_val)
      }
      
      gtf_df$freq_abs[gtf_df$grundtonfortschreitung == mvt_val] <- gtf_df$freq_abs[gtf_df$grundtonfortschreitung == mvt_val] + 1
    }
  }
}
 
# Relat. hinzufügen
gtf_df$freq_rel <- gtf_df$freq_abs / sum(gtf_df$freq_abs)

# Grafik erstellen mit ggplot2
library(ggplot2)

# Beispiel-Daten
data <- data.frame(
# Kategorie = c("1","5↓","5↑","2↑","3↓","2↓","3↑"),
# Werte = c(0.2997,0.2263,0.141,0.125,0.0853,0.0837,0.0435)
  Kategorie = c("1","2↑","3↑","5↓","5↑","3↓","2↓"),
  Wert = gtf_df$freq_rel
)

# Daten nach Wert sortieren
data <- data[order(data$Wert, decreasing = TRUE),]

# Farbpalette für Balken
Farben1 <- c("#A9A9A9","#627313","#8E6713","#8E6713","#627313","#8E6713","#627313")

# Balkendiagramm erstellen
ggplot(data, aes(x = reorder(Kategorie, -Wert), y = Wert, fill = Kategorie)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = Farben1) +  # Farbpalette festlegen
  labs(
    # title = paste("Alterationen \n µ_M =",round(A_entropy_mean, 4),"\n π_M =",round(p,4)),
    x = "Grundtonübergänge", 
    y = "rel. Häufigkeit") +
  theme(
    legend.position = "none",
    axis.line.x=element_line(color ="#4D4D4D"),
    axis.line.y=element_line(color ="#4D4D4D"),
    plot.title =element_text(size=22,hjust = 0.5),
    # axis.text.y = element_blank(),
    # axis.title.y = element_blank(),
    axis.text=element_text(size=12),
    axis.title=element_text(size=12),
    plot.margin=margin(10, 10, 5, 10, "pt") #t,r,b,l
  )

# Speichere das Diagramm als PNG-Datei
ggsave("grundton-uber-neu.png", plot = last_plot(), width = 8, height = 4, dpi = 480)
