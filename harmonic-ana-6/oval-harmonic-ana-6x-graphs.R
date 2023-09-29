# Graph zu 5x
# an die Gestalltung meiner Arbeit angepasst

library(ggplot2)

# Datenimport
file <- "sym-2000-maj.tsv"
sp_df <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "")

# Intervalle festlegen
intervalle <- seq(0, 1, by = 0.005)

u <- sum(sp_df$mittlere_symmetrie == 1, na.rm = T)
p <- u/2000
# Diagramm erstellen
ggplot(data.frame(x = sp_df), aes(x = mittlere_symmetrie)) +
  # Dartsellung von (B)
  geom_histogram(
    aes(y = after_stat(count)),
    breaks = intervalle,
    fill = "#7A9DCF") +
  # Darstellung von (A)
  geom_vline(
    xintercept = 1,
    color = "#215CAF",
    linetype = "solid",
    linewidth = 2) +
  # Allgemeines
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
  labs(
  # title = paste("Alterationen \n µ_M =",round(A_entropy_mean, 4),"\n π_M =",round(p,4)),
    x = "Bigramm-Symmetrien", 
    y = "Stichproben") +
  #theme_minimal()
  annotate(geom="text", x=0.5, y=150, label=paste("sym(g) = 0.1409 \n π_g = 0"),size=10) +
  theme(
    axis.line.x=element_line(color ="#4D4D4D"),
    plot.title =element_text(size=22,hjust = 0.5),
  # axis.text.y = element_blank(),
  # axis.title.y = element_blank(),
    axis.text=element_text(size=22),
    axis.title=element_text(size=22),
    plot.margin=margin(10, 10, 5, 10, "pt") #t,r,b,l
  )

# Speichere das Diagramm als PNG-Datei
ggsave("harmonic-ana-6-maj.png", plot = last_plot(), width = 7, height = 5, dpi = 480)
