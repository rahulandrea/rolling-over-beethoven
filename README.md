# big-data-beethoven
Eine Untersuchung statistischer Eigenschaften der tonalen Harmonie in Beethovens Klaviersonaten (Maturaarb. 2023)

## Durchführung der Analysen

Die Analysen der Daten wurden mit in der Programmiersprache R durchgeführt. 
R eignete gut für die Bearbeitung und Darstellung von Tabellen uns Statistiken. 
Für die Analysen wurden ausschliesslich die Dateien der Harmoniannotationen aus 
dem Korpus von Hentschel et al. genutzt (1)

Bei den Analysen wurde stets auf die Reproduzierbarkeit der Ergebnisse geachtet. 
Daher sind die gesamten Programme auf Github hochgeladen (2) worden und folgendermassen 
sturkturiert.

| Programm           | Beschreibung                                                                                                |
| ------------------ | ----------------------------------------------------------------------------------------------------------- |
| gen-ana            | Allgemeine Untersuchungen zur einfachen statisische Beschreibung des Datensets                              |
| harmonic-ana-2	   | Erste Versuche zur Erstellung des kombinierten Datenrahmens und eines einfachen Rang-Häufigkeits Diagramms  |
| harmonic-ana-3		 | Analysen zur Zentrierung (Rang-Häufigkeits-Verteilung, Optimierung Mandebrot-Zipf-Funktion)                 |
| harmonic-ana-4		 | Analysen zur Referenzialität (Häufigkeiten von Akkordübergängen, Heatmaps)                                  |
| harmonic-ana-5     | Analysen zur Referenzialität (Bedingte Entropie, Einflüsse von Merkmalen auf Vorhersagbarkeit)              |

--------
(1) Hentschel, Johannes et al. _Ludwig van Beethoven Piano Sonatas (A corpus of
annotated scores)_. Version 1.1. Dez. 2022. doi: 10.5281/zenodo.7535598. 
Erhältlich unter https://github.com/DCMLab/beethoven_piano_sonatas/tree/v2.0

(2) Erhältlich unter https://github.com/rahulandrea/big-data-beethoven.git
