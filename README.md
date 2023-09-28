# big-data-beethoven
Eine Untersuchung statistischer Eigenschaften der tonalen Harmonie in Beethovens Klaviersonaten (Maturaarb. 2023)

## Zusammenfassung

Tonale Harmonie ist eine der zentralen Eigenschaften der klassischen westlichen Musik. In der bisherigen Musikforschung über tonale Harmonie wird meist ein qualitativer Ansatz gewählt, bei welchem allgemeine Aussagen oft nur von einer kleinen Zahl von Beispielen gestützt werden. Diese Arbeit wählt wie andere kürzlich veröffentlichte Untersuchungen in dem Fachgebiet einen quantitativen Ansatz. Sie gibt zuerst eine Übersicht zu den musiktheoretischen Hintergründen und versucht anschliessend die statistischen Eigenschaften der tonalen Harmonie zu erfassen, untersuchen und beschreiben. Die Analysen werden an Beethovens 32 Klaviersonaten durchgeführt, die in einem kürzlich veröffentlichten Datensatzes von Hentschel, Johannes et al. harmonisch annotiert wurden. Die angewendeten Methoden stützen sich auf eine Untersuchung von Moss, Fabian C. et al. zur statistischen Charakterisierung der tonalen Harmonie in Beethovens Streichquartetten.

Mithilfe von rechnergestützter Auswertungen wird in dieser Arbeit gezeigt, dass 1. tonale Harmonie grösstenteils durch wenige führende Akkorde bestimmt wird; 2. Akkordübergänge sich an Bezugspunkten orientieren und die Vorhersage von Akkorden deutlich durch bestimmte Akkordmerkmale beeinflusst wird; und 3. tonale Harmonie zeitlich gerichtet ist und grundsätzlich authentische Übergänge bevorzugt.

Die Ergebnisse fügen sich grösstenteils in die vorherrschende Lehre und bestä- tigen daher die angenommenen Grundthesen der tonale Harmonie. Sie ergän- zen jedoch den Bereich der quantitativen Musikforschung. Arbeiten in diesem Bereich könnten in Zukunft hilfreich bei der Entwicklung von künstlicher In- telligenz in der Musik sein. Dazu sind jedoch Analysen noch umfassenderer Datensätze nötig, um eine lückenfreie, quantitativ belegbare Grundlage für daraus abgeleiteten Annahmen zu bilden.

## Durchführung der Analysen

Die Analysen der Daten wurden in der Programmiersprache R und der Ent- wicklungsumgebung RStudio auf einem MacBook Pro (2014) durchgeführt. R ist auf statistische Auswertung spezialisiert und eignet sich gut für die Bearbei- tung und Darstellung von Tabellen und Statistiken. Für die Analysen wurden ausschliesslich die Dateien der Harmonieannotationen aus dem Korpus von Hentschel et al. genutzt. (1) Tabellen im TSV-Format wurden wie folgt eingelesen und zu einem grossen Datenrahmen kombiniert, anhand welchem dann die Untersuchungen durchgeführt wurden.

Bei den Analysen wurde stets auf die Reproduzierbarkeit der Ergebnisse geachtet. Daher sind die gesamten Programme auf Github hochgeladen (2) worden und folgendermassen sturkturiert.

| Programm           | Beschreibung                                                                                                |
| ------------------ | ----------------------------------------------------------------------------------------------------------- |
| gen-ana            | Allgemeine Untersuchungen zur einfachen statisische Beschreibung des Datensets                              |
| harmonic-ana-2	   | Erste Versuche zur Erstellung des kombinierten Datenrahmens und eines einfachen Rang-Häufigkeits Diagramms  |
| harmonic-ana-3		 | Analysen zur Zentrierung (Rang-Häufigkeits-Verteilung, Optimierung Mandebrot-Zipf-Funktion)                 |
| harmonic-ana-4		 | Analysen zur Referenzialität (Häufigkeiten von Akkordübergängen, Heatmaps)                                  |
| harmonic-ana-5     | Analysen zur Referenzialität (Bedingte Entropie, Einflüsse von Merkmalen auf Vorhersagbarkeit)              |
| harmonic-ana-6     | Analysen zur Referenzialität (Bedingte Entropie, Einflüsse von Merkmalen auf Vorhersagbarkeit)              |

--------
(1) Hentschel, Johannes et al. _Ludwig van Beethoven Piano Sonatas (A corpus of
annotated scores)_. Version 2.0. Dez. 2022. doi: 10.5281/zenodo.7535598. 
Erhältlich unter https://github.com/DCMLab/beethoven_piano_sonatas/tree/v2.0

(2) Erhältlich unter https://github.com/rahulandrea/big-data-beethoven.git
