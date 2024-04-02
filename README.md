# Rolling Over Beethoven with Statistics
Eine Vergleich statistischer Eigenschaften der tonalen Harmonie in Beethovens Klaviersonaten und Streichquartetten (Wettbewerbsarb. Schweizer Jugend forscht 2024)

> **DIESES PROJEKT WIRD GERADE ÜBERARBEITET. FÜR FRÜHERE VERSIONEN SIEHE RoB v1.0 UNTER VERSIONEN.**

## Zusammenfassung

Tonale Harmonie ist eine der zentralen Eigenschaften der klassischen westlichen Musik. In der bisherigen Musikforschung über tonale Harmonie wird meist ein qualitativer Ansatz gewählt, bei welchem allgemeine Aussagen oft nur von einer kleinen Zahl von Beispielen gestützt werden. Diese Arbeit wählt wie andere kürzlich verö entlichten Untersuchungen in dem Fachgebiet einen quantitativen Ansatz. Sie versucht die statistischen Eigenschaften der tonalen Harmonie in Beethovens Klaviersonaten und Streicherquartetten zu erfassen, untersuchen und vergleichen. Die Analysen werden an zwei kürzlich veröffentlichen Datensätzen durchgeführt, die von Hentschel, Johannes et al. bzw. Neuwirth, Markus et al. harmonisch annotiert wurden. Die angewendeten Methoden stützen sich auf eine Untersuchung von Moss, Fabian C. et al. zur statistischen Charakterisierung der tonalen Harmonie in Beethovens Streichquartetten.

Mithilfe von rechnergestützten Auswertungen wird in dieser Arbeit gezeigt, dass ( 1) tonale Harmonie in Beethovens Klaviersonaten und Streichquartetten grösstenteils durch wenige führende Akkorde bestimmt wird; (2 ) die Vorhersage von Akkordübergängen deutlich durch bestimmte Akkordmerkmale beeinflusst werden; ( 3) sich die Vorhersagbarkeit und Symmetrie von Übergängen in Beethovens Klaviersonaten von der in den Streichquartetten unterscheidet und; (4 ) die Vorhersagbarkeit von Übergängen in Beethovens frühen Sonaten und Quartetten kaum anders ist seinen späten Werken.

Die Ergebnisse fügen sich grösstenteils in die vorherrschende Lehre und bestätigen daher die Grundthesen der tonalen Harmonie. Sie ergänzen den Bereich der quantitativen Musikforschung, lassen jedoch o ffene Fragen bezüglich bestimmten Übergangsformen, die zwischen den Sonaten und Quartetten unterschiedlich sind. Diese müssen durch eine qualitative Untersuchung der Partitur analysiert werden.

## Durchführung der Analysen

Die Analysen der Daten wurden in der Programmiersprache R und der Entwicklungsumgebung RStudio auf einem MacBook Pro (    2014) durchgeführt. R ist auf statistische Auswertung spezialisiert und eignet sich gut für die Bearbeitung und Darstellung von Tabellen und Statistiken. Jegliche Diagramme und Grafiken wurden ebenfalls mit R und RStudios erstellt. Für die Analysen wurden ausschliesslich die Dateien der Harmonieannotationen aus dem Korpus von Neuwirth et al. und Hentschel et al. genutzt. Tabellen im TSV-Format wurden eingelesen und zu einem grossen Datenrahmen kombiniert, anhand welchem dann die Untersuchungen durchgeführt wurden.

Beim Einlesen mussten Sonderheiten in der Annotation, wie die Nutzung in R geschützter Sonderzeichen ('%' und '#') und der von R als logische Werte interpretierten Zeichen 'F' und 'R', beachtet werden. Die durchgeführten Analysen sind in vier Kategorien eingeteilt. In einem ersten Schritt wurde die Rang-Häufigkeit von Unigrammen untersucht. Die zweite Analyse umfasst die Häufigkeit von Akkordübergängen. In einer dritten Analyse wurde Vorhersagbarkeit von Akkordübergängen betrachtet und in der vierten und letzten wurde die Symmetrie von Übergängen untersucht. Bei den Analysen wurde stets auf die Reproduzierbarkeit der Ergebnisse geachtet. Daher ist der gesamte Programmecode inklusive detaillierter Beschreibungen hier auf GitHub verfügbar. **Hinweise zu Fehlern in den Programmen oder andere Irrtümer werden gerne angenommen.**


