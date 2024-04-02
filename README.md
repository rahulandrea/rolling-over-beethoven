# Rolling Over Beethoven with Statistics
Eine Vergleich statistischer Eigenschaften der tonalen Harmonie in Beethovens Klaviersonaten und Streichquartetten (Wettbewerbsarb. Schweizer Jugend forscht, 2024)

## Zusammenfassung

Tonale Harmonie ist eine der zentralen Eigenschaften der klassischen westlichen Musik. In der bisherigen Musikforschung über tonale Harmonie wird meist ein quatitativer Ansatz gewählt, bei welchem allgemeine Aussagen oft nur von einer kleinen Zahl von Beispielen gestützt werden. Diese Arbeit wählt wie andere kürzlich veröffentlichte Untersuchungen in diesem Fachgebiet einen quantitativen Ansatz. Sie versucht die statistischen Eigenschaften der tonalen Harmonie in Beethovens Klaviersonaten und Streichquartetten zu erfassen, untersuchen und vergleichen. Die Analysen werden an zwei kürzlich veröffentlichten Datensätzen durchgeführt, die von Hentschel, Johannes et al.[^1] bzw. Neuwirth, Markus et al.[^2] harmonisch annotiert wurden. Die angewendeten Methoden stützen sich auf eine Untersuchung von Moss, Fabian C. et al.[^3] zur statistischen Charakterisierung der tonalen Harmonie in Beethovens Streichquartetten.

Mithilfe rechnergestützter Auswertungen wird in dieser Arbeit gezeigt, dass (1) tonale Harmonie in Beethovens Klaviersonaten und Streichquartetten grösstenteils durch dieselben wenigen führenden Akkorde bestimmt wird; (2) die Vorhersage von Akkordübergängen deutlich durch bestimmte Akkordmerkmale der Ausgangsakkorde beeinflusst wird; (3) sich die Vorhersagbarkeit und Symmetrie von Übergängen in Beethovens Klaviersonaten von der in Streichquartetten unterscheidet; und (4) zwischen den frühen und späten Werken (sowohl Klaviersonaten als auch Streichquartette) sich kaum Unterschiede dembezüglich feststellen lassen.

Die Ergebnisse fügen sich grösstenteils in die vorherschende Lehre und bestätigen daher die angenommenen Grundthesen der tonalen Harmonie. Sie ergänzen den Bereich der quantitativen Musikforschung, lassen jedoch offene Fragen bezüglich bestimmter Übergangsformen, die zwischen den Klaviersonaten und Streichquartetten unterschiedlich sind. Diese müssen durch eine qualitative Untersuchung der Partitur analysiert werden.

## Durcführung der Analysen

Die Analysen wurden in der Programmiersprache R und der Entwicklungsumgebung RStudios auf einem MacBookP Pro (2014) durchgeführt. R ist auf die statistische Auswertung spezialisiert und eignet sich gut für die Bearbeitung und Darstellung von Tabellen und Statistiken. Für die Analysen wurden ausschliesslich die Dateien der Harmonieannotationen aus dem Korpus von Neuwirth et al. und Hentschel et al. genutzt. Tabellen im TSV-Format wurden eingelesen und zu einem grossen Datenrahmen kombiniert, anhand welchem dann die Untersuchungen durchgeführt wurden.

Beim Einlesen mussten Sonderheiten in der Annotation, wie die Nutzung in R geschützter Sonderzeichen ('%' und '#') und der von R als logische Werte interpretierten Zeichen 'F' und 'R', beachtet werden. Die durchgeführten Analysen sind in vier Kategorien eingeteilt. In einem ersten Schritt wurde die Rang-Häufigkeit von Unigrammen untersucht. Die zweite Analyse umfasst die Häufigkeit von Akkordübergängen. In einer dritten Analyse wurde Vorhersagbarkeit von Akkordübergängen betrachtet und in der vierten und letzten wurde die Symmetrie von Übergängen untersucht. Bei den Analysen wurde stets auf die Reproduzierbarkeit der Ergebnisse geachtet. Daher ist der gesamte Programmcode inklusive detaillierter Beschreibungen hier auf GitHub verfügbar. **Hinweise zu Fehlern in den Programmen oder andere Irrtümer werden gerne angenommen.**
