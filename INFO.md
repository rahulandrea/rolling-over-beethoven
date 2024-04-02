# Rolling Over Beethoven with Statistics
Eine Vergleich statistischer Eigenschaften der tonalen Harmonie in Beethovens Klaviersonaten und Streichquartetten (Wettbewerbsarb. Schweizer Jugend forscht, 2024)

> [!NOTE]
> Für Infos zu den Skripten siehe README.pdf oder die jeweiligen Beschreibungen der Analysen. (Es folgt noch ein README im Markdown-Format)

## Zusammenfassung

Tonale Harmonie ist eine der zentralen Eigenschaften der klassischen westlichen Musik. In der bisherigen Musikforschung über tonale Harmonie wird meist ein quatitativer Ansatz gewählt, bei welchem allgemeine Aussagen oft nur von einer kleinen Zahl von Beispielen gestützt werden. Diese Arbeit wählt wie andere kürzlich veröffentlichte Untersuchungen in diesem Fachgebiet einen quantitativen Ansatz. Sie versucht die statistischen Eigenschaften der tonalen Harmonie in Beethovens Klaviersonaten und Streichquartetten zu erfassen, untersuchen und vergleichen. Die Analysen werden an zwei kürzlich veröffentlichten Datensätzen durchgeführt, die von Hentschel, Johannes et al.[^1] bzw. Neuwirth, Markus et al.[^2] harmonisch annotiert wurden. Die angewendeten Methoden stützen sich auf eine Untersuchung von Moss, Fabian C. et al.[^3] zur statistischen Charakterisierung der tonalen Harmonie in Beethovens Streichquartetten.

Mithilfe rechnergestützter Auswertungen wird in dieser Arbeit gezeigt, dass (1) tonale Harmonie in Beethovens Klaviersonaten und Streichquartetten grösstenteils durch dieselben wenigen führenden Akkorde bestimmt wird; (2) die Vorhersage von Akkordübergängen deutlich durch bestimmte Akkordmerkmale der Ausgangsakkorde beeinflusst wird; (3) sich die Vorhersagbarkeit und Symmetrie von Übergängen in Beethovens Klaviersonaten von der in Streichquartetten unterscheidet; und (4) zwischen den frühen und späten Werken (sowohl Klaviersonaten als auch Streichquartette) sich kaum Unterschiede dembezüglich feststellen lassen.

Die Ergebnisse fügen sich grösstenteils in die vorherschende Lehre und bestätigen daher die angenommenen Grundthesen der tonalen Harmonie. Sie ergänzen den Bereich der quantitativen Musikforschung, lassen jedoch offene Fragen bezüglich bestimmter Übergangsformen, die zwischen den Klaviersonaten und Streichquartetten unterschiedlich sind. Diese müssen durch eine qualitative Untersuchung der Partitur analysiert werden.

## Durcführung der Analysen

Die Analysen wurden in der Programmiersprache R und der Entwicklungsumgebung RStudios auf einem MacBookP Pro (2014) durchgeführt. R ist auf die statistische Auswertung spezialisiert und eignet sich gut für die Bearbeitung und Darstellung von Tabellen und Statistiken. Für die Analysen wurden ausschliesslich die Dateien der Harmonieannotationen aus dem Korpus von Neuwirth et al. und Hentschel et al. genutzt. Tabellen im TSV-Format wurden eingelesen und zu einem grossen Datenrahmen kombiniert, anhand welchem dann die Untersuchungen durchgeführt wurden.

Beim Einlesen mussten Sonderheiten in der Annotation, wie die Nutzung in R geschützter Sonderzeichen ('%' und '#') und der von R als logische Werte interpretierten Zeichen 'F' und 'R', beachtet werden. Die durchgeführten Analysen sind in vier Kategorien eingeteilt. In einem ersten Schritt wurde die Rang-Häufigkeit von Unigrammen untersucht. Die zweite Analyse umfasst die Häufigkeit von Akkordübergängen. In einer dritten Analyse wurde Vorhersagbarkeit von Akkordübergängen betrachtet und in der vierten und letzten wurde die Symmetrie von Übergängen untersucht. Bei den Analysen wurde stets auf die Reproduzierbarkeit der Ergebnisse geachtet. Daher ist der gesamte Programmcode inklusive detaillierter Beschreibungen hier auf GitHub verfügbar. **Hinweise zu Fehlern in den Programmen oder andere Irrtümer werden gerne angenommen.**

Die Analysen in dieser Ablage sind wie folgt gegliedert:

| Name                     | Analyseart | Beschreibung |
|------|------------|--------------|
|3-Benamsung               |-           |Umbenennung der Dateien aus den Datensätzen von Neuwirth et al. und Hentschel et al. damit diese von den Programmen dieser Arbeit eingelesen werden können. Diese Programme müssen zu Anfang einmal ausgeführt werden.|
|4-Datenbereinigung        |-           |Bereinigung der Tabellen aus den Datensätzen von Neuwirth et al. und Hentschel et al. bzgl. Format und Inhalt damit diese von den Programmen dieser Arbeit eingelesen werden können. Diese Programme sind jeweils – sofern nötig – in die anderen integriert.|
|ana1-UG-Rang-Häufigkeit   |Unigramm    |(a) Untersuchung der Rang-Häufigkeit; (b) Vergleich der R-H mit Mandelbrot-Zipf-Kurve; und (c) Vergleich der R-H verschiedener Datensätze|
|ana2-BG-Übergänge         |Bigramm     |(a) Häufigkeit der Übergänge zwischen den 25 häufigsten Akkorden in einem Datensatz; und (b) Vergleich zweier Datensätze|
|ana3-BG-Entropie          |Bigramm     |(a) Vgl. der Entropie einer Akkordstichprobe aus allen Akkorden mit Akkorden mit bestimmter Eigenschaft (aus demselben Datensatz); (b) Vgl. der Entropie von Akkordstichproben eines Datensatzes mit Akkorden eines anderen, darunter (b1) Normal, Differenzierung aller Akkordtoken (wie in ana3a); und (b2) Reduktion auf Stufen (I, II, ..)|
|ana4-BG-Symmetrie         |Bigramm     |Untersuchung der Symmetrie von Akkordübergängen|





[^1]: Hentschel, Johannes et al. _Ludwig van Beethoven Piano Sonatas (A corpus of annotated scores)_. Vers. 1.1. Dez. 2022. 10.5281/zenodo.7535598
[^2]: Neuwirth, Markus et al. _The Annotated Beethoven Corpus (ABC): A Dataset of Harmonic Analyses of All Beethoven String Quartets_. In Front. Dig. Humanit. 5.16. Jul. 2018. 10.3389/fdigh.2018.00016
[^3]: Moss, Fabian C. et al. _Statistical characteristics of tonal harmony: A corpus study of Beethoven's string quartets_. In PLOS ONE 14.6 Jun. 2019. 10.1371/journal.pone.0217242
