# Ferskvassfisk i Noreg

Dette repoet inneheld verktøy for å kartlegge ferskvassfisk i Noreg basert på observasjonsdata frå [GBIF](https://www.gbif.org/).

## Interaktivt kart (live)

Ein interaktiv versjon er tilgjengeleg som nettside her:
**https://andersfi.github.io/testmodus/**

På nettsida kan du søke etter artar på norsk eller latinsk namn (t.d. *ørret*, *gjedde*, *abbor*) og få vist observasjonar på kart direkte frå GBIF. Data vert henta i sanntid frå GBIF sitt API.

## Statiske kart

Repoet inneheld også R-skript for å lage statiske kart over artsobservasjonar:

- `fisk_norge.R` — genererer statiske kart med GBIF-observasjonar plotta over Noreg
- `norge.R` — hjelpeskript for kartdata

Dei statiske karta er laga med `ggplot2` og `maps` i R, og eignar seg for rapportar og publikasjonar.

## Shiny-app

`app.R` er ei interaktiv Shiny-applikasjon som gjer det same som nettsida, men krev ein R-server for å køyre. Ho kan startast lokalt med `shiny::runApp()` i R.
