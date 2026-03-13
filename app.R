library(shiny)
library(ggplot2)
library(maps)
library(jsonlite)

# --- Kartdata (lasta ein gong) ---
norge_kart <- tryCatch(
  map_data("worldHires", region = "Norway"),
  error = function(e) map_data("world", region = "Norway")
)

# --- Hjelpefunksjonar ---
sok_art <- function(query) {
  url <- paste0("https://api.gbif.org/v1/species/suggest?q=",
                URLencode(query), "&limit=8&rank=SPECIES")
  res <- tryCatch(fromJSON(url, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0) return(data.frame())
  res[, intersect(c("key", "scientificName", "vernacularName", "kingdom"), names(res))]
}

hent_gbif <- function(taxon_key, ar_fra = 2025) {
  base <- paste0(
    "https://api.gbif.org/v1/occurrence/search",
    "?taxonKey=", taxon_key,
    "&country=NO&year=", ar_fra, ",2026",
    "&hasCoordinate=true&limit=300"
  )
  first <- tryCatch(fromJSON(paste0(base, "&offset=0")), error = function(e) NULL)
  if (is.null(first)) return(data.frame())
  total <- first$count
  res   <- first$results[, c("decimalLongitude", "decimalLatitude"), drop = FALSE]

  if (total > 300) {
    for (off in seq(300, min(total, 3000), by = 300)) {
      page <- tryCatch(fromJSON(paste0(base, "&offset=", off)), error = function(e) NULL)
      if (!is.null(page))
        res <- rbind(res, page$results[, c("decimalLongitude", "decimalLatitude"), drop = FALSE])
    }
  }
  res <- as.data.frame(res)
  res[!is.na(res$decimalLongitude) & !is.na(res$decimalLatitude), ]
}

# --- UI ---
ui <- fluidPage(
  titlePanel("Ferskvassfisk i Noreg \u2013 GBIF"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("sok", "S\u00f8k etter art:", placeholder = "t.d. Salmo trutta"),
      actionButton("sok_btn", "S\u00f8k", class = "btn-primary"),
      hr(),
      uiOutput("resultat_ui"),
      hr(),
      sliderInput("ar_fra", "Observasjonar fr\u00e5 og med \u00e5r:",
                  min = 2000, max = 2025, value = 2025, step = 1, sep = ""),
      actionButton("hent_btn", "Vis p\u00e5 kart", class = "btn-success"),
      hr(),
      uiOutput("info_ui")
    ),

    mainPanel(
      width = 9,
      plotOutput("kart", height = "700px")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  sokeresultat <- reactiveVal(data.frame())
  valt_art     <- reactiveVal(NULL)
  obs_data     <- reactiveVal(data.frame())

  observeEvent(input$sok_btn, {
    req(nchar(trimws(input$sok)) > 1)
    res <- sok_art(trimws(input$sok))
    sokeresultat(res)
    valt_art(NULL)
    obs_data(data.frame())
  })

  output$resultat_ui <- renderUI({
    res <- sokeresultat()
    if (nrow(res) == 0) return(NULL)
    valalternativ <- setNames(as.character(res$key), res$scientificName)
    tagList(
      selectInput("valt_key", "Vel art:", choices = valalternativ),
      tags$small("Trykk 'Vis p\u00e5 kart' for \u00e5 hente data")
    )
  })

  observeEvent(input$hent_btn, {
    req(input$valt_key)
    data <- hent_gbif(input$valt_key, input$ar_fra)
    obs_data(data)
    res  <- sokeresultat()
    namn <- res$scientificName[as.character(res$key) == input$valt_key]
    valt_art(list(key = input$valt_key, namn = namn))
  })

  output$info_ui <- renderUI({
    obs <- obs_data()
    art <- valt_art()
    if (is.null(art) || nrow(obs) == 0) return(NULL)
    tagList(tags$b(art$namn), tags$br(),
            tags$span(paste0(nrow(obs), " observasjonar")))
  })

  output$kart <- renderPlot({
    obs <- obs_data()
    art <- valt_art()

    p <- ggplot() +
      geom_polygon(
        data = norge_kart,
        aes(x = long, y = lat, group = group),
        fill = "grey75", color = "grey40", linewidth = 0.3
      ) +
      coord_quickmap(xlim = c(4, 32), ylim = c(57, 72)) +
      scale_x_continuous(breaks = seq(4, 32, by = 4),
                         labels = paste0(seq(4, 32, by = 4), "\u00b0\u00d8")) +
      scale_y_continuous(breaks = seq(58, 72, by = 2),
                         labels = paste0(seq(58, 72, by = 2), "\u00b0N")) +
      theme_minimal() +
      theme(
        plot.title       = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle    = element_text(hjust = 0.5, size = 9, color = "grey40"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey85", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(size = 8, color = "grey40"),
        axis.title       = element_blank()
      )

    if (!is.null(art) && nrow(obs) > 0) {
      p <- p +
        geom_point(data = obs,
                   aes(x = decimalLongitude, y = decimalLatitude),
                   color = "#2ca02c", size = 2.5, alpha = 0.75) +
        labs(title    = art$namn,
             subtitle = paste0("GBIF \u2013 Noreg \u2013 n = ", nrow(obs), " observasjonar"))
    } else {
      p <- p + labs(title = "S\u00f8k etter ein art og trykk 'Vis p\u00e5 kart'")
    }
    p
  })
}

shinyApp(ui, server)
