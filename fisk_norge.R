# Fire-panel kart over utvalde fiskeartar i Noreg (GBIF, etter 2024)
# Krev pakkar: ggplot2, maps, mapproj, jsonlite, patchwork

library(ggplot2)
library(maps)
library(jsonlite)
library(patchwork)

# --- Kartdata ---
norge_kart <- tryCatch({
  map_data("worldHires", region = "Norway")
}, error = function(e) {
  map_data("world", region = "Norway")
})

# --- Hjelpefunksjon: hent alle observasjonar frå GBIF ---
hent_gbif <- function(taxon_key) {
  base_url <- paste0(
    "https://api.gbif.org/v1/occurrence/search",
    "?taxonKey=", taxon_key,
    "&country=NO&year=2025,2026&hasCoordinate=true&limit=300"
  )
  first   <- fromJSON(paste0(base_url, "&offset=0"))
  total   <- first$count
  res     <- first$results[, c("decimalLongitude", "decimalLatitude")]

  if (total > 300) {
    for (off in seq(300, total, by = 300)) {
      page <- fromJSON(paste0(base_url, "&offset=", off))
      res  <- rbind(res, page$results[, c("decimalLongitude", "decimalLatitude")])
    }
  }
  res <- as.data.frame(res)
  res[!is.na(res$decimalLongitude) & !is.na(res$decimalLatitude), ]
}

# --- Hent data ---
orret    <- hent_gbif(8215487)   # Salmo trutta       – ørret
gjedde   <- hent_gbif(2346633)   # Esox lucius        – gjedde
roye     <- hent_gbif(4284021)   # Salvelinus alpinus – røye
stingsild <- hent_gbif(5739510)  # Pungitius pungitius – nipigget stingsild

# --- Hjelpefunksjon: lag eitt panelkart ---
lag_panel <- function(obs, tittel, farge) {
  ggplot() +
    geom_polygon(
      data = norge_kart,
      aes(x = long, y = lat, group = group),
      fill = "grey75", color = "grey40", linewidth = 0.3
    ) +
    geom_point(
      data = obs,
      aes(x = decimalLongitude, y = decimalLatitude),
      color = farge, size = 1.5, alpha = 0.75
    ) +
    coord_map("mercator", xlim = c(4, 32), ylim = c(57, 72)) +
    scale_x_continuous(breaks = seq(4, 32, by = 8),
                       labels = paste0(seq(4, 32, by = 8), "\u00b0\u00d8")) +
    scale_y_continuous(breaks = seq(58, 72, by = 4),
                       labels = paste0(seq(58, 72, by = 4), "\u00b0N")) +
    labs(
      title    = tittel,
      subtitle = paste0("n = ", nrow(obs)),
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_text(hjust = 0.5, size = 11, face = "bold"),
      plot.subtitle    = element_text(hjust = 0.5, size = 8, color = "grey40"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 6, color = "grey40")
    )
}

# --- Lag panela ---
p1 <- lag_panel(orret,     "Ørret",              "black")
p2 <- lag_panel(gjedde,    "Gjedde",             "#2ca02c")
p3 <- lag_panel(roye,      "R\u00f8ye",          "#e06c00")
p4 <- lag_panel(stingsild, "Nipigget stingsild", "#9467bd")

# --- Sett saman 2x2 ---
(p1 | p2) / (p3 | p4) +
  plot_annotation(
    title   = "Ferskvassfisk i Noreg (GBIF, etter 2024)",
    theme   = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  )
