# Kart over Norge med gjedde og aure frå GBIF (etter 2024)
# Krev pakkar: ggplot2, maps, mapproj, jsonlite

library(ggplot2)
library(maps)
library(jsonlite)

# --- Kartdata for Noreg ---
norge_kart <- tryCatch({
  map_data("worldHires", region = "Norway")
}, error = function(e) {
  map_data("world", region = "Norway")
})

# --- Hjelpefunksjon: hent alle sider frå GBIF ---
hent_gbif <- function(taxon_key) {
  base_url <- paste0(
    "https://api.gbif.org/v1/occurrence/search",
    "?taxonKey=", taxon_key,
    "&country=NO&year=2025,2026&hasCoordinate=true&limit=300"
  )
  first    <- fromJSON(paste0(base_url, "&offset=0"))
  total    <- first$count
  results  <- first$results

  if (total > 300) {
    offsets <- seq(300, total, by = 300)
    for (off in offsets) {
      page <- fromJSON(paste0(base_url, "&offset=", off))
      results <- rbind(results[, c("decimalLongitude", "decimalLatitude")],
                       page$results[, c("decimalLongitude", "decimalLatitude")])
    }
  }
  results <- as.data.frame(results[, c("decimalLongitude", "decimalLatitude")])
  results[!is.na(results$decimalLongitude) & !is.na(results$decimalLatitude), ]
}

# --- Hent data ---
gjedde <- hent_gbif(2346633)   # Esox lucius
aure   <- hent_gbif(8215487)   # Salmo trutta

# --- Lag kartet ---
ggplot() +
  geom_polygon(
    data = norge_kart,
    aes(x = long, y = lat, group = group),
    fill = "grey75", color = "grey40", linewidth = 0.4
  ) +
  geom_point(
    data = aure,
    aes(x = decimalLongitude, y = decimalLatitude),
    color = "black", size = 1.5, alpha = 0.5
  ) +
  geom_point(
    data = gjedde,
    aes(x = decimalLongitude, y = decimalLatitude),
    color = "#2ca02c", size = 2, alpha = 0.8
  ) +
  coord_map("mercator", xlim = c(4, 32), ylim = c(57, 72)) +
  scale_x_continuous(
    breaks = seq(4, 32, by = 4),
    labels = paste0(seq(4, 32, by = 4), "\u00b0\u00d8")
  ) +
  scale_y_continuous(
    breaks = seq(58, 72, by = 2),
    labels = paste0(seq(58, 72, by = 2), "\u00b0N")
  ) +
  annotate("point", x = 6, y = 71.2, color = "#2ca02c", size = 2) +
  annotate("text",  x = 6.8, y = 71.2, label = paste0("Gjedde (n=", nrow(gjedde), ")"),
           hjust = 0, size = 3) +
  annotate("point", x = 6, y = 70.5, color = "black", size = 1.5) +
  annotate("text",  x = 6.8, y = 70.5, label = paste0("Aure (n=", nrow(aure), ")"),
           hjust = 0, size = 3) +
  labs(
    title = "Gjedde og aure i Noreg (GBIF, etter 2024)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 8, color = "grey40")
  )
