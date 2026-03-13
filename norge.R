# Kart over Norge med gjedde-registreringar frå GBIF (etter 2024)
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

# --- Hent gjedde-observasjonar frå GBIF (Esox lucius, Noreg, etter 2024) ---
gbif_url <- paste0(
  "https://api.gbif.org/v1/occurrence/search",
  "?taxonKey=2346633&country=NO&year=2025,2026",
  "&hasCoordinate=true&limit=300"
)
gbif_data   <- fromJSON(gbif_url)
gjedde      <- gbif_data$results[, c("decimalLongitude", "decimalLatitude", "year")]
gjedde      <- gjedde[!is.na(gjedde$decimalLongitude) & !is.na(gjedde$decimalLatitude), ]

# --- Lag kartet ---
ggplot() +
  geom_polygon(
    data = norge_kart,
    aes(x = long, y = lat, group = group),
    fill = "grey75", color = "grey40", linewidth = 0.4
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
  labs(
    title    = "Gjedde i Noreg (GBIF, etter 2024)",
    subtitle = paste0("n = ", nrow(gjedde), " registreringar"),
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle    = element_text(hjust = 0.5, size = 10, color = "grey40"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 8, color = "grey40")
  )
