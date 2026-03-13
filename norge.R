# Kart over Norge med fylkesgrenser
# Krev pakkar: ggplot2, maps, mapproj, jsonlite
# Fylkesgrenser: ivanhjel/counties_norway_2024 (CC-lisens, kjelde: Kartverket)

library(ggplot2)
library(maps)
library(jsonlite)

# --- Last ned fylkesgrenser frå GitHub ---
url <- "https://raw.githubusercontent.com/ivanhjel/counties_norway_2024/main/counties_norway_2024.geojson"
geojson <- fromJSON(url, simplifyVector = FALSE)

# Konverter GeoJSON til data.frame for ggplot2
geojson_til_df <- function(features) {
  df_list <- lapply(seq_along(features), function(i) {
    feat <- features[[i]]
    navn <- feat$properties$navn
    geom <- feat$geometry

    # Støttar Polygon og MultiPolygon
    rings <- if (geom$type == "Polygon") {
      list(geom$coordinates)
    } else {
      geom$coordinates  # MultiPolygon
    }

    do.call(rbind, lapply(seq_along(rings), function(j) {
      ring <- rings[[j]][[1]]  # ytre ring
      coords <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
      data.frame(
        long  = coords[, 1],
        lat   = coords[, 2],
        group = paste(i, j, sep = "_"),
        navn  = navn
      )
    }))
  })
  do.call(rbind, df_list)
}

fylker_df <- geojson_til_df(geojson$features)

# --- Lag kartet ---
ggplot(fylker_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey75", color = "grey40", linewidth = 0.4) +
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
    title = "Noreg \u2013 fylkesgrenser",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 8, color = "grey40")
  )
