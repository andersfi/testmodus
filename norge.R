# Kart over Norge
# Krev pakkar: ggplot2, maps, mapproj
# Merk: fylkesgrenser krev pakken 'mapdata' (install.packages("mapdata"))

library(ggplot2)
library(maps)

# Forsøk å laste mapdata for fylkesgrenser, fall tilbake til world-data
norge_kart <- tryCatch({
  map_data("worldHires", region = "Norway")
}, error = function(e) {
  map_data("world", region = "Norway")
})

# Lag kartet
ggplot(data = norge_kart, aes(x = long, y = lat, group = group)) +
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
    title = "Noreg",
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
