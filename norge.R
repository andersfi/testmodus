# Kart over Norge
# Krev pakkar: ggplot2, maps (installer med install.packages() om naudsynt)

library(ggplot2)
library(maps)

# Hent kartdata for Noreg
norge_kart <- map_data("world", region = "Norway")

# Lag kartet
ggplot(data = norge_kart, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "#4d94ff", color = "white", linewidth = 0.3) +
  coord_map("mercator", xlim = c(4, 32), ylim = c(57, 72)) +
  labs(
    title = "Noreg",
    x = "Lengdegrad",
    y = "Breiddegrad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    panel.background = element_rect(fill = "#cce5ff", color = NA),
    panel.grid = element_line(color = "white", linewidth = 0.2)
  )
