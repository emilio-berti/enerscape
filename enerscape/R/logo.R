library(ggplot2)
library(hexSticker)

ggplot() +
  geom_hexagon(size = 1.2, fill = "steelblue", color = "grey85") +
  geom_pkgname("enerscape",
               x = 1, y = 1.4,
               color = "grey85",
               size = 8) +
  theme_void() +
  theme_transparent()

ggsave("../Figures/logo.svg", width = 3.6, height = 4) #check ratio
