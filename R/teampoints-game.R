library(here)
library(tidyverse)
library(lemon)
library(gghighlight)

results <- read_csv(here::here("data", "FIBA-WBC19-results.csv"))

home <- results %>% 
  select(date, team = home, points = pts_home)

visitor <- results %>% 
  select(date, team = visitor, points = pts_visitor)

long_results <- bind_rows(home, visitor)

long_results %>%
ggplot(aes(date, points, group = team, color = as.numeric(factor(date)))) +
  geom_pointline(stroke = 0, linesize = 4, size = 4, linejoin = "mitre") +
  gghighlight() +
  ylim(0, 150) +
  scale_color_gradient(
    high = "#e7051a", 
    low = "#1c2b7d"
    ) +
  facet_wrap(vars(team), ncol = 8) +
  labs(
    title = "FIBA Basketball World Cup 2019",
    subtitle = "Team points in the games before the final",
    caption = "Source: basketball-reference.com | Graphic: Georgios Karamanis" 
    ) +
  theme_minimal(base_family = "IBM Plex Sans", base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 15, color = "grey20", family = "IBM Plex Sans Medium"),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(family = "IBM Plex Mono", color = "grey60", size = 12), 
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 24, color = "grey20", family = "IBM Plex Sans Medium", hjust = 0.5),
    plot.subtitle = element_text(size = 20, color = "grey40", margin = margin(0, 0, 20, 0), hjust = 0.5),
    plot.caption = element_text(size = 12, color = "grey60", margin = margin(20, 0, 0, 0))
  ) +
  ggsave("figures/teampoints-game.png", width = 18, height = 11, dpi = 320)