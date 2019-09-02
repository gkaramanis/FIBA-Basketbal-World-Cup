library(tidyverse)
library(lubridate)
library(ggridges)
library(here)

rosters <- read_csv(here::here("data", "FIBA-WBC19-rosters.csv"))

rosters %>%
  mutate(age = year(today()) - year(date_of_birth)) %>%
  group_by(country) %>%
  arrange(age) %>%
  ungroup() %>%
ggplot() +
  stat_density_ridges(aes(height, fct_reorder(toupper(country), height, median)),
    quantile_lines = TRUE, quantiles = 2,
    scale = 0.9, color = "#e7051a", fill = "#f7ca07") +
  scale_x_continuous(expand = c(0, 0), limits = c(1.70, 2.30), breaks = seq(1.7, 2.3, 0.2), labels = c("1.70", "1.90 m", "2.10", "2.30")) +
  coord_cartesian(clip = "off") +
  labs(title = "Height distribution in the FIBA Basketball World Cup 2019",
    subtitle = "By team, sorted by median height",
    caption = "Source: Wikipedia | Graphic: Georgios Karamanis") +
  theme_minimal(base_family = "IBM Plex Sans", base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 50, 20, 20),
    plot.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_text(family = "IBM Plex Sans Medium"),
    # panel.grid.major = element_line(color = "grey80"),
    # panel.grid.minor = element_line(color = "grey85"),
    plot.title = element_text(size = 16, color = "grey40", family = "IBM Plex Sans Medium", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(0, 0, 20, 0), hjust = 0.5),
    plot.caption = element_text(size = 8, color = "grey60", margin = margin(10, 0, 0, 0))
  ) +
  ggsave(here("figures", "ageheight.png"),
    height = 12, width = 8, dpi = 320)
