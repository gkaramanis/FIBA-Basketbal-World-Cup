library(tidyverse)
library(lubridate)
library(here)
library(ggtext)

rosters <- read_csv(here::here("data", "FIBA-WBC19-rosters.csv"))


hdf <- rosters %>%
  mutate(position = case_when(
    position == "SG" ~ "G",
    position == "PG" ~ "G",
    position == "SF" ~ "F",
    position == "PF" ~ "F",
    TRUE ~ position
  )) %>% 
  group_by(country) %>% 
  mutate(sum_height = sum(height))


ggplot(data = hdf, aes(x = fct_reorder(toupper(country), sum_height))) +
  geom_bar(aes(y = height, fill = position), stat = "identity", width = 0.8) +
  geom_text(aes(y = 0.3, label = (toupper(country))), hjust = 0, size = 4, color = "grey90", family = "IBM Plex Sans Medium") +
  geom_text(aes(y = sum_height - 0.3, label = sum_height), hjust = 1, size = 4, color = "grey90", family = "IBM Plex Sans Light") +
  coord_flip(expand = 0) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20), labels = c("0", "5 m", "10", "15", "20")) +
  scale_fill_manual(values=c("#1c2b7d", "#f7ca07", "#e7051a"),
                    limits = c("G", "F", "C"),
                    labels = c("guards", "forwards", "centers")) +
  labs(title = "Serbia is the tallest team in FIBA Basketball World Cup 2019",
    subtitle = "Summed height of <span style='color:#1c2b7d'>**guards (G, SG, PG)**</span>, <span style='color:#f7ca07'>**forwards (F, SF, PF)**</span> and <span style='color:#e7051a'>**centers (C)**</span>, by team",
    caption = "Source: Wikipedia | Graphic: Georgios Karamanis") +
  theme_minimal(base_family = "IBM Plex Sans", base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_markdown(size = 16, color = "grey40", family = "IBM Plex Sans Medium"),
    plot.subtitle = element_markdown(size = 12, color = "grey40", margin = margin(0, 0, 20, 0)),
    plot.caption = element_markdown(size = 8, color = "grey60", margin = margin(10, 0, 0, 0))
  ) +
  ggsave(here::here("figures", "height.png"), 
         height = 12, width = 8, dpi = 320)

# Colors of the tournament logo 
#e7051a Red Orange (Red)
#f7ca07 Tangerine Yellow (Yellow)
#1c2b7d Blue Gem (Violet)

