library(tidyverse)
library(here)
library(ggtext)

player_stats <- read_csv(here::here("data", "FIBA-WBC19-playerstats.csv"))

points_share <- player_stats %>% 
  select(player, team, points) %>% 
  mutate(team = toupper(team)) %>%
  group_by(team) %>% 
  mutate(
    team_points = sum(points),
    player_share = points/team_points
    ) %>%
  arrange(team, -player_share) %>%
    mutate(
      player_color = case_when(
        row_number() == 1 ~ "red",
        TRUE ~ "#f7ca07"
      )
   )

ordered_teams <- points_share %>% filter(player_color == "red") %>% arrange(player_share) %>%
  select(team)

points_share$team <- factor(points_share$team, ordered_teams$team)

ggplot(points_share, aes(x = team, y = player_share, fill = player_color)) +
  geom_bar(stat="identity", color = "grey90", width = 0.8) +
  geom_text(data = subset(points_share, player_color == "red"), aes(label = player, x = team, y = 0.005), hjust = 0, color = "grey90", family = "IBM Plex Sans Condensed", size = 3.5) +
  geom_text(data = subset(points_share, player_color == "red"), aes(label = paste0(round(player_share*100, 1), "%"), x = team, y = player_share - 0.005), color = "grey90", family = "IBM Plex Sans Condensed ExtraLight", hjust = 1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(sec.axis = sec_axis(~.*1, labels = scales::percent), limits = c(0, 1), labels = scales::percent, expand = c(0, 0)) +
  scale_fill_identity() +
  labs(
    title = "Players with the highest percentage of team points in the 2019 FIBA<br>Basketball World Cup (31 Aug - 7 Sep)",
    subtitle = "Guna Ra of South Korea has scored almost **one third** of his team's points (31.9%, 89 points)<br>Dar Tucker and Ahmet Duverioglu have together scored **more than half** of all Jordan's points (28.7%<br>and 58 points, 28.2% and 57 points, respectively). The plot shows the <span style='color:#e7051a'>**players with the highest<br>percentage**</span> of team points against their <span style='color:#f7ca07'>**teammates' percentages**</span>.",
    caption = "Source: basketball-reference.com | Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "IBM Plex Sans", base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_text(family = "IBM Plex Mono Bold", size = 12), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_markdown(size = 16, lineheight = 1.1, color = "grey20", family = "IBM Plex Sans Medium"),
    plot.subtitle = element_markdown(size = 14, lineheight = 1.1, color = "grey40", margin = margin(0, 0, 20, 0)),
    plot.caption = element_markdown(size = 8, color = "grey60", margin = margin(20, 0, 0, 0))
  ) +
  ggsave(here::here("figures", "teampoints-perc.png"), height = 12, width = 12, dpi = 320)
