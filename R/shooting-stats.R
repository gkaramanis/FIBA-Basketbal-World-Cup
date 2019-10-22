library(here)
library(tidyverse)
library(ggtext)
library(lemon)

totalstats <- read_csv(here::here("data", "FIBA-WBC19-totalteamstats.csv"))

shot_stats <- totalstats %>%
  select(team, freethrows_percentage, twopoints_percentage, threepoints_percentage) %>% 
  gather("type", "percentage", freethrows_percentage, twopoints_percentage, threepoints_percentage) %>% 
  mutate(
    team_color = case_when(
      team == "Serbia" ~ "#e7051a",
      team == "France" ~ "#1c2b7d",
      team == "Australia" ~ "#f7ca07",
      TRUE ~ "grey60"
    )
  )

shot_stats$type <- factor(shot_stats$type, c("freethrows_percentage", "twopoints_percentage", "threepoints_percentage"))
 
ggplot(shot_stats, aes(x = type, y = percentage, group = team, color = team_color, alpha = (team_color != "grey60"))) +
  geom_hline(yintercept = max(totalstats$threepoints_percentage), color = "grey70", linetype = "dotted") +
  geom_pointline(stroke = 0, linesize = 4, size = 6, linejoin = "mitre") +
  # geom_point() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_x_discrete(labels = c("Free Throws", "Two-pointers", "Three-pointers")) +
  scale_color_identity() +
  scale_alpha_manual(values = c(0.15, 1)) +
  labs(
    title = "Shooting percentages in the first round of the FIBA<br>World Cup 2019: Serbia and the others",
    subtitle = "<span style='color:#e7051a'>**Serbia**</span> has a three-point field goal percentage of 53%, shooting<br>3-pointers better than 23 teams shoot 2-pointers. <span style='color:#1c2b7d'>**France**</span> is very<br>close with 52.6%. <span style='color:#e7051a'>**Serbia**</span> has also the best two-point field goal<br> percentage with 67.9% while <span style='color:#f7ca07'>**Australia**</span> has the second best with 62%",
    caption = "Source: basketball-reference.com | Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "IBM Plex Sans", base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(family = "IBM Plex Mono", size = 12), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_markdown(size = 16, lineheight = 1.1, color = "grey20", family = "IBM Plex Sans Medium"),
    plot.subtitle = element_markdown(size = 14, lineheight = 1.1, color = "grey40", margin = margin(0, 0, 20, 0)),
    plot.caption = element_markdown(size = 8, color = "grey60", margin = margin(20, 0, 0, 0))
  ) +
  ggsave(here::here("figures", "shooting-stats.png"),
         height = 12, width = 8)

# Colors of the tournament logo 
#e7051a Red Orange (Red)
#f7ca07 Tangerine Yellow (Yellow)
#1c2b7d Blue Gem (Violet)
