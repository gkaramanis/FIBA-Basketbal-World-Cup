library(rvest)
library(tidyverse)
library(janitor)
library(here)


url <- "https://www.basketball-reference.com/international/fiba-world-cup/2019_totals.html"

# read in tables
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

player_stats <- html_table(tbls[[1]]) %>% 
  clean_names() %>% 
  as_tibble() %>% 
  filter(player != "Player") %>% 
  mutate_all(na_if,"") %>%
  mutate(team = replace(team, team == "Korea", "South Korea")) %>%
  set_names(c("player",
              "team",
              "games",
              "minutes_played",
              "field_goals",
              "field_goal_attempts",
              "field_goal_percentage",
              "three_point_field_goals",
              "three_point_field_goal_attempts",
              "three_point_field_goal_percentage",
              "two_point_field_goals",
              "two_point_field_goal_attempts",
              "two_point_field_goal_percentage",
              "effective_field_goal_percentage",
              "free_throws",
              "free_throws_attempts",
              "free_throws_percentage",
              "offensive_rebounds",
              "defensive_rebounds",
              "total_rebounds",
              "assists",
              "steals",
              "blocks",
              "turnovers",
              "personal_fouls",
              "points"
              ))

# write_csv(player_stats, here::here("data", "FIBA-WBC19-playerstats.csv"))
