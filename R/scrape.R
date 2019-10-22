library(tidyverse)
library(rvest)
library(janitor)
library(here)

url <- "https://en.m.wikipedia.org/wiki/2019_FIBA_Basketball_World_Cup_squads"

# read in tables
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

# read in countries list
countries <- webpage %>%
  html_nodes("h3 .mw-headline") %>%
  xml_attr("id") %>% 
  str_replace(., "_", " ")

# read in rosters
teams = list()
for (i in  seq(2, 64, 2)) {
  team <- html_table(tbls[[i]], fill = TRUE)
  team$country <- countries[i/2]
  team$Ctr. <- html_node(html_nodes(tbls[[i]], "tbody tr:not(:first-child)"), ".flagicon a") %>% xml_attr("title")
  teams[[i]] <- team
}
teams <- do.call(rbind, teams)

# clean
teams_clean <- teams %>%
  clean_names() %>% 
  rename(
    position = pos,
    number = no,
    date_of_birth = age_date_of_birth,
    club_country = ctr
    ) %>% 
  # extract dob, height, captain and change types
  mutate(
    date_of_birth = as.Date(str_extract(date_of_birth, "\\d{4}-\\d{2}-\\d{2}")),
    height = as.numeric(str_extract(height, "\\d{1}\\.\\d{1,2}")),
    captain = case_when(
      str_detect(name, "\\(C\\)") ~ TRUE,
      TRUE ~ NA),
    name = str_remove(name, ".\\(C\\)"),
    number = as.numeric(number)
    ) %>%
  # reorder columns
  select(country, 1:3, captain, everything())

# write_csv(teams_clean, here::here("data", "FIBA-WBC19-rosters.csv"))
