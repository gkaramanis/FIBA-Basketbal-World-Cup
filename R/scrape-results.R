library(tidyverse)
library(lubridate)
library(rvest)
library(janitor)
library(here)

url <- "https://www.basketball-reference.com/international/fiba-world-cup/2019-schedule.html"

# read in tables
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

results <- html_table(tbls[[1]]) %>% 
  clean_names() %>%
  as_tibble() %>% 
  filter(pts != "September") %>% 
  select(date, home = home_neutral, pts_home = pts_2, visitor = visitor_neutral, pts_visitor = pts) %>%
  mutate(date = ymd(parse_date_time(date, "a b d Y")))
  
write_csv(results, here::here("data", "FIBA-WBC19-results.csv"))
