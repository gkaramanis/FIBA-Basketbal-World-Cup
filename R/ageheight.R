library(tidyverse)
library(lubridate)
library(here)

rosters <- read_csv(here::here("data", "FIBA-WBC19-rosters.csv"))


ageheight_df <- rosters %>%
  mutate(age = year(today()) - year(date_of_birth))