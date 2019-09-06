library(tidyverse)
library(lubridate)
library(rvest)
library(janitor)
library(here)

url <- "https://www.basketball-reference.com/international/fiba-world-cup/2019.html"

# read in tables
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

stats <- html_table(tbls[[1]])
