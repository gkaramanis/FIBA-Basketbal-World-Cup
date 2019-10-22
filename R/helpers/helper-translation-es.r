# helper-translation-es
library(countrycode)
library(tidyverse)
library(here)
#country names

# extract country names ---------------------------------------------------
# first aproximation
# filter_es <- colnames(countrycode::codelist)[str_detect(colnames(countrycode::codelist),"es")]
# names_en_es_all <- countrycode::codelist  %>%  as_tibble() %>% 
#   select(country.name.en,short ,filter_es) 

# getting shorts, and the iso codes, as they might be usefull for maps and flags
countries_for_join <- countrycode::codelist %>% 
select(cldr.short.en,cldr.short.es,iso2c,iso3c,un.name.en,country.name.en)

# https://github.com/vincentarelbundock/countrycode/issues/211
# library(countrycode)
# library(tidyverse)
# countrycode::codelist %>%
#   select(cldr.short.en,cldr.short.es,iso2c,iso3c,un.name.en,country.name.en) %>%
#   filter(iso2c=="CI")
#

# apply to FIBA-WBC-history.csv specific dataset -----------------------------------------------

history_base <- readr::read_csv(here::here("data","FIBA-WBC-history.csv")) %>% janitor::clean_names()
history_base_tidy <- history_base %>% tidyr::pivot_longer(c("gold","silver","bronze")) %>% 
  rename(medal=name,country=value)

history_only_countries <- history_base_tidy %>% distinct(country)
history_translation <- history_only_countries %>% 
  dplyr::left_join(countries_for_join,by=c("country"="cldr.short.en")) %>% 
  mutate(pais=cldr.short.es)

# FIBA-WBC-history.csv specific dataset: handle special cases ----------------------------------------------------
history_translation %>% filter(!complete.cases(history_translation))
# USA 
us_code_data <- countries_for_join %>% filter(str_detect(str_to_lower(iso2c),"us")) %>% 
  mutate(country="USA") %>% mutate(pais=cldr.short.es) %>% 
  select(country,pais,iso2c,iso3c)

# Yugoslavia 
yugoslavia_code_data <- countries_for_join %>% filter(str_detect(str_to_lower(country.name.en),"yug")) 
# not found on default so adding via custom name by "empiric knowladge" 
yugoslavia_code_data <- yugoslavia_code_data %>%  mutate(custom_es = "Yugoslavia") %>% 
  mutate(country="Yugoslavia") %>% mutate(pais=custom_es) %>% 
  select(country,pais,iso2c,iso3c)

# Soviet Union >> not found :S, custom name then
soviet_union_code_data <- countries_for_join %>% filter(str_detect(str_to_lower(country.name.en),"union")) 
soviet_union_code_data <- countrycode::codelist %>%  filter(str_detect(str_to_lower(cowc),"ussr"))
# not found on default so adding via custom name by "empiric knowladge" 
soviet_union_code_data <- data.frame(country="Soviet Union",
                                     custom_es="Unión Sovietica",
                                     iso2c=NA,
                                     iso3c=NA,
                                     stringsAsFactors = FALSE) %>% as_tibble() %>% 
  mutate(pais=custom_es)%>% 
  select(country,pais,iso2c,iso3c)

# Phillippines  >> Philippines
philippines_code_data <- countries_for_join %>% 
  filter(str_detect(str_to_lower(country.name.en),"phil")) %>% 
  mutate(country="Phillippines")%>% mutate(pais=cldr.short.es)%>% 
  select(country,pais,iso2c,iso3c)


history_translation <- history_translation %>% 
  select(country,pais,iso2c,iso3c) %>% 
  filter(complete.cases(history_translation)) %>% 
  union_all(us_code_data) %>% 
  union_all(yugoslavia_code_data) %>% 
  union_all(soviet_union_code_data) %>% 
  union_all(philippines_code_data) 
#   left_join(yugoslavia_code_data,by= "country") %>% 
#   left_join(soviet_union_code_data,by= "country") %>% 
#   left_join(philippines_code_data,by= "country")

# ok , have some NA's in iso codes as they do not exist anymore.
history_translation %>% 
  filter(!complete.cases(history_translation)) 

# save the mapping
readr::write_csv(history_translation,path = here::here("data-es","historia_traduccion.csv"))

# cross with results ------------------------------------------------------
history_translation_es <- history_base_tidy %>% 
  left_join(history_translation,by = "country") %>% 
  select(-country,-iso2c,-iso3c) %>% 
  pivot_wider(names_from = medal,values_from = pais) %>% 
  rename(anio=year,oro=gold,plata=silver,bronce=bronze)

readr::write_csv(history_translation_es,path = here::here("data-es","FIBA-WBC-historia.csv"))

# FIBA-WBC19-rosters.csv ----------------------------------------------
rosters_base <- readr::read_csv(here::here("data","FIBA-WBC19-rosters.csv")) %>% janitor::clean_names()
extracted_countries_roster <- rosters_base %>% distinct(country)

roster_countries_join_clean <- extracted_countries_roster %>% 
  left_join(countries_for_join, by=c("country"="country.name.en"))

# only 2 missings
roster_countries_join_clean %>% 
  filter(is.na(cldr.short.es))

# cross missings:
# its not translated in countries ! its default country name that is french.
# english is ivory coast, and spanish is: costa de marfil
ivory_coast <- countries_for_join %>% 
  filter(str_detect(iso2c,"CI")) %>% 
  mutate(country="Ivory Coast",
         pais="Costa de Marfil") %>% 
  select(country,pais,iso2c,iso3c)
  # filter(str_detect(str_to_lower(country.name.en),"ivory"))

# Czech Republic
czech_republic <- countries_for_join %>% 
  filter(str_detect(iso2c,"CZ")) %>% 
  mutate(country=un.name.en,
         pais="República Checa") %>% 
  select(country,pais,iso2c,iso3c)

roster_countries_join_cleaned <- roster_countries_join_clean %>% 
  filter(!is.na(cldr.short.es)) %>% 
  rename(pais=cldr.short.es) %>% 
  select(country,pais,iso2c,iso3c) %>% 
  union_all(ivory_coast) %>% 
  union_all(czech_republic)  


# roster_countries_join_cleaned %>% filter(complete.cases(roster_countries_join_cleaned))
# readr::write_csv(roster_countries_join_cleaned,path = here::here("data-es","roster_paises_traduccion.csv"))

# https://en.wikipedia.org/wiki/Basketball_positions
# http://www.fiba.basketball/es/basketballworldcup/2019/game/1309/Argentina-Francia#tab=overview
# http://www.fiba.basketball/es/basketballworldcup/2019/game/3108/Cote-d-Ivoire-China
# https://en.wikipedia.org/wiki/Starting_lineup#Basketball
# https://es.wikipedia.org/wiki/Posiciones_del_baloncesto

position_desc_en <- c("Shooting guard",
                      "Point guard",
                      "Small forward",
                      "Power forward",
                      "Center",
                      "Guard",
                      "Forward",
                      "Guard/Forward")

position_desc_es <- c("Escolta(SG)",
                      "Base(PG)",
                      "Alero(SF)",
                      "Ala-pívot(PF)",
                      "Pívot(C)",
                      "Guardia(G)",
                      "Alero(F)",
                      "Guardia/Alero(G/F)")

position_descriptions <- rosters_base %>% 
  distinct(position) %>% 
  mutate(position_desc_en=position_desc_en,
         position_desc_es=position_desc_es)

# readr::write_csv(position_descriptions,path = here::here("data-es","roster_positions_traduccion.csv"))
# rosters_base %>% filter(position=="F")

rosters_base_translation <- rosters_base %>% 
  left_join(roster_countries_join_cleaned) %>% 
  select(-iso2c,-iso3c,-country) %>% 
  left_join(position_descriptions,by="position") %>% 
  select(-position,-position_desc_en,-position) %>% 
  rename(posicion=position_desc_es,
         numero=number,
         nombre=name,
         capitan=captain,
         fecha_de_nacimiento=date_of_birth,
         altura=height,
         club_pais=club_country) %>% 
  select(pais,posicion,numero,nombre,capitan,fecha_de_nacimiento,altura,club,club_pais)


# readr::write_csv(rosters_base_translation,path = here::here("data-es","FIBA-WBC19-lista_jugadores.csv"))


# FIBA-WBC19-playerstats.csv ----------------------------------------------

playerstats_base <- readr::read_csv(here::here("data","FIBA-WBC19-playerstats.csv")) %>% janitor::clean_names()

# FIBA-WBC19-results.csv ----------------------------------------------

results_base <- readr::read_csv(here::here("data","FIBA-WBC19-results.csv")) %>% janitor::clean_names()

# FIBA-WBC19-totalteamstats.csv ----------------------------------------------

totalteamstats_base <- readr::read_csv(here::here("data-es","FIBA-WBC19-totalteamstats_fix.csv")) %>% janitor::clean_names()

