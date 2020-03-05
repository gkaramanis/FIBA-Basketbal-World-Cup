# helper-translation-es
library(countrycode)
library(tidyverse)
library(here)
# extract country names ---------------------------------------------------

# getting shorts, and the iso codes, as they might be usefull for maps and flags
countries_for_join <- countrycode::codelist %>% 
select(cldr.short.en,cldr.short.es,iso2c,iso3c,un.name.en,country.name.en)

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
# rosters_base_translation <- readr::read_csv(here::here("data-es","FIBA-WBC19-lista_jugadores.csv"))
# roster_countries_join_cleaned <- readr::read_csv(here::here("data-es","roster_paises_traduccion.csv"))

# FIBA-WBC19-playerstats.csv ----------------------------------------------

playerstats_base <- readr::read_csv(here::here("data","FIBA-WBC19-playerstats.csv")) %>% janitor::clean_names()

playerstats_base_join <- playerstats_base %>% 
  left_join(roster_countries_join_cleaned,by=c("team"="country")) %>% 
  select(player,pais,games,minutes_played,
         field_goals,field_goal_attempts,field_goal_percentage,
         three_point_field_goals,three_point_field_goal_attempts,three_point_field_goal_percentage,
         two_point_field_goals,two_point_field_goal_attempts,two_point_field_goal_percentage,
         effective_field_goal_percentage ,free_throws,free_throws_attempts,free_throws_percentage,
         offensive_rebounds,defensive_rebounds,total_rebounds,
         assists,steals,blocks,turnovers,personal_fouls,points) %>% 
    # http://www.costaricaspanish.net/2011/09/basketball-terms-in-spanish/
    # https://es.wikipedia.org/wiki/Rebote_(baloncesto)
    rename(jugador=player,
           
           partidos=games,
           minutos_jugados=minutes_played,
           
           goles_de_campo=field_goals,
           goles_de_campo_intentos=field_goal_attempts,
           goles_de_campo_porcentaje=field_goal_percentage,
           
           goles_de_campo_tres_puntos=three_point_field_goals,
           goles_de_campo_tres_puntos_intentos=three_point_field_goal_attempts,
           goles_de_campo_tres_puntos_porcentaje=three_point_field_goal_percentage,
           
           goles_de_campo_dos_puntos=two_point_field_goals,
           goles_de_campo_dos_puntos_intentos=two_point_field_goal_attempts,
           goles_de_campo_dos_puntos_porcentaje=two_point_field_goal_percentage,
           
           goles_de_campo_efectividad_porcentaje=effective_field_goal_percentage ,
           tiros_libres=free_throws,
           tiros_libres_intentos=free_throws_attempts,
           tiros_libres_porcentaje=free_throws_percentage,
           
           rebotes_ofensiva=offensive_rebounds,
           rebotes_defensiva=defensive_rebounds,
           rebotes_total=total_rebounds,
           
           asistencias=assists,
           robadas=steals,
           bloqueos=blocks,
           pelotas_perdidas=turnovers,
           faltas_personales=personal_fouls,
           puntos=points)
# readr::write_csv(playerstats_base_join,path = here::here("data-es","FIBA-WBC19-estadisticas_jugadores.csv"))



# FIBA-WBC19-results.csv ----------------------------------------------
# roster_countries_join_cleaned <- readr::read_csv(here::here("data-es","roster_paises_traduccion.csv"))

results_base <- readr::read_csv(here::here("data","FIBA-WBC19-results.csv")) %>% janitor::clean_names()
# south korea its shown as "korea", so it generates NA, will clean.
results_base_fix <- results_base %>% 
  mutate(home=if_else(home=="Korea","South Korea",home)) %>% 
  mutate(visitor=if_else(visitor=="Korea","South Korea",visitor)) 


roster_countries_join_cleaned_tmp <- roster_countries_join_cleaned %>% 
  select(country,pais)

results_base_join <- results_base_fix %>% 
  left_join(roster_countries_join_cleaned_tmp,by=c("home"="country") ) %>% 
  rename(local=pais) %>%
  left_join(roster_countries_join_cleaned_tmp,by=c("visitor"="country") ) %>% 
  rename(visitante=pais) %>% 
  select(-home,-visitor) %>% 
  rename(
    fecha=date,
    puntos_local=pts_home,
    puntos_visitante=pts_visitor
  ) %>% 
  select(fecha,
         local,puntos_local,visitante,puntos_visitante)
# every miss is korea
results_base_join %>% filter(!complete.cases(results_base_join))
# readr::write_csv(results_base_join,path = here::here("data-es","FIBA-WBC19-resulados.csv"))

# FIBA-WBC19-totalteamstats.csv ----------------------------------------------

totalteamstats_base <- readr::read_csv(here::here("data-es","FIBA-WBC19-totalteamstats_fix.csv")) %>% janitor::clean_names()
totalteamstats_base_fix <- totalteamstats_base %>% 
  mutate(team=if_else(team=="Korea","South Korea",team))
roster_countries_join_cleaned_tmp <- roster_countries_join_cleaned %>% 
  select(country,pais) 

totalteamstats_base_joined <- totalteamstats_base_fix %>% 
  left_join(roster_countries_join_cleaned_tmp,by = c("team"="country")) %>% 
  # colnames()
  select(pais,
         games,
         field_goals_made,
         field_goals_attempted,
         field_goals_percentage,
         three_points_made,
         three_points_attempted,
         three_points_percentage,
         two_points_made,
         two_points_attempted,
         two_points_percentage,
         free_throws_made,
         free_throws_attempted,
         free_throws_percentage,
         offensive_rebounds,
         defensive_rebounds,
         total_rebounds,
         assists,
         steals,
         blocks,
         turnovers,
         personal_fouls,
         points) %>% 
  rename(
    partidos=games,
    goles_de_campo_realizados=field_goals_made,
    goles_de_campo_intentos=field_goals_attempted,
    goles_de_campo_porcentaje=field_goals_percentage,
    goles_de_campo_tres_puntos_realizados=three_points_made,
    goles_de_campo_tres_puntos_intentos=three_points_attempted,
    goles_de_campo_tres_puntos_porcentaje=three_points_percentage,
    goles_de_campo_dos_puntos_realizados=two_points_made,
    goles_de_campo_dos_puntos_intentos=two_points_attempted,
    goles_de_campo_dos_puntos_porcentaje=two_points_percentage,
    tiros_libres_realizados=free_throws_made,
    tiros_libres_intentos=free_throws_attempted,
    tiros_libres_porcentaje=free_throws_percentage,
    rebotes_ofensiva=offensive_rebounds,
    rebotes_defensiva=defensive_rebounds,
    rebotes_total=total_rebounds,
    asistencias=assists,
    robadas=steals,
    bloqueos=blocks,
    pelotas_perdidas=turnovers,
    faltas_personales=personal_fouls,
    puntos=points)

# readr::write_csv(totalteamstats_base_joined,path = here::here("data-es","FIBA-WBC19-estadisticas_totales_equipos.csv"))

# FIBA-WBC19-totalteamstats.csv ----------------------------------------------

venues_base <- readr::read_csv(here::here("data-es","FIBA-WBC19-venues.csv")) %>% janitor::clean_names()

venues_base_es <- venues_base %>% rename(ubicacion=location,
                                         nombre=name,
                                         capacidad=capacity) %>% 
  mutate(nombre=str_replace(nombre,"Nanjing Youth Olympic Sports Park Gymnasium","Parque Gimnasio de Jóvenes Olimpiadas de Nanjing")) %>% 
  mutate(nombre=str_replace(nombre,"Wukesong Arena","Arena de Wukesong")) %>% 
  mutate(nombre=str_replace(nombre,"Shanghai Oriental Sports Center","Centro de Deportes oriental de Shanghai")) %>% 
  mutate(nombre=str_replace(nombre,"Wuhan Gymnasium","Gimnasio de Wuhan")) %>% 
  mutate(nombre=str_replace(nombre,"Dongguan Cultural and Sports Centre","Centro Cultural y de Deportes de Dongguan")) %>% 
  mutate(nombre=str_replace(nombre,"Foshan International Sports & Cultural Arena","Arena Internacional Cultural y de Deportes de Foshan")) %>% 
  mutate(nombre=str_replace(nombre,"Guangzhou Gymnasium","Gimnasio de Guangzhou")) %>% 
  mutate(nombre=str_replace(nombre,"Shenzhen Bay Sports Centre","Centro de deportes de la Bahía de Shenzhen"))
# readr::write_csv(venues_base_es,path = here::here("data-es","FIBA-WBC19-estadios.csv"))
