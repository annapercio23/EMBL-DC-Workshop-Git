library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
#per vedere che tipo di dato è
str(surveys)
#selezzione (tabella, e nome colonna)
select(surveys, plot_id, species_id, weight)
#non selezionare
select(surveys, -record_id, -species_id)
#filtra i dati basandosi su una colonna in
#questo caso year e in questo caso anno 1995
filter(surveys,year == 1995)
filter(surveys,year == 1995, sex == "M")

surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, sex, species_id, weight)

survey3 <-select(filter(surveys, weight < 5), species_id, sex, weight)

surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)
# %>% prende la funzione a destra e la applica su cosa abbiam scritto
#a sinistra, dopo ogni %>% andare a capo

#challenge

challenge2 <- surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)

#inserire una nuova colonna contenente un dato che si rifà
#a qualcosa già presente es. weight da g a kg

surveys %>% 
  mutate(weight_kg = weight/1000) %>% 
  view
#view per vedere direttamente la tabella
#possiamo anche aggiungere piu colonne insieme
surveys %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg * 2.2) %>% 
  view()
#alternativamente si puo anche usare head per vedere prime sei righe
#filtra togliendo i missing del peso
surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg * 2.2) %>% 
  view()


