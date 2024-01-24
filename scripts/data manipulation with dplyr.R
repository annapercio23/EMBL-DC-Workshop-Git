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
#a sinistra

#challenge

challenge2 <- surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)
