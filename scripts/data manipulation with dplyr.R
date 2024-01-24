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
#alternativamente si può anche usare head per vedere prime sei righe 
#o tail ultime
#filtra togliendo i missing del peso
surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg * 2.2) %>% 
  view()

#confrontiamo il peso dei maschi con il peso delle femmine, 
#praticamente è come se si diviesse la tabella
#per gruppi di interesse
#nb l'obiettivo finale è sull'ultima riga
surveys%>% 
  group_by(sex) %>% 
  summarise(mean_weight = mean(weight, na.rm=TRUE))

#remove na from sex
surveys%>% 
  group_by(sex) %>% 
  filter(!is.na(sex)) %>% 
  summarise(mean_weight = mean(weight, na.rm=TRUE))
 #per creare tabella 
# tabella <- surveys%>% 
#   group_by(sex) %>% 
#   filter(!is.na(sex)) %>% 
#   summarise(mean_weight = mean(weight, na.rm=TRUE))

table5 <- surveys%>% 
  group_by(sex, species_id) %>% 
  filter(!is.na(sex), !is.na(species_id)) %>% 
  summarise(mean_weight = mean(weight, na.rm=TRUE)) %>% 
  print(n=15)
#usando la funzione print sipuò vedere il numero di righe che vogliamo 
#specificando il numero n=
surveys%>% 
  group_by(sex, species_id) %>% 
  filter(!is.na(sex), !is.na(species_id), !is.na(weight)) %>% 
  summarise(mean_weight = mean(weight, na.rm=TRUE), 
            min_weight=min(weight)) %>% 
  arrange(min_weight)
#arrange per ordinarlo in ordine crescente in base a colonna x
#in ordine dercrescente arrange(desc(min_weight))
surveys%>% 
  group_by(sex, species_id) %>% 
  filter(!is.na(sex), !is.na(species_id), !is.na(weight)) %>% 
  summarise(mean_weight = mean(weight, na.rm=TRUE), 
            min_weight=min(weight)) %>% 
  arrange(desc(min_weight))

#count per avere il nuemro di osservazioni
surveys%>% 
  count(sex)

surveys%>% 
  count(sex, species) %>% 
  arrange(species,desc(n))
#desc (n) per l'ordine descrescente del numero di osservazioni

#challenge
animalsinplot <- surveys%>% 
  count(species, plot_type)

animalscharac <-  surveys%>% 
  group_by(species_id) %>% 
  filter(!is.na(hindfoot_length)) %>% 
  summarise(mean_lenght = mean(hindfoot_length), min_lenght 
            = min(hindfoot_length), max_lenght= max(hindfoot_length))

heaviestperyear <- surveys%>% 
  filter((!is.na(weight))) %>% 
   group_by(year) %>% 
  filter(weight == max(weight)) %>% 
  select(year, genus, species_id, weight) %>% 
  arrange(desc(year)) %>% 
  unique()
#  unique() per eliminare i duplicati




