library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")
data.frame200 <- surveys[200,]

#factors
str(surveys)
#useful to work with categorical data
#impose one of the columns to be a factor ($ per selezionare la colonna)

surveys$sex <- factor(surveys$sex)
factor(surveys$sex)
# [ reached getOption("max.print") -- omitted 33786 entries ]
# Levels: F M
#sorted in alphabetical order

#per vedere che livelli ci sono
levels(surveys$sex)
nlevels(surveys$sex)

#come forzare i fattori a prendere i livelli come vogliamo noi

sex <- factor(c("male","female", "female", "male"))
#qui normale imposto alfabeticamente 2,1,1,2
sex <- factor(sex, levels = c("male", "female"))
#qui imposto 1,2,2,1

#Challenge
#quanti rabbits ci sono?
surveys$taxa <- factor(surveys$taxa)
factor(surveys$taxa)
levels(surveys$taxa)
nlevels(surveys$taxa)
taxa <- factor((c("Bird", "Rabbit", "Reptile", "Rodent" )))
sum(surveys$taxa == "Rabbit")
# 75
# si può anche usare summary(surveys)
   
#numero di livelli nel genus
surveys$genus <- factor(surveys$genus)
factor(surveys$genus)
levels(surveys$genus)
nlevels(surveys$genus)
# > nlevels(surveys$genus)
# [1] 26

#convertire fattore a carattere
as.character(sex)

#come convertire un numero a fattore
year_fct <- factor(c(1990, 1983,1977, 1997))
#livelli assegnati in ordine crescente
levels(year_fct)
as.numeric(year_fct)
#come cambiare l'ordine di assegnazione
as.numeric(as.character(year_fct))
#ottenere i livelli con levels(year_fct)
#conversione livelli in numeri as.numeric(levels(year_fct))
#indexing [year_fct]
as.numeric(levels(year_fct))[year_fct]

# renaming factors
plot(surveys$sex)
summary(surveys$sex)
# F     M  NA's 
# 15690 17348  1748 
#in questo caso vogliamo graficare 
#anche quelli di cui non sappiamo il sesso
sex <- addNA(sex)
levels(sex)
#sostituzione di na come undetermined
levels(sex)[3] <- "undetermined"
nlevels(sex)
plot(sex)
#Challenge

levels(sex)[1:2] <- c("female", "male")
plot(sex)

sex <- factor(sex, levels = c("undetermined", "female", "male"))
plot(sex)
