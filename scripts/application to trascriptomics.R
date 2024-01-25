# Crea cartella
# dir.create("data")
# 
# # Download the data provided by your collaborator
# #using a for loop to automate this step
# for(i in c("counts_raw.csv", "counts_transformed.csv", "sample_info.csv", "test_result.csv")){
#   download.file(
#     url = paste0("https://github.com/tavareshugo/data-carpentry-rnaseq/blob/master/data/", i, "?raw=true"),
#     destfile = paste0("data/", i)
#   )
# }
# #destfile = paste0("data/", i) per la cartella dove vuoi mettere file
# 

#Create objects
library(tidyverse)
counts_raw <- read.csv("data/counts_raw.csv")
counts_transformed <- read.csv("data/counts_transformed.csv")
sample_info <- read.csv(("data/sample_info.csv"))
test_result <- read.csv(("data/test_result.csv"))

# Remove df object from R Environment
#remove(df)
str(counts_transformed)
#la tabella si trova in formato wide quindi bisogna trasformarla
#nel formato pivot long perchè ggplot preferisce il long format
#quindi in questo caso una colonna per gene names
#una colonna per nome campione e poi valori

trans_cts_long <-  counts_transformed %>% 
  pivot_longer(names_to = "sample", 
               values_to = "cts", cols = wt_0_r1 : mut_180_r3)
#combine le due tabelle transcts e sample info
#questa funzione è tipo un cerca vert
#full_join
#inner_join solo per le entries in comune tra entrambe le tabelle
#left_join ( a sx la tabella intatta e a dx ci attacca la seconda 
#tabella con le info corrispondenti )
tablejoin <- full_join(trans_cts_long, sample_info, by = "sample")

library(ggplot2)

tablejoin %>% 
  ggplot(aes(x = cts)) +
  geom_freqpoly()
#frequency polygon

tablejoin %>% 
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly()
#stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#questo è un default di geom_freqpoly 

tablejoin %>% 
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly(binwidth = 1) 

#usiamo grid e non wrap perchè sappiamo per cosa dividere il plot
tablejoin %>% 
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

#CHALLENGE-- make a pivot long for raw counts and display in log 10
raw_cts_long <-  counts_raw %>% 
  pivot_longer(names_to = "sample", 
               values_to = "cts", cols = wt_0_r1 : mut_180_r3)
tablejoinraw <- full_join(raw_cts_long, sample_info, by = "sample")

tablejoinraw %>% 
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly(binwidth = 1) +
  scale_x_log10() 
  
tablejoinraw %>% 
  ggplot(aes(x = cts, color = replicate)) +
  geom_freqpoly(binwidth = 1) +
  scale_x_log10() +
  facet_grid(rows = vars(strain), cols = vars(minute))
#si possono inserire i valori logaritmici anche nell'aes
tablejoinraw %>% 
  ggplot(aes(x = log10(cts), color = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))

#Removed 645 rows containing non-finite values (`stat_bin()`). 
# perchè log di 0
#aggiungiamo 1 a tutti i conteggi così da poter far vedere 
#gli 0 in scala logaritmica
tablejoinraw %>% 
  ggplot(aes(x = log10(cts + 1), color = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))



