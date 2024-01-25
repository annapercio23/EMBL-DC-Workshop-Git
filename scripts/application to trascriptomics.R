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
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

#boxplot

tablejoinraw %>% 
  ggplot(aes(x = factor(minute), y = log10(cts + 1), 
             fill = strain)) +
  geom_boxplot() +
  ggtitle("boxplot")
  theme_bw()
#dato che minute sono numeri li dobbiamo mettere come factors  
#fill strain per fare il colore wt e mut diversi

tablejoinraw %>% 
    ggplot(aes(x = factor(minute), y = log10(cts + 1),fill = strain)) +
    geom_boxplot() +
  facet_grid(cols = vars(replicate))
#vars per cosa lo vuoi separare  
#check correlation
#correlazione tra wt a t0 e t30 con scatter plot

correlation <-  counts_transformed %>% 
  select(gene, wt_0_r1, wt_30_r1)
#usiamo counts transformed perchè i valori di interesse
#sono in verticale
correlation %>% 
  ggplot(aes(x = wt_0_r1, y = wt_30_r1)) +
  geom_point() +
  geom_abline(color = "red") 
# geom_abline per la linea di correlazione
#nb cosa scrivi dopo sta davanti nel grafico

#look at correlation of count data across all samples

trans_cts_corr <- counts_transformed %>% 
  select(wt_0_r1 : mut_180_r3) %>% 
  cor(method = "spearman")
#select (- gene) per togliere gene column
#cor per vedere le correlazioni espletando il metodo in questo
#caso spearman, l'output è una MATRICE
# var(x, y = NULL, na.rm = FALSE, use)
# 
# cov(x, y = NULL, use = "everything",
#     method = c("pearson", "kendall", "spearman"))
# 
# cor(x, y = NULL, use = "everything",
#     method = c("pearson", "kendall", "spearman"))

#heatmap
library(corrr)
#se si deve installare install.package
rplot(trans_cts_corr)
#size della bolla proporzionale alla correlazione

#yassification dell'asse x
rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#hjust per mettere il testo allineato a dx dell'asse

#compare trans e raw counts
summary(raw_cts_long$cts)
summary(trans_cts_long$cts)

counts_raw %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2))+
  geom_point()
#log 2

counts_raw %>% 
  ggplot(aes(x = wt_0_r1 + 1, y = wt_0_r2 + 1))+
  geom_point()+
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_abline(color = "red")
#scale_x_continuous() perchè so valori continyi
#trans transform 

raw_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() +
  geom_abline(color = "red") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

#dsyc2 package per RNA transfonmation e
#normalization

trans_cts_long %>% 
  group_by(gene) %>% 
  summarise(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() 
  







  