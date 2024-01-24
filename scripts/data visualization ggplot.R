library (ggplot2)
surveys <- read_csv("data/surveys123.csv")

ggplot(data = surveys, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()
str(surveys)

#NB copia e incolla i nomi dalla tabella precedente

plt <- ggplot(data = surveys, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()
plt +
  ggtitle("myfirstggplot")

#hexbin per vedere la distribuzione delle densità
install.packages("hexbin")
library(hexbin)
ggplot(data = surveys, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_hex()

#costruzione interattiva
#alpha è trasparenza
ggplot(data = surveys, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")

ggplot(data = surveys, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.25, aes(color = species_id))

ggplot(data = surveys, mapping = aes(x = weight, y = hindfoot_length, 
                                     color = species_id)) +
  geom_point(alpha = 0.25)
 
#challenge
#scatter weight vs species_id color by polt type
ggplot(data = surveys, mapping = aes(x = weight, y = species_id)) +
  geom_point(aes(color = plot_type))


ggplot(data = surveys, mapping = aes(x = weight, y = species_id)) +
  geom_boxplot() 

ggplot(data = surveys, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, color = "blue") #aggiunge un valore piccolo per ogni coordinata x
#non mostrare outliers

ggplot(data = surveys, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, color = "blue")

#leva il fill dal boxplot fill = NA
ggplot(data = surveys, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "blue") +
  geom_boxplot(outlier.shape = NA, fill = NA) 
install.packages("plotly")

#challenge
ggplot(data = surveys, mapping = aes(x = species_id, y = weight)) +
  geom_violin(color = "green") +
  geom_jitter(alpha = 0.3, color = "blue")

#trasforma tutto in logaritmo
ggplot(data = surveys, mapping = aes(x = species_id, y = weight)) +
  geom_violin(color = "green") +
  scale_y_log10() +
  ylab("wightlog10") #per cambiare nome asse

#challenge: boxplot + jitter scatter di hindfood lenght by species id. 
#boxplot in front of the dots and fille dwith white

ggplot(data = surveys, mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, color = "darkgreen") +
  geom_boxplot(fill = "white") 
#per i colori si puo usare il sistema rgb o hexadecimal

ggplot(data = surveys, mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, color = rgb(.128, .0, .128)) +
  geom_boxplot()


#rendering delle colonne con valori numerici (continui)
ggplot(data = surveys, mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, aes(color = plot_id)) +
  geom_boxplot(fill = "white") 

#convertiamo i valori numerici in valori categorici
#lo possiamp fare introducento il fattore
ggplot(data = surveys, mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_jitter(aes(color = factor(plot_id))) +
  geom_boxplot()

#grouping o cambia colore per ogni gruppo 
yearly_count <- surveys %>% 
  count(year, genus)
ggplot(data = yearly_count, mapping = 
         aes(x = year, y = n,group= genus)) +
  geom_line()

ggplot(data = yearly_count, mapping = 
         aes(x = year, y = n,color= genus)) +
  geom_line()




