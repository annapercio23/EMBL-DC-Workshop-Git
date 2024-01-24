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
  geom_line()+
  geom_point() #per aggiungere i punti

ggplot(data = yearly_count, mapping = 
         aes(x = year, y = n,color= genus, shape =genus)) + #per aggiungere
  #forme diverse per i vari gruppi
  geom_line()+
  geom_point() 
#si può fare il grafico con il pipe system
yearly_count_graph <- yearly_count %>% 
  ggplot(mapping = aes(x = year, y = n, color = genus)) +
  geom_line()

yearly_count_graph

#come separare il nostro plot in tutti subplot e fare un matrix plot
#la tecnica si chiama  FACETING
ggplot(data = yearly_count, mapping = 
         aes(x = year, y = n,color= genus, shape =genus))+
  geom_line() +
  facet_wrap(facets = vars(genus))
#aggiungi il sesso
yearly_count2 <- surveys %>% 
  count(year, genus, sex)

ggplot(data = yearly_count2, mapping = 
         aes(x = year, y = n,color= sex))+
  geom_line() +
  facet_wrap(facets = vars(genus))

#specifichiamo cosa vogliamo nelle colonne e nei rows
#qua si usa grid
ggplot(data = yearly_count2, mapping = 
         aes(x = year, y = n,color= sex))+
  geom_line() +
  facet_grid(rows = vars(sex), cols = vars(genus))


#theme 
#praticamente sono dei preset 
grafico <- ggplot(data = yearly_count2, mapping = 
         aes(x = year, y = n,color= sex))+
  geom_line() +
  facet_grid(rows = vars(sex), cols = vars(genus)) +
  xlab("numero di osservazioni") +
  ylab("anno") +
  theme_bw(base_line_size = 18)
grafico
#salva il plot
ggsave(filename = "downloads/bellissimografico.pdf",
       plot = grafico,
       width = 20,
       height = 20,
      )

#nomina ascissa e ordinata 
#xlab() e ylab()

ggplot(data = yearly_count2, mapping = 
         aes(x = year, y = n,color= sex))+
  geom_line() +
  facet_grid(rows = vars(sex), cols = vars(genus)) +
  xlab("numero di osservazioni") +
  ylab("anno") +
  #theme_minimal()+ #per togliere background
  theme(plot.background = element_blank(),
        legend.position = "bottom",
        aspect.ratio = 1,
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        panel.grid = element_blank())
#panel.grid(element_blank())
#scale_color_manual per mettere noi i colori 
#per leggenda labels name = ""
#hjust e vjust 0.5 per mettere la roba al centro
#togli leggenda legend.position = "none"
#face = "bold"

lol <- ggplot(data = yearly_count2, mapping = 
         aes(x = year, y = n,color= sex))+
  geom_line() +
  facet_grid(rows = vars(sex), cols = vars(genus)) +
  xlab("numero di osservazioni") +
  ylab("anno") +
  theme_minimal()
#geom_text e geom_annotate per aggiugerci cose

#CHALLENGE
install.packages("ggpubr")
#ggoubr per dare i box plot molto belli con i p-value




