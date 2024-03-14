library(tidyverse)
library(ggplot2)
library(tidyverse)

pca_matrix <- mito_fals_vs_mals %>% 
  column_to_rownames("Accession") %>% 
  as.matrix() %>% 
  na.omit() %>% 
  t() 

sample_pca <- prcomp(pca_matrix, retx = TRUE, center = TRUE, scale. = TRUE,
                     tol = NULL, rank. = NULL )

#da matrice a tabella
ccp <- as_tibble(pca_matrix, rownames = "sample")

pc_eigenvalues1 <- sample_pca$sdev ^2

pc_eigenvalues1 <- tibble(PC = factor(1:length(pc_eigenvalues1)), 
                          variance = pc_eigenvalues1) %>% 
  mutate(pct = variance/sum(variance)*100) %>%
  mutate(pct_cum = cumsum(pct))
#pareto plot
library(ggplot2)
pareto <- pc_eigenvalues1 %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct), fill = "lightgrey") +
  geom_line(aes(y = pct_cum, group = 1), color = "blue") +
  geom_point(aes(y= pct_cum)) +
  labs(x = "Principal Component", y= "Fraction of Explained Variance")+
  geom_hline(yintercept = 75, color = "brown") +
  theme(plot.background = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", size = 0.5, fill = NA)) +
  ggtitle("Explained Variance per Component - FvsM ALS")


pc_scores <-  sample_pca$x %>% 
  as_tibble(rownames = "sample")

sample_info <- sample_name_mito

joinscor <- pc_scores %>% 
  inner_join(sample_info, by = "sample") 

joinscor %>% 
  ggplot(aes(x = PC1, y = PC2, color = gender, shape = gender )) +
  geom_point() +
  geom_hline(yintercept =  0)+
  geom_vline(xintercept = 0) +
  scale_color_manual(values = c("F" = "pink", "M" = "blue")) + 
  theme(plot.background = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", size = 0.5, fill = NA)) +
  ggtitle("PCA- M vs F als") 


library(ellipse)
library(stats)

library(cluster)
library(ggfortify)
autoplot(sample_pca) 
library(ggplot2)

sample_info2 <- sample_info[1:6,]

pca_plot <- autoplot(sample_pca, data = sample_info2, 
                     color = "gender", shape = "gender", frame = TRUE, frame.type = 'norm') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = c("F" = "pink", "M" = "blue")) + 
  theme(plot.background = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", size = 0.5, fill = NA)) +
  ggtitle("PCA- Mito fALS vs mALS ") 
library(plotly)

plot3d <- plot_ly(pc_scores, x = ~PC1, y = ~PC2, z = ~PC3, 
                  color = ~sample_info$gender, colors = c("#FF00FF", "cyan3"),
                  text = sample) %>%
  add_markers(size = 12) %>%
  add_text(text = sample_info$sample,  # Specify the text (sample names) again
           textposition = "bottom", showlegend = FALSE) %>%
  layout(title = "PCA 3D - Gender Mito ALS vs WT") 



library(patchwork) 
(pca_plot | pareto) +
  plot_annotation(tag_levels = "a", 
                  tag_prefix = "(", tag_suffix = ")", 
                  theme = theme(plot.title = element_text(face = "bold", vjust = 0, hjust = 0.5))
  )



library(broom)

tidy(sample_pca, matrix = "eigenvalues")
#il comando tidy ci permette di sostituire tutto questo codice:
# pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)), 
#                          variance = pc_eigenvalues) %>% 
#   mutate(pct = variance/sum(variance)*100) %>%
#   mutate(pct_cum = cumsum(pct))
#si possono creare le proprie funzioni e dargli dei nomi
#cosicchè possiamo solo scrivere la funzione e non fare copia
#incolla di piu azioni

tidy(sample_pca, matrix = "loadings")
#ci sono anche molti altri pacchetti, cercarli
install.packages("plotly")
install.packages("stats")
library(plotly)
library(stats)
data(matrix_topi)
