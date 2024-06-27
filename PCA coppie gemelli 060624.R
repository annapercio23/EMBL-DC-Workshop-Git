library(tidyverse)
library(ggplot2)
library(tidyverse)
gemelli_tutti_120624 <- read_excel("~/Downloads/GEMELLI 060624/excel gemelli/gemelli tutti 120624.xlsx")

pca_matrix <- gemelli_tutti_120624 %>% 
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
  ggtitle("Explained Variance per Component - TWINS ALS")

print(pareto)

pc_scores <-  sample_pca$x %>% 
  as_tibble(rownames = "Sample")

lista_campioni <- read_excel("~/Downloads/GEMELLI 060624/excel gemelli/lista campioni.xlsx")

sample_info <- lista_campioni

joinscor <- pc_scores %>% 
  inner_join(sample_info, by = "Sample") 

joinscor %>% 
  ggplot(aes(x = PC1, y = PC2, color = Category, shape = Category)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text(aes(label = Sample), color = "black", hjust = 0.5, vjust = -0.5, size = 3) + # Aggiungi i nomi dei campioni
  scale_color_manual(values = c("ALS" = "lightblue", "CTRL" = "coral")) + 
  theme(plot.background = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", size = 0.5, fill = NA)) +
  ggtitle("PCA- TWINS ALS vs CTRL")



library(ellipse)
library(stats)

library(cluster)
library(ggfortify)
autoplot(sample_pca) 
library(ggplot2)

sample_info2 <- sample_info[1:12,]

pca_plot <- autoplot(sample_pca, data = sample_info2, 
                     color = "Category", shape = "Category", frame = TRUE, frame.type = 'norm') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = c("ALS" = "lightblue", "CTRL" = "coral")) + 
  theme(plot.background = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", size = 0.5, fill = NA)) +
  ggtitle("PCA- TWINS ALS vs CTRL ") 

print(pca_plot)
library(plotly)

plot3d <- plot_ly(pc_scores, x = ~PC1, y = ~PC2, z = ~PC3, 
                  color = ~sample_info$Category, colors = c("cyan3", "coral"),
                  text = sample) %>%
  add_markers(size = 12) %>%
  add_text(text = sample_info$Sample,  # Specify the text (sample names) again
           textposition = "bottom", showlegend = FALSE) %>%
  layout(title = "PCA 3D - TWINS ALS vs CTRL") 
print(plot3d)


library(patchwork) 
(pca_plot | pareto) +
  plot_annotation(tag_levels = "a", 
                  tag_prefix = "(", tag_suffix = ")", 
                  theme = theme(plot.title = element_text(face = "bold", vjust = 0, hjust = 0.5))
  )


#Per coppie

joinscor <- pc_scores %>% 
  inner_join(sample_info, by = "Sample") 

joinscor %>% 
  ggplot(aes(x = PC1, y = PC2, color = factor(Couple), shape = Category)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text(aes(label = Sample), color = "black", hjust = 0.5, vjust = -0.5, size = 3) + # Aggiungi i nomi dei campioni
  theme(plot.background = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", size = 0.5, fill = NA)) +
  ggtitle("PCA- TWINS ALS vs CTRL")



library(ellipse)
library(stats)

library(cluster)
library(ggfortify)
autoplot(sample_pca) 
library(ggplot2)

sample_info2 <- sample_info[1:12,]

pca_plot <- autoplot(sample_pca, data = sample_info2, 
                     color = "Couple", shape = "Category", frame = TRUE, frame.type = 'norm') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(plot.background = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", size = 0.5, fill = NA)) +
  ggtitle("PCA- TWINS ALS vs CTRL ") 

print(pca_plot)
library(plotly)

plot3d <- plot_ly(pc_scores, x = ~PC1, y = ~PC2, z = ~PC3, 
                  color = ~sample_info$Category, colors = c("cyan3", "coral"),
                  text = sample) %>%
  add_markers(size = 12) %>%
  add_text(text = sample_info$Sample,  # Specify the text (sample names) again
           textposition = "bottom", showlegend = FALSE) %>%
  layout(title = "PCA 3D - TWINS ALS vs CTRL") 

library(ggfortify)
library(ggalt)
library(dplyr)
library(ggforce)
pca_plot <-  joinscor %>% 
  ggplot(aes(x = PC1, y = PC2, color = factor(Couple), shape = Category)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text(aes(label = Sample), color = "black", hjust = 0.5, vjust = -0.5, size = 3) + # Aggiungi i nomi dei campioni
  theme(plot.background = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(color = "black", size = 0.5, fill = NA)) +
  ggtitle("PCA- TWINS ALS vs CTRL")
print(pca_plot)
# Add ellipses around each couple
gg <- pca_plot +
  geom_encircle(aes(x = PC1, y = PC2, group = Couple, fill = factor(Couple)), 
                data = joinscor, color = "black", size = 1, 
                expand = 0.05, 
                alpha = 0.2)

print(gg)
