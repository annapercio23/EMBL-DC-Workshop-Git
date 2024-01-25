library(tidyverse)

trans_cts <- read.csv("data/counts_transformed.csv")
sample_info <- read.csv(("data/sample_info.csv"))

#i dati devono essere ristruttturati perchè la funzione della pca non va
#bene con le tabelle ma lavora con le 
#MATRICE
#LA MATRICE DEVE ESSERE TRASPOSTA!

pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>% 
  as.matrix() %>% 
  t()
#trasponi è solo t()

sample_pca <-  prcomp(pca_matrix)
str(sample_pca)
class(sample_pca)
summary(sample_pca)
prcomp(pca_matrix, retx = TRUE, center = TRUE, scale. = FALSE,
       tol = NULL, rank. = NULL)
pca_matrix[1:10, 1:5]
#da matrice a tabella
ccp <- as_tibble(pca_matrix, rownames = "samples")

#eigenvalues sd.dev alla seconsa
pc_eigenvalues <- sample_pca$sdev ^2

#ci serve tabella per graficare
pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)), 
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance/sum(variance)*100) %>%
  mutate(pct_cum = cumsum(pct))
#pareto plot
library(ggplot2)
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y= pct_cum)) +
  labs(x = "Principal Component", y= "Fraction of Explained Variance")+
  geom_hline(yintercept = 90, color = "blue") 
  
#install.packages("ggfortify")
#arcobaleno ggplot(aes(x = PC, color = PC)) +

pc_scores <-  sample_pca$x %>% 
  as_tibble(rownames = "sample")

#usare ggsave per le altre estensioni tipo .tif .png

pc_scores %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point() +
  
joinscor <- pc_scores %>% 
  full_join(sample_info, by = "sample") 

joinscor %>% 
  ggplot(aes(x = PC1, y = PC2, 
             color = factor(minute), shape = strain)) +
  geom_point() +
  geom_hline(yintercept =  0)+
  geom_vline(xintercept = 0) +
  theme(plot.background = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank())


pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")

top_10_genes <-pc_loadings %>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(loading)) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()
  

top_loadings <- pc_loadings %>% 
  filter(gene %in% top_10_genes)

plt <- top_loadings %>% 
  ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")), color = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene), nudge_y = 0.05, size = 3) +
  scale_x_continuous(expand = c(0.02,0.02)) 

  

