library(tidyverse)
test_results <- read_csv("data/test_result.csv")
test_results
#gene        
#baseMean - darti normalizzati dopo deseq execution
#log2FoldChange 
#lfcSE  -standard error associato a log2fc
#stat sarebbe log2FC/lfcse comparato a standard normal distribution
#pvalue  
#padj -adjusted p-value per testing di ipotesi multiple bonferroni e compagni
#comparison - colonna opzionale che in questo caso dice i time points


#MA PLOT
#base mean against log2fc
#organize panels by comparison
library(ggplot2)
test_results %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "red") +
  facet_grid(facets = vars(comparison))

#coloriamo i dots significativi

test2 <- test_results %>% 
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA))
#se minore di x copia il valore in colonna y, se no lascialo NA
ma_plotwithcolors <- test2 %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange))+
  geom_point(alpha = 0.1) +
  geom_point(aes(y = sig), color = "tomato") +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(facets = vars(comparison)) 
#"dodgerblue"
library(patchwork)
(ma_plotwithcolors | pca_plot) +
  plot_annotation(tag_levels = "A")

#plottiamo i geni interessanti
#Visualizing expression trends
candidate_gene <- test_results %>% 
  filter(padj < 0.01) %>% 
  pull(gene) %>% 
  unique()
#pull function per estrarre una colonna e trasformarla in vettore  

trans_cts <- read.csv("data/counts_transformed.csv")
trans_cts_long <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample", values_to = 
                 "cts") %>% 
  full_join(sample_info, by = "sample")

#filter translong for candidate gene e media
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene, strain, minute) %>% 
  summarise(mean_count = mean(cts), nrep = n()) %>% 
  ungroup() #ungroup importante perchè molte funzioni non vanno sui gruppi 
 
str(trans_cts_mean) 
#spaghetti plot
trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_count)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

#SCALING DATA TO IMPROVE VISUALIZATION
#z-score
trans_scale <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene) 
trans_scale2 <-  trans_scale %>% 
  mutate(cts_scale = (cts - mean(cts)/ sd(cts))) %>% 
           group_by(gene, strain, minute) %>% 
           summarise(mean_cts_scale = mean(cts_scale), nrep = n()) %>% 
  ungroup()

trans_scale2 %>% 
  ggplot(aes(x = minute, y = mean_cts_scale)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  geom_hline(yintercept = 0, color = "brown", linetype = "dashed") +
  facet_grid(rows = vars(strain)) +
  scale_x_continuous(breaks = unique(trans_scale2$minute))
#qui rivedere ultima parte perche non ho capito a chi si applica
#questo scale contunous

#CLUSTERING
#1 create a matrix of counts
hclust_matrix <- trans_cts %>% 
  select (-gene) %>% 
  as.matrix()
rownames(hclust_matrix) <- trans_cts$gene
hclust_matrix <- hclust_matrix[candidate_gene, ] 
#zscore la matrice e trasponi
hclust_matrix <- hclust_matrix %>% 
  t() %>% 
  scale() %>% 
  t()
#perchè traspone due volte ?
#perchè la scale function si applica alle colonne, vogliamo scalare il gruppo 
#non il campione!

#Distance Matrix

gene_dist <- dist(hclust_matrix)
#dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
#This must be one of "euclidean", "maximum", "manhattan", "canberra", 
#"binary" or "minkowski". Any unambiguous substring can be given.

#HIERARCHICAL CLUSTERING
gene_hclust <- hclust(gene_dist, method = "complete")
# the agglomeration method to be used. This should be 
# (an unambiguous abbreviation of) one of "ward.D",
# "ward.D2", "single", "complete", "average" (= UPGMA), 
# "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

plot(gene_hclust, labels = F)
#labels = F vuol dire senza labels
#retrieve the clusters from the tree

plot(gene_hclust, labels = F)
#tira una linea
abline(h =10, col = "brown", lwd = 2)
#dividi in clusters a priori
cutree(gene_hclust, k = 5)

gene_cluster <- cutree(gene_hclust, k = 5) %>% 
  enframe() %>% 
  rename(gene = name, cluster = value)

trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")
  
trans_cts_cluster %>% 
  ggplot(aes(x = minute, y = mean_count)) + #qua il mean count deve essere scaled
  geom_line(aes(group = gene)) +
  facet_grid(cols = vars(cluster), rows = vars(strain))

#cambiamo numero dei clusters

gene_cluster <- cutree(gene_hclust, k = 9) %>% 
  enframe() %>% 
  rename(gene = name, cluster = value)

trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")

trans_cts_cluster %>% 
  ggplot(aes(x = minute, y = mean_count)) + #qua il mean count deve essere scaled
  geom_line(aes(group = gene)) +
  facet_grid(cols = vars(cluster), rows = vars(strain))

#heatmap
library(ComplexHeatmap)
Heatmap(hclust_matrix, show_row_names = F)
