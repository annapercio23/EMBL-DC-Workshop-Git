library(pheatmap)
library(tidyverse)
library(dplyr)

imaggiori <- Label_free_G1_vs_G2_160424[,2:3] %>%
  mutate_at(vars(-group_cols()), ~ log2 %>%
  as.data.frame()


pheatmap(
  imaggiori,
  cluster_rows = F,
  cluster_cols = F,
  labels_row = row.names(Label_free_G1_vs_G2_160424$`Gene Name`),  # Provide your row labels
  labels_col = c("CTRL", "ALS"), # Provide your column labels
  color = colorRampPalette(c("green","grey", "red"))(n = 100),
  display_numbers = FALSE,
  cellwidths = 20,
  fontsize_row = 10,  # Adjust font size of row labels
  angle_row = 0 )

