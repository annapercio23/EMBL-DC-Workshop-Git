library(tidyverse)
library(ComplexHeatmap)

library(readxl)
library(readxl)
#Label_free_G1_vs_G2_160424 <- read_excel("Downloads/gemelle/Label free_G1 vs G2 160424.xlsx", 
#                                         sheet = "significative 1e5", col_types = c("skip", 
#                                                                                    "text", "skip", "skip", "numeric", 
#                                                                                    "numeric", "numeric", "skip", "skip", 
#                                                                                    "skip"))
trasp <- Label_free_G1_vs_G2_160424 [,2:3] %>% 
  t() 
notrap <- Label_free_G1_vs_G2_160424 [,2:3] %>% 
  as.matrix()
rownames(notrap) <- Label_free_G1_vs_G2_160424$`Gene Name`
Heatmap(notrap, show_row_names = T)
library(RColorBrewer)
Heatmap(notrap,
        cluster_rows = FALSE,  # Disable row clustering
        cluster_columns = TRUE, # Enable column clustering
        show_row_names = TRUE, # Show row names
        row_names_gp = gpar(fontsize = 12), # Customize the fontsize of row names
        col = colorRampPalette(c("chartreuse2","slategrey", "orangered2"))(400),
        # Turn off margin expansion
        row_dend_side = "left", row_title_side = "left", row_names_side = "left",
        column_dend_side = "top", column_title_side = "top",
        column_names_side = "top")
# Turn off the grid

#normalizzazione
library(dplyr)
log_due<- Label_free_G1_vs_G2_160424[,2:3] %>%
  mutate_at(vars(-group_cols()), log2) %>%
  as.data.frame()

Heatmap(log_due,
        cluster_rows = FALSE,  # Disable row clustering
        cluster_columns = TRUE, # Enable column clustering
        show_row_names = TRUE, # Show row names
        row_names_gp = gpar(fontsize = 12), # Customize the fontsize of row names
        col = colorRampPalette(c("chartreuse2","grey30", "orangered2"))(400),
        # Turn off margin expansion
        row_dend_side = "left", row_title_side = "left", row_names_side = "left",
        column_dend_side = "top", column_title_side = "top",
        column_names_side = "top")

