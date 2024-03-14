library(tidyverse)
library(ComplexHeatmap)

library(readxl)
#Confronto_tra_i_total_e_i_gender <- read_excel("Downloads/mito als/TOTAL/Confronto tra i total e i gender.xlsx", 
#                                               sheet = "base heatmap")
#View(Confronto_tra_i_total_e_i_gender)
trasp <- Confronto_tra_i_total_e_i_gender [,2:3] %>% 
  t() 
notrap <- Confronto_tra_i_total_e_i_gender [,2:3] %>% 
  as.matrix()
rownames(notrap) <- Confronto_tra_i_total_e_i_gender$`Gene total`
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

#solo per i confronti gender
#library(readxl)
#mito_confronto_fra_tutte_le_medie <- read_excel("Downloads/mito als/working files/mito confronto fra tutte le medie.xlsx", 
#                                                sheet = "log2")
#View(mito_confronto_fra_tutte_le_medie)
gender <- mito_confronto_fra_tutte_le_medie [,5:6] %>% 
  as.matrix()
rownames(gender) <- mito_confronto_fra_tutte_le_medie$Gene
Heatmap(gender,
        cluster_rows = F,  # Disable row clustering
        cluster_columns = T, # Enable column clustering
        show_row_names = TRUE, # Show row names
        row_names_gp = gpar(fontsize = 8), # Customize the fontsize of row names
        row_names_rot = 0, # Rotate row names to be horizontal
        col = colorRampPalette(c("hotpink", "slategrey", "dodgerblue"))(400),
        # Reduce the size of heatmap blocks
        heatmap_legend_param = list(title_gp = gpar(fontsize = 10)), # Customize the fontsize of legend title
        # Position the legend on the right side
        # Turn off margin expansion
        row_dend_side = "left", row_title_side = "left", row_names_side = "left",
        column_dend_side = "top", column_title_side = "top",
        column_names_side = "top"
)


#malati vs sani divisi in m e f
alswt <- mito_confronto_fra_tutte_le_medie [,2:4] %>% 
  as.matrix()
rownames(alswt) <- mito_confronto_fra_tutte_le_medie$Gene
Heatmap(alswt,
        cluster_rows = F,  # Disable row clustering
        cluster_columns = T, # Enable column clustering
        show_row_names = TRUE, # Show row names
        row_names_gp = gpar(fontsize = 8), # Customize the fontsize of row names
        row_names_rot = 0, # Rotate row names to be horizontal
        col = colorRampPalette(c("chartreuse2","slategrey", "orangered2"))(400),
        # Reduce the size of heatmap blocks
        heatmap_legend_param = list(title_gp = gpar(fontsize = 10)), # Customize the fontsize of legend title
        # Position the legend on the right side
        # Turn off margin expansion
        row_dend_side = "left", row_title_side = "left", row_names_side = "left",
        column_dend_side = "top", column_title_side = "top",
        column_names_side = "top"
)

