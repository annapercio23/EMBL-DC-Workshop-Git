library(tidyverse)
library(readxl)
library(ggplot2)
library(ggrepel)
ALS_GemelleTDP43_3RUN <- read_excel("~/Downloads/gemelle/ALS_GemelleTDP43_3RUN.xlsx", 
                                         sheet = "Vulcano")
ALS_G12_LFQ_twins_proteins <- ALS_GemelleTDP43_3RUN %>% 
  na.omit()
ALS_G12_LFQ_twins_proteins$color <- ifelse(ALS_G12_LFQ_twins_proteins$Log2FC < 0.3785, "green", "red") 
ALS_G12_LFQ_twins_proteins$outliers <- ifelse(abs(ALS_G12_LFQ_twins_proteins$Log2FC) > 0.3785, "Yes", "No")


ggplot(ALS_G12_LFQ_twins_proteins, aes(x = Log2FC, y = `p-value`)) +
  geom_point(aes(color = ifelse(Log2FC < -0.3785 & `p-value` > 1.3, "green", ifelse(Log2FC > 0.3785 & `p-value` > 1.3, "red", "grey"))), alpha = 0.7) +
  scale_color_identity() +
  geom_text_repel(data = subset(ALS_G12_LFQ_twins_proteins, (Log2FC < -0.3785 & `p-value` > 1.3) | (Log2FC > 0.3785 & `p-value` > 1.3)),
                  aes(label = Gene), size = 3, fontface = "bold",
                  box.padding = 1, point.padding = 1) +
  scale_color_manual(values = c("green" = "green", "red" = "red", "grey" = "grey"),
                     labels = c("Downregulated in TARDBP-d", "Not Significant", "Upregulated in TARDBP-d"),
                     name = "Category") +
  geom_hline(yintercept = 1.3, linetype = "dotted") +
  geom_vline(xintercept = 0.3785, linetype = "dotted") +
  geom_vline(xintercept = -0.3785, linetype = "dotted") +
  labs(x = "Log2 Fold Change", y = "-Log10 p-Value", title = "TARDB TWINS Disease vs Control") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))




