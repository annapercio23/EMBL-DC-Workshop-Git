library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggrepel)

#MITO ALS vs WT
#Mice_als_volcano_plots <- read_excel("~/Downloads/mito als/Volcano plots/Mice als volcano plots.xlsx", 
#                                       +     sheet = "als vs wt")
#
Mice_als_volcano_plots$color <- ifelse(Mice_als_volcano_plots$FC < 0.3785, "green", "red")
Mice_als_volcano_plots$outliers <- ifelse(abs(Mice_als_volcano_plots$FC) > 0.3785, "Yes", "No")

ggplot(Mice_als_volcano_plots, aes(x = FC, y = pvalue)) +
  geom_point(aes(color = ifelse(FC < -0.3785 & pvalue > 1.3, "green", ifelse(FC > 0.3785 & pvalue > 1.3, "red", "grey"))), alpha = 0.7) +
  scale_color_identity() +
  geom_text_repel(data = subset(Mice_als_volcano_plots, (FC < -0.3785 & pvalue > 1.3) | (FC > 0.3785 & pvalue > 1.3)),
                  aes(label = Gene), size = 3, family = "bold",
                  box.padding = 1, point.padding = 1) +
  scale_color_manual(values = c("green" = "green", "red" = "red", "grey" = "grey"),
                     labels = c("Downregulated in ALS", "Upregulated in ALS", "Not Significant"),
                     name = "Category") +
  geom_hline(yintercept = 1.3, linetype = "dotted") +  # Add a horizontal line at y = 1.3
  geom_vline(xintercept = 0.3785, linetype = "dotted") +  # Add a vertical line
  geom_vline(xintercept = -0.3785, linetype = "dotted") +  # Add a vertical line
  labs(x = "Log2 Fold Change", y = "-Log10 p-Value", title = "Mito ALS vs WT") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),  # Make title bold
        axis.title = element_text(face = "bold"),  # Make axis titles bold
        legend.title = element_text(face = "bold"))  # Make legend title bold


#fALS vs fWT
#Mice_als_volcano_plots <- read_excel("~/Downloads/mito als/Volcano plots/Mice als volcano plots.xlsx", 
#                                       +     sheet = "falsvsfwt")
#

ggplot(Mice_als_volcano_plots, aes(x = FC, y = pvalue)) +
  geom_point(aes(color = ifelse(FC < -0.3785 & pvalue > 1.3, "green", ifelse(FC > 0.3785 & pvalue > 1.3, "red", "grey"))), alpha = 0.7) +
  scale_color_identity() +
  geom_text_repel(data = subset(Mice_als_volcano_plots, (FC < -0.3785 & pvalue > 1.3) | (FC > 0.3785 & pvalue > 1.3)),
                  aes(label = Gene), size = 3, family = "bold",
                  box.padding = 1, point.padding = 1) +
  scale_color_manual(values = c("green" = "green", "red" = "red", "grey" = "grey"),
                     labels = c("Downregulated in fALS", "Upregulated in fALS", "Not Significant"),
                     name = "Category") +
  geom_hline(yintercept = 1.3, linetype = "dotted") +  # Add a horizontal line at y = 1.3
  geom_vline(xintercept = 0.3785, linetype = "dotted") +  # Add a vertical line
  geom_vline(xintercept = -0.3785, linetype = "dotted") +  # Add a vertical line
  labs(x = "Log2 Fold Change", y = "-Log10 p-Value", title = "fALS vs fWT") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),  # Make title bold
        axis.title = element_text(face = "bold"),  # Make axis titles bold
        legend.title = element_text(face = "bold"))  # Make legend title bold

#mALS vs mWT
#Mice_als_volcano_plots <- read_excel("~/Downloads/mito als/Volcano plots/Mice als volcano plots.xlsx", 
#                                     sheet = "mals vs mwt")

ggplot(Mice_als_volcano_plots, aes(x = FC, y = pvalue)) +
  geom_point(aes(color = ifelse(FC < -0.3785 & pvalue > 1.3, "green", ifelse(FC > 0.3785 & pvalue > 1.3, "red", "grey"))), alpha = 0.7) +
  scale_color_identity() +
  geom_text_repel(data = subset(Mice_als_volcano_plots, (FC < -0.3785 & pvalue > 1.3) | (FC > 0.3785 & pvalue > 1.3)),
                  aes(label = Gene), size = 3, family = "bold",
                  box.padding = 1, point.padding = 1) +
  scale_color_manual(values = c("green" = "green", "red" = "red", "grey" = "grey"),
                     labels = c("Downregulated in mALS", "Upregulated in mALS", "Not Significant"),
                     name = "Category") +
  geom_hline(yintercept = 1.3, linetype = "dotted") +  # Add a horizontal line at y = 1.3
  geom_vline(xintercept = 0.3785, linetype = "dotted") +  # Add a vertical line
  geom_vline(xintercept = -0.3785, linetype = "dotted") +  # Add a vertical line
  labs(x = "Log2 Fold Change", y = "-Log10 p-Value", title = "mALS vs mWT") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),  # Make title bold
        axis.title = element_text(face = "bold"),  # Make axis titles bold
        legend.title = element_text(face = "bold"))  # Make legend title bold

# total als vs wt
#Mice_als_volcano_plots <- read_excel("~/Downloads/mito als/Volcano plots/Mice als volcano plots.xlsx", 
#                                     sheet = "total als vs wt")

names(Mice_als_volcano_plots)
ggplot(Mice_als_volcano_plots, aes(x = FC, y = pvalue)) +
  geom_point(aes(color = ifelse(FC < -0.3785 & pvalue > 1.3, "green", ifelse(FC > 0.3785 & pvalue > 1.3, "red", "grey"))), alpha = 0.7) +
  scale_color_identity() +
  geom_text_repel(data = subset(Mice_als_volcano_plots, (FC < -0.3785 & pvalue > 1.3) | (FC > 0.3785 & pvalue > 1.3)),
                  aes(label = `Gene Name`), size = 3, family = "bold",
                  box.padding = 1, point.padding = 1) +  # Adjust box.padding and point.padding
  scale_color_manual(values = c("green" = "green", "red" = "red", "grey" = "grey"),
                     labels = c("Downregulated in ALS", "Upregulated in ALS", "Not Significant"),
                     name = "Category") +
  geom_hline(yintercept = 1.3, linetype = "dotted") +  # Add a horizontal line at y = 1.3
  geom_vline(xintercept = 0.3785, linetype = "dotted") +  # Add a vertical line
  geom_vline(xintercept = -0.3785, linetype = "dotted") +  # Add a vertical line
  labs(x = "Log2 Fold Change", y = "-Log10 p-Value", title = "Total ALS vs WT") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),  # Make title bold
        axis.title = element_text(face = "bold"),  # Make axis titles bold
        legend.title = element_text(face = "bold"))  # Make legend title bold



#mals vs fals
#Mice_als_volcano_plots <- read_excel("~/Downloads/mito als/Volcano plots/Mice als volcano plots.xlsx", 
#                                     sheet = "mals vs fals")


ggplot(Mice_als_volcano_plots, aes(x = FC, y = pvalue)) +
  geom_point(aes(color = ifelse(FC < -0.3785 & pvalue > 1.3, "hotpink", ifelse(FC > 0.3785 & pvalue > 1.3, "dodgerblue", "grey"))), alpha = 0.7) +
  scale_color_identity() +
  geom_text_repel(data = subset(Mice_als_volcano_plots, (FC < -0.3785 & pvalue > 1.3) | (FC > 0.3785 & pvalue > 1.3)),
                  aes(label = Gene), size = 3, family = "bold",
                  box.padding = 1, point.padding = 1) +
  scale_color_manual(values = c("hotpink" = "hotpink", "dodgerblue" = "dodgerblue", "grey" = "grey"),
                     labels = c("Upregulated in fALS", "Upregulated in mALS", "Not Significant"),
                     name = "Category") +
  geom_hline(yintercept = 1.3, linetype = "dotted") +  # Add a horizontal line at y = 1.3
  geom_vline(xintercept = 0.3785, linetype = "dotted") +  # Add a vertical line
  geom_vline(xintercept = -0.3785, linetype = "dotted") +  # Add a vertical line
  labs(x = "Log2 Fold Change", y = "-Log10 p-Value", title = "mALS vs fALS") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),  # Make title bold
        axis.title = element_text(face = "bold"),  # Make axis titles bold
        legend.title = element_text(face = "bold"))  
