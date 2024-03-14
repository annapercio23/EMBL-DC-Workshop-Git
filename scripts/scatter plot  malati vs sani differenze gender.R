library(tidyverse)
library(ggplot2)
library(tidyr)
#dna methilation
Dnamet <- up_in_fals_vs_male_als [1:4,1:6] %>% 
  na.omit() 
Dnamet2 <- Dnamet [1:4,c(1,3:5)] %>% 
  rename(`mALS vs fALS` = `mals vs fals`, `mWT vs fWT` = `mwt vs fwt`)

Dna_long <- pivot_longer(Dnamet2, cols = -Gene, names_to = "Group", values_to = "Value")

Dnamethilation <- Dna_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "mALS vs fALS" = "green3", "mWT vs fWT" = "cyan")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "mALS vs fALS" = "green3", "mWT vs fWT" = "cyan")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(size = 2) +  # Used filter instead of subset
  ggtitle("DNA Methylation") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("ALS vs WT" = "darkorange1", "mALS vs fALS" = "green3", "mWT vs fWT" = "cyan")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"),panel.grid = element_blank())

# TCA
TCA <- up_in_fals_vs_male_als [1:8,1:6] %>% 
  na.omit() 
TCA2 <- TCA [1:8,c(1,3:5)] %>% 
  rename(`mALS vs fALS` = `mals vs fals`, `mWT vs fWT` = `mwt vs fwt`)

TCA_long < - pivot_longer(TCA2, cols = -Gene, names_to = "Group", values_to = "Value")

TCA <- TCA_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "mALS vs fALS" = "green3", "mWT vs fWT" = "cyan")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "mALS vs fALS" = "green3", "mWT vs fWT" = "cyan")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(size = 2) +  # Used filter instead of subset
  ggtitle("TCA") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("ALS vs WT" = "darkorange1", "mALS vs fALS" = "green3", "mWT vs fWT" = "cyan")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"),panel.grid = element_blank())


#detox
detox <- up_in_fals_vs_male_als [1:2,1:6] %>% 
  na.omit() 
detox2 <- detox [1:8,c(1,3:5)] %>% 
  rename(`mALS vs fALS` = `mals vs fals`, `mWT vs fWT` = `mwt vs fwt`)

detox_long <- pivot_longer(detox2, cols = -Gene, names_to = "Group", values_to = "Value")

detox <- detox_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "mALS vs fALS" = "green3", "mWT vs fWT" = "cyan")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "mALS vs fALS" = "green3", "mWT vs fWT" = "cyan")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(size = 2) +  # Used filter instead of subset
  ggtitle("Detoxification of ROS") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("ALS vs WT" = "darkorange1", "mALS vs fALS" = "green3", "mWT vs fWT" = "cyan")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"),panel.grid = element_blank())



