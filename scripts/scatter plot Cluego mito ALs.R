library(tidyverse)
library(ggplot2)
library(tidyr)
#stress ox
stressox <- TUTTI_da_office_online_ClueGOResultTable_0 [1:19,1:8] %>% 
  na.omit()
stressox2 <- stressox [1:14,c(1,3:5)]  

stressox_long <- pivot_longer(stressox2, cols = -Gene, names_to = "Group", values_to = "Value")

Ox <- stressox_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(data = filter(stressox_long, Group == "ALS vs WT"), size = 2) +  # Used filter instead of subset
  ggtitle("Oxidative Stress and Redox Pathway") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("darkorange1", "pink", "dodgerblue")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"),panel.grid = element_blank())

#tca cycle

TCA <- TUTTI_da_office_online_ClueGOResultTable_0 [1:17,1:5] %>% 
  na.omit()
TCA2 <- TCA [1:17,c(1,3:5)]  

TCA_long <- pivot_longer(TCA2, cols = -Gene, names_to = "Group", values_to = "Value")

TCA <- TCA_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(data = filter(TCA_long, Group == "ALS vs WT"), size = 2) +  # Used filter instead of subset
  ggtitle("Citrate Cycle (TCA cycle)") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("darkorange1", "pink", "dodgerblue")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"),panel.grid = element_blank())

#unfolded prot binding
unf <- TUTTI_da_office_online_ClueGOResultTable_0 [1:12,1:5] %>% 
  na.omit()
unf2 <- unf [1:10,c(1,3:5)]  

unf_long <- pivot_longer(unf2, cols = -Gene, names_to = "Group", values_to = "Value")

unf <- unf_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(data = filter(unf_long, Group == "ALS vs WT"), size = 2) +  # Used filter instead of subset
  ggtitle("Unfolded Protein Binding") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("darkorange1", "pink", "dodgerblue")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"),panel.grid = element_blank())

#cys and met
cys <- TUTTI_da_office_online_ClueGOResultTable_0 [1:9,1:5] %>% 
  na.omit()
cys2 <- cys [1:7,c(1,3:5)]  

cys_long <- pivot_longer(cys2, cols = -Gene, names_to = "Group", values_to = "Value")

cys <- cys_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(data = filter(cys_long, Group == "ALS vs WT"), size = 2) +  # Used filter instead of subset
  ggtitle("Cysteine and Methionine Metabolism") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("darkorange1", "pink", "dodgerblue")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"),panel.grid = element_blank())

#axonal growth inhibition
ax <- TUTTI_da_office_online_ClueGOResultTable_0 [1:5,1:5] %>% 
  na.omit()
ax2 <- ax [1:5,c(1,3:5)]  

ax_long <- pivot_longer(ax2, cols = -Gene, names_to = "Group", values_to = "Value")

ax <- ax_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(data = filter(ax_long, Group == "ALS vs WT"), size = 2) +  # Used filter instead of subset
  ggtitle("Axonal growth inhibition (RHOA activation)") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("darkorange1", "pink", "dodgerblue")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"), panel.grid = element_blank())

#actin
ac <- TUTTI_da_office_online_ClueGOResultTable_0 [1:16,1:5] %>% 
  na.omit()
ac2 <- ac [1:11,c(1,3:5)]  

ac_long <- pivot_longer(ac2, cols = -Gene, names_to = "Group", values_to = "Value")

ac <- ac_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(data = filter(ac_long, Group == "ALS vs WT"), size = 2) +  # Used filter instead of subset
  ggtitle("Regulation of actin cytoskeleton") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("darkorange1", "pink", "dodgerblue")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"))

ac <- TUTTI_da_office_online_ClueGOResultTable_0 [1:16,1:5] %>% 
  na.omit()
ac2 <- ac [1:11,c(1,3:5)]  

ac_long <- pivot_longer(ac2, cols = -Gene, names_to = "Group", values_to = "Value")

ac <- ac_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(data = filter(ac_long, Group == "ALS vs WT"), size = 2) +  # Used filter instead of subset
  ggtitle("Regulation of actin cytoskeleton") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("darkorange1", "pink", "dodgerblue")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"))

#macroautophagy

macro<- TUTTI_da_office_online_ClueGOResultTable_0 [1:16,1:5] %>% 
  na.omit()
macro2 <- macro [1:11,c(1,3:5)]  

macro_long <- pivot_longer(macro2, cols = -Gene, names_to = "Group", values_to = "Value")

macro <- macro_long %>%
  ggplot(aes(x = Gene, y = Value, color = Group)) +
  scale_color_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  scale_fill_manual(values = c("ALS vs WT" = "darkorange1", "fALS vs fWT" = "pink", "mALS vs mWT" = "dodgerblue")) +
  geom_line(aes(group = Group)) +  # Removed position aesthetic from geom_line
  geom_ribbon(aes(ymin = -2, ymax = 3, fill = Group)) +  # Removed position aesthetic
  geom_point(data = filter(macro_long, Group == "ALS vs WT"), size = 2) +  # Used filter instead of subset
  ggtitle("Mactoautophagy") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = c("darkorange1", "pink", "dodgerblue")))) + # Adjust legend box colors
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.text.y = element_text(face = "bold"), panel.grid = element_blank())

