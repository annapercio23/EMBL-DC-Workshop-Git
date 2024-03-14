library(tidyverse)
library(ggplot2)
library(tidyr)
#stress ox
todos_long <- pivot_longer(Confronto_tra_i_total_e_i_gender, cols = -Gene, names_to = "Group", values_to = "Value")


todos_long %>%
  ggplot(aes(x = Gene, y = Value, fill = Group)) +  # Use fill aesthetic for bar fill
  geom_bar(stat = "identity", position = "dodge") +  # Use geom_bar with stat = "identity" for bar plot
  scale_fill_manual(values = c("total ALS vs WT" = "darkseagreen1", "elution ALS vs WT" = "dodgerblue"),
                    labels = c("Total", "Gender")) + 
  theme_bw() +
  geom_hline(yintercept =0, alpha = 0.5) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1),  # Rotate x-axis text by 45 degrees and align to the right
        axis.text.y = element_text(face = "bold"), panel.grid = element_blank())


#concordi
#Confronto_tra_i_total_e_i_gender <- read_excel("~/Downloads/mito als/TOTAL/Confronto tra i total e i gender.xlsx", 
  #                                             +     sheet = "concordi", col_types = c("text", 
   #                                                                                       +         "numeric", "numeric", "skip", "skip", 
   #                                                                                       +         "skip"))
concordi <- pivot_longer(Confronto_tra_i_total_e_i_gender, cols = -Gene, names_to = "Group", values_to = "Value")


concordi %>%
  ggplot(aes(x = Gene, y = Value, fill = Group)) +  # Use fill aesthetic for bar fill
  geom_bar(stat = "identity", position = "dodge") +  # Use geom_bar with stat = "identity" for bar plot
  scale_fill_manual(values = c("total ALS vs WT" = "darkseagreen1", "elution ALS vs WT" = "dodgerblue"),
                    labels = c("Total", "Gender")) + 
  theme_bw() +
  geom_hline(yintercept =0, alpha = 0.5) +
  ggtitle("Concordi") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1),  # Rotate x-axis text by 45 degrees and align to the right
        axis.text.y = element_text(face = "bold"), panel.grid = element_blank())

  
#disconcordi
#Confronto_tra_i_total_e_i_gender <- read_excel("~/Downloads/mito als/TOTAL/Confronto tra i total e i gender.xlsx", 
                                                # +     sheet = "discordanti", col_types = c("text", 
   ##                                                                                         +         "numeric", "numeric", "skip", "skip", 
    #                                                                                        +         "skip"))
discordi <- pivot_longer(Confronto_tra_i_total_e_i_gender, cols = -Gene, names_to = "Group", values_to = "Value")


discordi %>%
  ggplot(aes(x = Gene, y = Value, fill = Group)) +  # Use fill aesthetic for bar fill
  geom_bar(stat = "identity", position = "dodge") +  # Use geom_bar with stat = "identity" for bar plot
  scale_fill_manual(values = c("total ALS vs WT" = "darkseagreen1", "elution ALS vs WT" = "dodgerblue"),
                    labels = c("Total", "Gender")) + 
  theme_bw() +
  geom_hline(yintercept =0, alpha = 0.5) +
  ggtitle("Discordi") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1),  # Rotate x-axis text by 45 degrees and align to the right
        axis.text.y = element_text(face = "bold"), panel.grid = element_blank())
