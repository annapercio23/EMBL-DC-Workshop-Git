#Bar Plot per i twins 
library(tidyverse)
library(ggplot2)
library(readxl)
##########NEUTROPHIL DEGRANULATION##############
#bea vs bem
#reactome_bea_vs_bem_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome bea vs bem 1 e 5.xlsx", 
                                        sheet = "Neutrophil degranulation", range = "a4:e30")

a <- reactome_bea_vs_bem_1_e_5 %>% 
  na.omit()

a1 <- a %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Neutrophil Degranulation - BEM vs BEA") +
  labs(x = "Gene Name", y = "Ratio BEM vs BEA") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(a1)

#dagvsdal
#library(readxl)
#reactome_dag_vs_dal_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome dag vs dal 1 e 5.xlsx", 
                                        sheet = "Neutrophil degranulation", range = "a4:e50")
b <- reactome_dag_vs_dal_1_e_5 %>% 
  na.omit()

b1 <- b %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Neutrophil Degranulation - DAL vs DAG") +
  labs(x = "Gene Name", y = "Ratio DAL vs DAG") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(b1)
#ligvs lif
#Reactome_lig_vs_lif_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/Reactome lig vs lif 1 e 5.xlsx", 
                                        sheet = "Neutrophil degranulation", range = "a4:e50")

c <- Reactome_lig_vs_lif_1_e_5 %>% 
  na.omit()

c1 <- c %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Neutrophil Degranulation - LIG vs LIF") +
  labs(x = "Gene Name", y = "Ratio LIG vs LIF") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 9, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(c1)

#loavsloc
#reactome_loa_vs_loc_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome loa vs loc 1 e 5.xlsx", 
                                        sheet = "Neutrophil degranulation", range = "a4:e30")

d <- reactome_loa_vs_loc_1_e_5 %>% 
  na.omit()

d1 <- d %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Neutrophil Degranulation - LOA vs LOC") +
  labs(x = "Gene Name", y = "Ratio LOA vs LOC") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(d1)

#nuanud
#reactome_nud_vs_nua_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome nud vs nua 1 e 5.xlsx", 
                                        sheet = "Neutrophil degranulation", range = "a4:e50")


e <- reactome_nud_vs_nua_1_e_5 %>% 
  na.omit()

e1 <- e %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Neutrophil Degranulation - NUA vs NUD") +
  labs(x = "Gene Name", y = "Ratio NUA vs NUD") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(e1)

#PAMPAC
##reactome_pam_vas_pac_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome pam vas pac 1 e 5.xlsx", 
                                         sheet = "Neutrophil degranulation", range = "a4:E50")

f <- reactome_pam_vas_pac_1_e_5 %>% 
  na.omit()

f1 <- f %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Neutrophil Degranulation - PAC vs PAM") +
  labs(x = "Gene Name", y = "Ratio PAC vs PAM") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(f1)

library(patchwork)
(a1 + b1)/(c1 + d1)/ (e1 + f1) +
  plot_annotation(tag_levels = "a") 

(a1 + b1 + c1)/ (d1 + e1 + f1) +
  plot_annotation(tag_levels = "a") & 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) 

########STRESS RESPONSE############

#beavsbem
#non c'è

#dagvsdal
#library(readxl)
#reactome_dag_vs_dal_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome dag vs dal 1 e 5.xlsx", 
                                        sheet = "Cellular responses to stress", 
                                        range = "a4:e90")
b <- reactome_dag_vs_dal_1_e_5 %>% 
  na.omit()

b1 <- b %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Cellular Response to Stress - DAL vs DAG") +
  labs(x = "Gene Name", y = "Ratio DAL vs DAG") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(b1)
#ligvs lif
#Reactome_lig_vs_lif_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/Reactome lig vs lif 1 e 5.xlsx", 
                                        sheet = "Cellular responses to stress", 
                                        range = "a4:e120")

c <- Reactome_lig_vs_lif_1_e_5 %>% 
  na.omit()

c1 <- c %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Cellular Response to Stress - LIG vs LIF") +
  labs(x = "Gene Name", y = "Ratio LIG vs LIF") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 9, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(c1)

#loavsloc
#reactome_loa_vs_loc_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome loa vs loc 1 e 5.xlsx", 
                                        sheet = "Cellular responses to stress", 
                                        range = "a4:e50")
d <- reactome_loa_vs_loc_1_e_5 %>% 
  na.omit()

d1 <- d %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Cellular Response to Stress - LOA vs LOC") +
  labs(x = "Gene Name", y = "Ratio LOA vs LOC") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(d1)

#nuanud
#reactome_nud_vs_nua_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome nud vs nua 1 e 5.xlsx", 
                                        sheet = "Cellular responses to stress", 
                                        range = "a4:e50")

e <- reactome_nud_vs_nua_1_e_5 %>% 
  na.omit()

e1 <- e %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Cellular Response to Stress - NUA vs NUD") +
  labs(x = "Gene Name", y = "Ratio NUA vs NUD") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(e1)

#PAMPAC
#reactome_pam_vas_pac_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome pam vas pac 1 e 5.xlsx", 
                                         sheet = "Cellular responses to stress", 
                                         range = "a4:e90")

f <- reactome_pam_vas_pac_1_e_5 %>% 
  na.omit()

f1 <- f %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Cellular Response to Stress - PAC vs PAM") +
  labs(x = "Gene Name", y = "Ratio PAC vs PAM") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(f1)

library(patchwork)
(a1 + b1)/(c1 + d1)/ (e1 + f1) +
  plot_annotation(tag_levels = "a") 

(a1 + b1 + c1)/ (d1 + e1 + f1) +
  plot_annotation(tag_levels = "a") & 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) 
##############PROTEIN METABOLISM############################
#beavsbem
reactome_bea_vs_bem_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome bea vs bem 1 e 5.xlsx", 
                                        sheet = "Metabolism of proteins", range = "a4:e100")
a <- reactome_bea_vs_bem_1_e_5 %>% 
  na.omit()

a1 <- a %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Metabolism of Proteins - BEM vs BEA") +
  labs(x = "Gene Name", y = "Ratio BEM vs BEA") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(a1)

#dagvsdal
#library(readxl)
reactome_dag_vs_dal_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome dag vs dal 1 e 5.xlsx", 
sheet = "Metabolism of proteins", 
range = "a4:e90")
b <- reactome_dag_vs_dal_1_e_5 %>% 
  na.omit()

b1 <- b %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Metabolism of Proteins - DAL vs DAG") +
  labs(x = "Gene Name", y = "Ratio DAL vs DAG") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(b1)
#ligvs lif
Reactome_lig_vs_lif_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/Reactome lig vs lif 1 e 5.xlsx", 
                                        sheet = "Metabolism of amino acids and d", 
                                        range = "a4:e100")

c <- Reactome_lig_vs_lif_1_e_5 %>% 
  na.omit()

c1 <- c %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Metabolism of Proteins - LIG vs LIF") +
  labs(x = "Gene Name", y = "Ratio LIG vs LIF") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 9, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(c1)

#loavsloc
reactome_loa_vs_loc_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome loa vs loc 1 e 5.xlsx", 
sheet = "Metabolism of proteins", 
range = "a4:e50")
d <- reactome_loa_vs_loc_1_e_5 %>% 
  na.omit()

d1 <- d %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Metabolism of Proteins - LOA vs LOC") +
  labs(x = "Gene Name", y = "Ratio LOA vs LOC") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(d1)

#nuanud
reactome_nud_vs_nua_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome nud vs nua 1 e 5.xlsx", 
                                        sheet = "Metabolism of amino acids and d", 
                                        range = "a4:e100")

e <- reactome_nud_vs_nua_1_e_5 %>% 
  na.omit()

e1 <- e %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Metabolism of Proteins - NUA vs NUD") +
  labs(x = "Gene Name", y = "Ratio NUA vs NUD") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(e1)

#PAMPAC
reactome_pam_vas_pac_1_e_5 <- read_excel("Downloads/GEMELLI 060624/Reactome/significative 1 e 5/reactome pam vas pac 1 e 5.xlsx", 
                                         sheet = "Metabolism of amino acids and d", 
                                         range = "a4:e100")

f <- reactome_pam_vas_pac_1_e_5 %>% 
  na.omit()

f1 <- f %>%
  ggplot(aes(x = Gene, y = FC)) +
  geom_bar(stat = "identity", fill ="steelblue", width = 0.5,
           color = "gray50") +
  ggtitle("Metabolism of Proteins - PAC vs PAM") +
  labs(x = "Gene Name", y = "Ratio PAC vs PAM") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(f1)

library(patchwork)
(a1 + b1)/(c1 + d1)/ (e1 + f1) +
  plot_annotation(tag_levels = "a") 

(a1 + b1 + c1)/ (d1 + e1 + f1) +
  plot_annotation(tag_levels = "a") & 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) 




#####SOD1##############
tioredoxina_sod_fus_tDP43 <- read_excel("Downloads/GEMELLI 060624/excel gemelli/tioredoxina_sod_fus_tDP43.xlsx", 
                                        range = "a1:g2")
library(tidyr)
sod <- tioredoxina_sod_fus_tDP43 %>% 
  pivot_longer(
  cols = -Gene,           # Exclude the 'gene' column from pivoting
  names_to = "sample",    # Name of the new column that will contain the sample names
  values_to = "ratio"  # Name of the new column that will contain the expression values
)

sod1 <- sod %>%
  ggplot(aes(x = sample, y = ratio, fill = sample)) +
  geom_bar(stat = "identity", width = 0.5,
           color = "gray50") +
  ggtitle("SOD1") +
  labs(x = "Twins", y = "Ratio ALS vs CTRL") +
  scale_fill_brewer(palette = "Set4") +  
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(sod1)

#####FUS##############
tioredoxina_sod_fus_tDP43 <- read_excel("Downloads/GEMELLI 060624/excel gemelli/tioredoxina_sod_fus_tDP43.xlsx", 
                                        range = "a4:g5")
fus <- tioredoxina_sod_fus_tDP43 %>% 
  pivot_longer(
    cols = -Gene,           # Exclude the 'gene' column from pivoting
    names_to = "sample",    # Name of the new column that will contain the sample names
    values_to = "ratio"  # Name of the new column that will contain the expression values
  )

fus1 <- fus %>%
  ggplot(aes(x = sample, y = ratio, fill = sample)) +
  geom_bar(stat = "identity",  width = 0.5,
           color = "gray50") +
  ggtitle("FUS") +
  labs(x = "Twins", y = "Ratio ALS vs CTRL") +
  scale_fill_brewer(palette = "Set4") +  
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(fus1)

(sod1)/(fus1) +
  plot_annotation(tag_levels = "a") & 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) 


#####tioredoxin##############
tioredoxina_sod_fus_tDP43 <- read_excel("~/Downloads/GEMELLI 060624/excel gemelli/tioredoxina_sod_fus_tDP43.xlsx", 
                                        sheet = "tioredossina")
tio <- tioredoxina_sod_fus_tDP43 %>% 
  pivot_longer(
    cols = -Gene,           # Exclude the 'gene' column from pivoting
    names_to = "sample",    # Name of the new column that will contain the sample names
    values_to = "ratio"  # Name of the new column that will contain the expression values
  )


tio1 <- tio %>%
  ggplot(aes(x = Gene, y = ratio, fill = sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "gray50") +
  scale_fill_brewer(palette = "Set4") +  
  ggtitle("Thioredoxin-Related") +
  labs(x = "Gene", y = "Ratio ALS vs CTRL") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )
print(tio1)

#h2s

tioredoxina_sod_fus_tDP43 <- read_excel("~/Downloads/GEMELLI 060624/excel gemelli/tioredoxina_sod_fus_tDP43.xlsx", 
                                        sheet = "sulfide")

h2s <- tioredoxina_sod_fus_tDP43 %>% 
  pivot_longer(
    cols = -Gene,           # Exclude the 'gene' column from pivoting
    names_to = "sample",    # Name of the new column that will contain the sample names
    values_to = "ratio"  # Name of the new column that will contain the expression values
  )


h2 <- h2s %>%
  ggplot(aes(x = Gene, y = ratio, fill = sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "gray50") +
  scale_fill_brewer(palette = "Set4") +  
  ggtitle(expression("H"[2]*"S-Related")) + 
  labs(x = "Gene", y = "Ratio ALS vs CTRL") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )
print(h2)
####gsh########

gsh <- read_excel("~/Downloads/GEMELLI 060624/excel gemelli/tioredoxina_sod_fus_tDP43.xlsx", 
                  sheet = "GSH")

gsh1 <- gsh %>% 
  pivot_longer(
    cols = -Gene,           # Exclude the 'gene' column from pivoting
    names_to = "sample",    # Name of the new column that will contain the sample names
    values_to = "ratio"  # Name of the new column that will contain the expression values
  )


gsh2 <- gsh1 %>%
  ggplot(aes(x = Gene, y = ratio, fill = sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "gray50") +
  scale_fill_brewer(palette = "Set4") +  
  ggtitle("GSH Metabolism") +
  labs(x = "Gene", y = "Ratio ALS vs CTRL") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )
print(gsh2)



#################REDOX#########################################
redox <- read_excel("~/Downloads/GEMELLI 060624/excel gemelli/tioredoxina_sod_fus_tDP43.xlsx", 
                  sheet = "Redox")

redox1 <- redox %>% 
  pivot_longer(
    cols = -Gene,           # Exclude the 'gene' column from pivoting
    names_to = "sample",    # Name of the new column that will contain the sample names
    values_to = "ratio"  # Name of the new column that will contain the expression values
  )


redox2 <- redox1 %>%
  ggplot(aes(x = Gene, y = ratio, fill = sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, color = "gray50") +
  scale_fill_brewer(palette = "Set4") +  
  ggtitle("Response to Oxidative Stress") +
  labs(x = "Gene", y = "Ratio ALS vs CTRL") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )
print(redox2)
