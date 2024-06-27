library(ggplot2)
library(tidyverse)
library(readxl)
library(scales)
library(dplyr)
#######Reactome pathway sod vs ctrl######
ALS_gemelle_reactome_solo_sign_geni <- read_excel("~/Downloads/gemelle/ALS gemelle reactome solo sign geni.xlsx", 
                                                      sheet = "Pathways")

ALS_gemelle_reactome_solo_sign_geni <- ALS_gemelle_reactome_solo_sign_geni %>%
  mutate(logFDR = -log10(FDR))

ALS_gemelle_reactome_solo_sign_geni %>% 
  ggplot(aes(x = Mean, y = Pathway, size = Count, color = logFDR)) +
  geom_point() +
  geom_vline(xintercept = 0, alpha = 0.5, linetype = "dashed") +
  scale_size_continuous(range = c(3, 15)) +  
  scale_color_gradient(low = "blue", high = "red", name = "-log10(FDR)") +  
  labs(x = "Mean Ratio TARDBP-d vs TARDBP-h", y = "Reactome Pathway Name", 
       title = "Reactome Pathway Analysis - TARDBP TWINS") +
  facet_wrap(~ Category, scales = "free_y") +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 14, face="bold"),  # Modify facet titles
        plot.title = element_text(size = 16, face = "bold")) 

####barre#####

ALS_gemelle_reactome_solo_sign_geni %>% 
  ggplot(aes(x = -log10(pValue), y = reorder(Pathway, -log10(pValue)), fill = -log10(pValue))) +
  geom_bar(stat = "identity", 
           width = 0.5, 
           color = "gray50") +
  ggtitle("Top Reactome Pathways") +
  labs(x= "-log10 p-Value", y= "Reactome Pathway Name")+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(plot.title = element_text(face = "bold"), 
        panel.grid = element_blank(),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))
###GO#######
ClueGOResultTable_0_gemelle_tutte_kegg_reactome_e_wiki_path <- read_excel("~/Downloads/gemelle/ClueGOResultTable-0 gemelle tutte kegg reactome e wiki path.xlsx", 
                                                                          sheet = "Foglio1")

ClueGOResultTable_0_gemelle_tutte_kegg_reactome_e_wiki_path <- ClueGOResultTable_0_gemelle_tutte_kegg_reactome_e_wiki_path %>%
  mutate(logpValue = -log10(pValue))

ClueGOResultTable_0_gemelle_tutte_kegg_reactome_e_wiki_path %>% 
  ggplot(aes(x = Mean, y = Pathway, size = Count, color = logpValue)) +
  geom_point() +
  geom_vline(xintercept = 0, alpha = 0.5, linetype = "dashed") +
  scale_size_continuous(range = c(3, 15)) +  
  scale_color_gradient(low = "blue", high = "red", name = "-log10(pValue)") +  
  labs(x = "Mean Ratio TARDBP-d vs TARDBP-h", y = "Pathway Name", 
       title = "ClueGO Pathway Analysis - TARDBP TWINS") +
  facet_wrap(~ Category, scales = "free_y") +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 10, face="bold"),  # Modify facet titles
        plot.title = element_text(size = 10, face = "bold")) 

####barre#####
ClueGOResultTable_0_gemelle_tutte_kegg_reactome_e_wiki_path <- read_excel("~/Downloads/gemelle/ClueGOResultTable-0 gemelle tutte kegg reactome e wiki path.xlsx", 
                                                                              sheet = "Foglio1")


ClueGOResultTable_0_gemelle_tutte_kegg_reactome_e_wiki_path %>% 
  ggplot(aes(x = -log10(pValue), y = reorder(Pathway, -log10(pValue)), fill = -log10(pValue))) +
  geom_bar(stat = "identity", 
           width = 0.5, 
           color = "gray50") +
  ggtitle("Top ClueGO Pathways") +
  labs(x= "-log10 p-Value", y= "Pathway Name")+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(plot.title = element_text(face = "bold"), 
        panel.grid = element_blank(),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))

#############GRAFICI A BARRE DEI PATHWAY########
library(tidyverse)
library(ggplot2)
library(ggpubr)
#insieme
Insieme <- read_excel("~/Downloads/gemelle/ClueGOResultTable-0 gemelle tutte kegg reactome e wiki path.xlsx", 
                          sheet = "metabolisms")

Insieme <- Insieme %>%
  mutate(Gene = factor(Gene, levels = unique(Gene)))
Insieme$Metabolism <- factor(Insieme$Metabolism, levels = c("Carbohydrate Metabolism", "Fatty Acid Metabolism", "Amino Acid Metabolism","Tricarboxylic Acid Cycle"))

mean_values <- Insieme %>%
  group_by(Metabolism) %>%
  summarise(mean_value = mean(Ratio))
ins <- Insieme %>%
  ggplot(aes(x = Gene, y = Ratio, fill = Metabolism)) +
  geom_area(aes(group = Metabolism), alpha = 0.5) + 
  geom_bar(stat = "identity", width = 0.5, color = "gray50") +
  scale_fill_manual(values = c("Amino Acid Metabolism" = "orange", "Carbohydrate Metabolism" = "steelblue4", "Fatty Acid Metabolism" = "forestgreen", "Tricarboxylic Acid Cycle" = "purple")) +
  ggtitle("Metabolisms") +
  labs(x= "Gene Name", y="Ratio TARDBP-d vs TARDBP-h") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(face = "bold", size = 10),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(ins)

ETC <- read_excel("~/Downloads/gemelle/ClueGOResultTable-0 gemelle tutte kegg reactome e wiki path.xlsx", 
                      sheet = "Oxidative phosphorylation", 
                       range = "a46:f64")

ETC$Type <- factor(ETC$Complex)

ETC <- ETC %>%
  mutate(Name = factor(Gene, levels = unique(Gene))) # Convert Name to factor with specified levels

library(ggplot2)
library(dplyr)

# Ensure the factors are in the correct order
ETC <- ETC %>%
  mutate(Name = factor(Name),
         Type = factor(Type, levels = c("Complex I", "Complex II", "Complex III", "Complex IV"))) 

# Create the plot
gg2 <- ggplot(ETC, aes(x = Name, y = FC, fill = Type)) +
  geom_area(aes(group = Type), alpha = 0.5) + 
  geom_bar(stat = "identity", width = 0.5, color = "grey50") +
  ggtitle("Electron Transport Chain") +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("Complex I" = "steelblue4", "Complex II" = "steelblue2", "Complex III" = "slategray2", "Complex IV" = "lightblue"),
                    breaks = c("Complex I", "Complex II", "Complex III", "Complex IV"),  # Ensure legend order
                    labels = c("Complex I", "Complex II", "Complex III", "Complex IV")) +
  labs(fill = "Complex", x = "Gene Name", y="Ratio TARDBP-d vs TARDBP-h") +   # Change legend title
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 10),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

print(gg2)


library(patchwork)
((ins) / (gg2)) +
  plot_annotation(tag_levels = "a") +
  plot_layout(widths = c(1, 1), heights = c(1, 1))


#####BARRE######

#####Apoptosis ###
apop <- read_excel("~/Downloads/gemelle/tabelle mariagrazia gemelle tdo43 modf.xlsx", 
                        sheet = "stress response", range = "a2:e37")

library(dplyr)
library(forcats)

# Supponiamo che il dataset apop sia già caricato
# Riordina i livelli del fattore Gene in base ai valori di Ratio
apop <- apop %>%
  mutate(Gene = fct_reorder(Gene, Ratio))

# Crea il grafico
b <- ggplot(apop, aes(x = Gene, y = Ratio)) +
  geom_bar(stat = "identity", fill ="navyblue", width = 0.5, color = "black") +
  ggtitle("Apoptosis") +
  labs(x = "Gene Name", y="Ratio TARDBP-d vs TARDBP-h") +
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
print(b)
####Mitochondrial prot degradatiomn###
mito <- read_excel("~/Downloads/gemelle/tabelle mariagrazia gemelle tdo43 modf.xlsx", 
                   sheet = "stress response", range = "a39:e65")

mito <- mito %>%
  mutate(Gene = fct_reorder(Gene, Ratio))

# Crea il grafico
c <- ggplot(mito, aes(x = Gene, y = Ratio)) +
  geom_bar(stat = "identity", fill ="navyblue", width = 0.5, color = "black") +
  ggtitle("Mitochondrial Protein Degradation") +
  labs(x = "Gene Name", y="Ratio TARDBP-d vs TARDBP-h") +
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
print(c)
#####MEMBR TRAFF##
mem <- read_excel("~/Downloads/gemelle/tabelle mariagrazia gemelle tdo43 modf.xlsx", 
                  sheet = "stress response", range = "a67:e183")

mem <- mem %>%
  mutate(Gene = fct_reorder(Gene, Ratio))

# Crea il grafico
d <- ggplot(mem, aes(x = Gene, y = Ratio)) +
  geom_bar(stat = "identity", fill ="navyblue", width = 0.5, color = "black") +
  ggtitle("Membrane Trafficking") +
  labs(x = "Gene Name", y="Ratio TARDBP-d vs TARDBP-h") +
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
print(d)

####Macroautophagy#####
macro <- read_excel("~/Downloads/gemelle/tabelle mariagrazia gemelle tdo43 modf.xlsx", 
                  sheet = "stress response", range = "a185:e211")

macro <- macro %>%
  mutate(Gene = fct_reorder(Gene, Ratio))

# Crea il grafico
e <- ggplot(macro, aes(x = Gene, y = Ratio)) +
  geom_bar(stat = "identity", fill ="navyblue", width = 0.5, color = "black") +
  ggtitle("Macroautophagy") +
  labs(x = "Gene Name", y = "Ratio TARDBP-d vs TARDBP-h") +
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
print(e)

###detox ros###
ros <- read_excel("~/Downloads/gemelle/tabelle mariagrazia gemelle tdo43 modf.xlsx", 
                  sheet = "stress response", range = "a213:e226")

ros <- ros %>%
  mutate(Gene = fct_reorder(Gene, Ratio))

# Crea il grafico
f <- ggplot(ros, aes(x = Gene, y = Ratio)) +
  geom_bar(stat = "identity", fill ="navyblue", width = 0.5, color = "black") +
  ggtitle("Detoxification of ROS") +
  labs(x = "Gene Name", y = "Ratio TARDBP-d vs TARDBP-h") +
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
print(f)
#########PROT FOld°°°°°°àààààà
fold <- read_excel("~/Downloads/gemelle/tabelle mariagrazia gemelle tdo43 modf.xlsx", 
                  sheet = "stress response", range = "a228:e242")

fold <- fold %>%
  mutate(Gene = fct_reorder(Gene, Ratio))

# Crea il grafico
g <- ggplot(fold, aes(x = Gene, y = Ratio)) +
  geom_bar(stat = "identity", fill ="navyblue", width = 0.5, color = "black") +
  ggtitle("Protein Folding") +
  labs(x = "Gene Name", y = "Ratio TARDBP-d vs TARDBP-h") +
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

print(g)

library(patchwork)
((g|c)/(b)/(e|f)) +
  plot_annotation(tag_levels = "a") 


##RNA METABOLISM
rna <- read_excel("~/Downloads/gemelle/tabelle mariagrazia gemelle tdo43 modf.xlsx", 
                                sheet = "met RNA", range = "a2:e154")
rna <- rna %>%
  mutate(Gene = fct_reorder(Gene, Ratio))

# Crea il grafico
rna %>% ggplot(aes(x = Gene, y = Ratio)) +
  geom_bar(stat = "identity", fill ="navyblue", width = 0.5, color = "black") +
  ggtitle("RNA Metabolism") +
  labs(x = "Gene Name", y = "Ratio TARDBP-d vs TARDBP-h") +
  geom_hline(yintercept = 0) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.ticks.x = element_line(size = 0.5),
    axis.ticks.y = element_line(size = 0.5),
    axis.text = element_text(size = 8, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

###GSH##
GSH <- read_excel("~/Downloads/gemelle/ClueGOResultTable-0 gemelle tutte kegg reactome e wiki path.xlsx", 
                sheet = "Glutathione metabolism", range = "a38:e50")

GSH <- GSH %>%
  mutate(Gene = fct_reorder(Gene, Ratio))

#GSH %>% ggplot(aes(x = Gene, y = Ratio, fill = ifelse(Ratio < -1.5, "green", ifelse(Ratio > 1.5, "red", "cadetblue3")))) +
#  geom_bar(stat = "identity", width = 0.5, color = "gray50") +
 
GSH %>% ggplot(aes(x = Gene, y = Ratio)) +
  geom_bar(stat = "identity", fill ="navyblue", width = 0.5, color = "black") +
 ggtitle("Glutathione Metabolism") +
  labs(x = "Gene Name", y = "Ratio TARDBP-d vs TARDBP-h") +
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
  ) +
  scale_fill_identity()


