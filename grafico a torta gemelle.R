library(ggplot2)
library(tidyverse)
library(dplyr)
library(patchwork)

#METABOLISMS
#Glycolysis and gluconeogenesis
dataGlycolysis <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(0, 11, 16)
)
dataGlycolysis$percent <- dataGlycolysis$count / sum(dataGlycolysis$count) * 100

# Definizione dei colori per ciascuna categoria
colors <- c("Up in TARDBP-d" = "red", "Down in TARDBP-d" = "green", "Not Significant" = "grey")

# Creazione del grafico a torta con ggplot2
Glycolysis <- ggplot(dataGlycolysis, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Glycolysis and Gluconeogenesis")

#Fatty acid beta-oxidation
dataFAO <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(3, 2, 16)
)
dataFAO$percent <- dataFAO$count / sum(dataFAO$count) * 100


# Creazione del grafico a torta con ggplot2
FAO <- ggplot(dataFAO, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Fatty Acid Beta-Oxidation")
print(FAO)

#Oxidative phosphorylation
dataoxphos <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(4, 15, 15)
)
dataoxphos$percent <- dataoxphos$count / sum(dataoxphos$count) * 100


 # Creazione del grafico a torta con ggplot2
oxphos <- ggplot(dataoxphos, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Oxidative Phosphorylation")
print(oxphos)

#Citrate cycle (TCA cycle)
datatca <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(2, 9, 15)
)
datatca$percent <- datatca$count / sum(datatca$count) * 100

# Creazione del grafico a torta con ggplot2
TCA <- ggplot(datatca, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Citrate cycle (TCA cycle)")
print(TCA)

((Glycolysis | FAO) /(TCA | oxphos))+
  plot_annotation(
    tag_levels = "A", 
    title = "Metabolisms",
    theme = theme(
      plot.title = element_text(face = "bold")
    )
  )

#sulfur metabolism
datasulf <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(2, 2, 5)
)
datasulf$percent <- datasulf$count / sum(datasulf$count) * 100

# Creazione del grafico a torta con ggplot2
sulfurmet <- ggplot(datasulf, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Sulfur Metabolism")
print(sulfurmet)

#Glutathione metabolism 
datagluth <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(6, 5, 18)
)
datagluth$percent <- datagluth$count / sum(datagluth$count) * 100

# Creazione del grafico a torta con ggplot2
gluthmet <- ggplot(datagluth, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Glutathione Metabolism")
print(gluthmet)


#Macroautophagy
datamacroau <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(7, 20, 27)
)
datamacroau$percent <- datamacroau$count / sum(datamacroau$count) * 100

# Creazione del grafico a torta con ggplot2
macroautophagy <- ggplot(datamacroau, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Macroautophagy")
print(macroautophagy)

#Post-translational protein modification
dataPTM <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(78, 101, 248)
)
dataPTM$percent <- dataPTM$count / sum(dataPTM$count) * 100

# Creazione del grafico a torta con ggplot2
PTM <- ggplot(dataPTM, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Post-translational Protein Modification")
print(PTM)

#Immune System
dataimmune <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(94, 143, 305)
)
dataimmune$percent <- dataimmune$count / sum(dataimmune$count) * 100

# Creazione del grafico a torta con ggplot2
immune <- ggplot(dataimmune, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Immune System")
print(immune)

#Protein Folding
dataprot <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(3, 13, 28)
)
dataprot$percent <- dataprot$count / sum(dataprot$count) * 100


# Creazione del grafico a torta con ggplot2
prot <- ggplot(dataprot, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Protein Folding")
print(prot) 

#Membrane Trafficking

datamem <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(56, 64, 157)
)
datamem$percent <- datamem$count / sum(datamem$count) * 100


# Creazione del grafico a torta con ggplot2
mem <- ggplot(datamem, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Mebrane Trafficking")

print(mem)

#stress response
datastress <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(46, 110, 191)
)
datastress$percent <- datastress$count / sum(datastress$count) * 100
# Creazione del grafico a torta con percentuali esposte
ggplot(datastress, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() +
  labs(title = "Cellular Responses to Stress")

#dETOX OF ROS
dataros <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(2, 9, 24)
)
dataros$percent <- dataros$count / sum(dataros$count) * 100

ros <- ggplot(dataros, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  ggtitle("Detoxification of Reactive Oxygen Species")+
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() 
print(ros)


###traff

datatraff <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(56, 87, 134)
)
datatraff$percent <- datatraff$count / sum(datatraff$count) * 100

traff <- ggplot(datatraff, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  ggtitle("Membrane Trafficking")+
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() 
print(traff)



dataapop <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(14, 26, 55)
)
dataapop$percent <- dataapop$count / sum(dataapop$count) * 100

apop <- ggplot(dataapop, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  ggtitle("Apoptosis")+
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() 
print(apop)

####RNA MEtabolis##
dataRNA <- data.frame(
  Category = c("Up in TARDBP-d", "Down in TARDBP-d", "Not Significant"),
  count = c(50, 92, 234)
)
dataRNA$percent <- dataRNA$count / sum(dataRNA$count) * 100
RNA <- ggplot(dataRNA, aes(x = "", y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  ggtitle("RNA Metabolism")+
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 5)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +  # Specifica i colori manualmente
  theme_void() 
print(RNA)



immune + PTM + RNA
