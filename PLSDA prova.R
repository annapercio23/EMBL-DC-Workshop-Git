library(mixOmics)
library(ggplot2)
library(mixOmics)
library(tidyverse)

# Lettura dei dati dal file CSV
df <- COMUNI_a_tutte_le_6_coppie %>% 
  na.omit()
df <- df[, -1]
# Trasposizione del dataframe
data <- t(df)

# Conversione in matrice numerica
data <- as.matrix(data)

class <- factor(c(rep("ALS", 6), rep("CTRL", 6)))
row.names(COMUNI_a_tutte_le_6_coppie$Gene)
# Esecuzione della PLS-DA
plsda_result <- plsda(data, class, ncomp = 2)

# Plot dei campioni nei primi due componenti con mixOmics
plotIndiv(plsda_result, comp = 1:2, group = class, ellipse = TRUE, legend = TRUE)

# Visualizzazione con ggplot2
scores <- as.data.frame(plsda_result$variates$X)
lol <-  as.data.frame(plsda_result$prop_expl_var$Y) #i modelli sono spiegati sulla covarianza 
scores$class <- class

ggplot(scores, aes(x = comp1, y = comp2, color = class)) +
  geom_point() +
  labs(title = "PLS-DA: component 1 vs component 2",
       x = "Component 1",
       y = "Component 2")

kk <- as.data.frame(plsda_result[["mat.c"]])
###########################################################################
X <- data
Y <- class
plsda.results <- plsda(X,Y, ncomp = 2)

# Iterazione del modello per la scelta delle ncomp
perf.plsda.results <- perf(plsda.results, validation = 'Mfold', folds = 5, #Mfold O LOO
                           progressBar = TRUE,  # Set to TRUE to track progress
                           nrepeat = 10)         # suggest nrepeat = 50
legend_size <- 0.7 
par(cex = legend_size)
plot(perf.plsda.results, sd = TRUE,
     legend.position = "horizontal",legend=TRUE)
perf.plsda.results$choice.ncomp #ideal number of ncomp

# Modello PLS-DA con l'ncomp scelto
final.plsda.results <- plsda(X,Y, ncomp = 2)

plotIndiv(final.plsda.results, ind.names = FALSE, 
          pch = c(17, 17), cex = c(2,2), #16 15 17
          col = c("blue","red3"),   #'darkorchid'
          legend=TRUE,  
          comp=c(1,2), ellipse = F, 
          group = Class_names,
          title = 'PLS-DA comp 1-2',
          X.label = 'PLS-DA comp 1', Y.label = 'PLS-DA comp 2')
# Valutazion dei parametri
print(final.plsda.results$prop_expl_var$X*100)
print(final.plsda.results$prop_expl_var$Y*100)
par(mfrow=c(1,2))
barplot(final.plsda.results$prop_expl_var$Y * 100,
        main = "Varianza Spiegata per Y",
        xlab = "Componenti",
        ylab = "Percentuale di Varianza Spiegata")
par(mfrow=c(1,1))

# Iterazione del modello per testarne la bonta``
perf.final.plsda.results <- perf(final.plsda.results, validation = 'Mfold', 
                                 folds = 12, 
                                 progressBar = TRUE, # TRUE to track progress
                                 nrepeat = 10) # we recommend 50 
perf.final.plsda.results$error.rate$BER[, 'max.dist'] #performance of each component
perf.final.plsda.results$error.rate.class$max.dist #error rate per class
plot(perf.final.plsda.results, sd = TRUE,
     legend.position = "horizontal",legend=TRUE)
# Indici VIP
VIP <- vip(final.plsda.results)
print(VIP)
vipPC1 <- VIP[, 1]
VIPPC1ordine <- sort(vipPC1, decreasing = TRUE)
print(VIPPC1ordine)
VIPsignificative <- VIPPC1ordine[VIPPC1ordine >= 0.8]
print(VIPsignificative)
gg <- as.data.frame(VIPsignificative)
#PLOT LOADINGS per Y
par(mfrow=c(1,3))
#par(mfrow=c(2,2))
barplot(final.plsda.results$loadings$Y[,1],
        main = "Loadings su comp1",
        xlab = "Classi",
        ylab = "Loadings") #per la classe 1
barplot(final.plsda.results$loadings$Y[,2],
        main = "Loadings su comp2",
        xlab = "Classi",
        ylab = "Loadings") #per la classe 3
barplot(final.plsda.results$loadings$Y[,3],
        main = "Loadings su comp3",
        xlab = "Classi",
        ylab = "Loadings") #per la classe 4
barplot(final.plsda.results$loadings$Y[,4],
        main = "Loadings su comp4",
        xlab = "Classi",
        ylab = "Loadings") #per la classe 2
par(mfrow=c(1,2))
print(final.plsda.results$loadings$Y)
# scores on LV
scores <- final.plsda.results$variates$X
# Define colors for each class
class_colors <- c("CT" = "blue", "PD"="red3")  # Replace with appropriate colors
# Create barplot and color bars according to class
barplot(scores[,1], col = class_colors[class], 
        xlab = "Sample", ylab = "Score on LV1", main = "Scores on LV1",
        names.arg = 1:length(scores[,1]))
barplot(scores[,2], col = class_colors[class], 
        xlab = "Sample", ylab = "Score on LV1", main = "Scores on LV2",
        names.arg = 1:length(scores[,2]))
barplot(scores[,3], col = class_colors[Class_names], 
        xlab = "Sample", ylab = "Score on LV3", main = "Scores on LV3",
        names.arg = 1:length(scores[,3]))

par(mfrow=c(1,1))
plotLoadings(final.plsda.results, comp = 1, method = 'mean', contrib = 'max')
plotLoadings(final.plsda.results, comp = 2, method = 'mean', contrib = 'max')
plotLoadings(final.plsda.results, comp = 3, method = 'mean', contrib = 'max')
par(mfrow=c(1,1))  


