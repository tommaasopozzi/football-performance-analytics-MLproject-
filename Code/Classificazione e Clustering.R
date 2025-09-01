#Caricamento delle librerie necessarie
library("FactoMineR")
library(factoextra)
library(ggcorrplot)
library(GGally)
library(randomForest)
library(gridExtra)
library(plotly)
library(caret)
library('e1071')
library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)
library(mclust)
library(Rmixmod)
library(flexmix)
library(car)
library(mlrMBO)
library(mlr)
library(DiceKriging)
library(mclust)
library(cluster)
library(clValid)
library(ggrepel)
library(fmsb)
library(EMCluster)

# Classification with PCA -----------------------------------------------------

calcio = read.csv('Dataset_imp.csv')[,-1]
str(calcio)

#Osservo le numerosità delle classi
calcio %>%
  group_by(Pos)%>%
  count()


num = calcio %>% #Seleziono solo le variabili numeriche di cui sono interessato fare variable selection 
  dplyr::select(-(Season_End_Year:Nation), -Born, -Pos, -DummyCamp)


# Pca eseguita sui dati standardizzati
str(num)
pca = princomp(num, cor=T) 

# Estraggo le componenti principali
principal_components <- pca$scores

# Varianza spiegata dalle componenti principali
explained_variance <- summary(pca)
fviz_eig(pca, addlabels = TRUE, choice = 'variance', barfill = 'darkorange',barcolor = 'black', xlab = '',ncp = 11)
#Seleziono solo le prime 11 in modo da spiegare l'85% della devianza


q <- 11
cumsum(pca$sdev^2/sum(pca$sdev^2))[1:q] 

# Le prime 11 componenti spiegano l'86% della varianza

selected_components <- principal_components[, 1:q]

sel_pca = calcio %>%
  dplyr::select(Season_End_Year:Born) %>%
  dplyr::select(-Age)
sel_pca = data.frame(sel_pca, selected_components)

corrplot(cor(selected_components)) #Ovviamente sono incorrelate tra loro
pca_classification = sel_pca
pca_classification$Pos = as.factor(calcio$Pos)


#Estraggo soltanto le variabili numeriche  
classif = pca_classification
cl_num = classif %>%
  dplyr::select(Pos, Comp.1:Comp.11)%>%
  mutate(Pos = factor(Pos))

#Ho estratto tutte le variabili numeriche e Pos 

#Applico il primo metodo: SVM

set.seed(123)
#Divido il dataset in training set e test set 

ixs = createDataPartition(cl_num$Pos, times = 1, 0.75)
training = cl_num[ixs$Resample1, ]
test = classif[-ixs$Resample1, ]


#Eseguo automl per la stima dei parametri ottimi

par.set = makeParamSet(
  makeDiscreteParam('kernel', values = c('linear')),
  makeNumericParam('cost', lower = -2, upper = 2, trafo = function(x) 10^x)
)

#Esce polynomiaal = 1 posso rifarla senza polynomial

#par.set = makeParamSet(
# makeDiscreteParam('kernel', values = c('linear', 'radial', 'sigmoid')),
#  makeNumericParam('cost', lower = -2, upper = 2, trafo = function(x) 10^x),
#  makeNumericParam('gamma', lower = -2, upper = 2, trafo = function(x) 10^x,
#                   requires = quote(kernel == 'radial' | kernel == 'sigmoid')),
#  makeNumericParam('coef0', lower = -2, upper = 2, trafo = function(x) 10^x, 
#                   requires = quote(kernel == 'sigmoid'))
#) #Ancora linear il migliore 


par.set = makeParamSet(
  makeDiscreteParam('kernel', values = c('linear', 'radial')),
  makeNumericParam('cost', lower = -2, upper = 2, trafo = function(x) 10^x),
  makeNumericParam('gamma', lower = -2, upper = 2, trafo = function(x) 10^x,
                   requires = quote(kernel == 'radial'))
)


ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iter = 100)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

tune.ctrl = makeTuneControlMBO(mbo.control = ctrl)

#Sto cercando i parametri ottimali per riuscere a fare classificazione attraverso SVM sulla position 
task = makeClassifTask(data = training, target = 'Pos')
run = tuneParams(makeLearner('classif.svm'), task, cv10, measures = acc, 
                 par.set = par.set, control = tune.ctrl, show.info = T) 

#Gamma piccolissimo, a questo punto conviene ottimizzare come lineare

#Tune result:
#Op. pars: kernel=linear; cost=0.239
#acc.test.mean=0.8695468

#Osservo come è andata l'ottimizzazione bayesiana
y = getOptPathY((run$opt.path))
plot(y, type = 'o', lwd = 3, col = 'green3')
lines(cummax(y), type = 'o', pch = 19, lwd = 3, col = 'blue')

#Stabilizzo il modello e controllo il generalisation error


ix = createFolds(y = training$Pos, k = 10, list = T)
trnACC.svm = valACC.svm = numeric(length(ix))

for(k in 1:length(ix)){
  trnFold.svm = training[-ix[[k]],]
  valFold.svm = training[ix[[k]], ]
  
  model.svm = svm(Pos~., data = trnFold.svm, scale = T,
                  type = 'C-classification', cost = 0.239, kernel =  'linear',)  
  
  cat('* Confusion Matrix on the training fold: \n')
  print(table(trnFold.svm$Pos, model.svm$fitted))
  trnACC.svm[k] = mean(trnFold.svm$Pos == model.svm$fitted)
  
  preds.svm = predict(model.svm, newdata = valFold.svm) 
  
  cat('+ Confusion matrix on the validation fold: \n')
  print(table(valFold.svm$Pos, preds.svm))
  valACC.svm[k] = mean(valFold.svm$Pos == preds.svm)
}

plot(trnACC.svm, type = 'o', pch = 19, lwd = 3, col = 'blue', ylab = 'Accuracy', xlab = 'Fold',
     ylim = range(c(1, trnACC.svm, valACC.svm)))

lines(valACC.svm, col = 'green', type = 'o', pch = 19, lwd= 3)
abline(h = 1, col = 'red', lty = 2, lwd = 2)

legend('topright', legend = c('training', 'validation'), col = c('blue', 'green'), lwd = 3, pch = 19, cex = 1.3)

cat('> Generalization Error:', round(mean(1-valACC.svm), 4), '\n')

model_full.svm = svm(Pos~., data = training, scale = T,
                     type = 'C-classification', cost = 0.239, kernel =  'linear')
fullDSAcc.svm = mean(training$Pos == model_full.svm$fitted) #Accuracy on the entire dataset
cat('> Empirical Error:', round(1-fullDSAcc.svm, 4), '\n') 


#Osservo il numero di support vectors

model_full.svm$tot.nSV
model_full.svm$tot.nSV/nrow(training)

#Visualizzo test set 
testPreds.svm = predict(model_full.svm, newdata = test)
testACC.svm = mean(test$Pos == testPreds.svm)  

#Matrice di confusione test set

confusionMatrix(testPreds.svm, test$Pos)

#Osservo quali osservazioni sono missclassificate
test$Predict.svm = testPreds.svm
FW_err = test[test$Pos != testPreds.svm & test$Pos == 'FW', ]
MF_err = test[test$Pos != testPreds.svm & test$Pos == 'MF' , ]
DF_err = test[test$Pos != testPreds.svm & test$Pos == 'DF' , ]
GK_err = test[test$Pos != testPreds.svm & test$Pos == 'GK' , ]
MF_err_predict = MF_err$Player
FW_err_predict = FW_err$Player


#Alcune rappresentazioni grafiche 
FW_test = test[test$Pos == 'FW', ]
FW_test$Predict.svm
FW_test %>%
  ggplot(aes(x = Comp.1, y = Comp.2))+
  geom_point(col = ifelse(FW_test$Predict.svm != FW_test$Pos, 'red', 'green'))+
  theme_bw()+
  theme_minimal(base_size = 12)+
  geom_label_repel(aes(label = ifelse(Predict.svm != Pos, Player, '')), box.padding = unit(0.4, 'lines'), min.segment.length = unit(0, 'lines'),
                   max.overlaps = Inf)+
  labs(x = 'Prima Componente', y = 'Seconda Componente', title = 'Attaccanti classificati erroneamente', caption = 'Source = FBref')



#Altri grafici 

MF_test = test[test$Pos == 'MF', ]
MF_test$Predict.svm
MF_test %>%
  ggplot(aes(x = Comp.1, y = Comp.2))+
  geom_point(col = ifelse(MF_test$Predict.svm != MF_test$Pos, 'red', 'green'))+
  theme_bw()+
  theme_minimal(base_size = 12)+
  geom_label_repel(aes(label = ifelse(Predict.svm != Pos, Player, '')), box.padding = unit(0.4, 'lines'), min.segment.length = unit(0, 'lines'),
                   max.overlaps = Inf)+
  labs(x = 'Prima Componente', y = 'Seconda Componente', title = 'Centrocampisti classificati erroneamente', caption = 'Source = FBref', color = '')

DF_test = test[test$Pos == 'DF', ]
DF_test$Predict.svm
DF_test %>%
  ggplot(aes(x = Comp.1, y = Comp.2))+
  geom_point(col = ifelse(DF_test$Predict.svm != DF_test$Pos, 'red', 'green'))+
  theme_bw()+
  theme_minimal(base_size = 12)+
  geom_label_repel(aes(label = ifelse(Predict.svm != Pos, Player, '')), box.padding = unit(0.4, 'lines'), min.segment.length = unit(0, 'lines'),
                   max.overlaps = Inf)+
  labs(x = 'Prima Componente', y = 'Seconda Componente', title = 'Difensori classificati erroneamente', caption = 'Source = FBref', color = '')


GK_test = test[test$Pos == 'GK', ]
GK_test$Predict.svm
GK_test %>%
  ggplot(aes(x = Comp.1, y = Comp.2))+
  geom_point(col = 'green')+
  theme_bw()+
  theme_minimal(base_size = 12)+
  geom_label_repel(aes(label = ifelse(Predict.svm != Pos, Player, '')), box.padding = unit(0.4, 'lines'), min.segment.length = unit(0, 'lines'),
                   max.overlaps = Inf)+
  labs(x = 'Prima Componente', y = 'Seconda Componente', title = 'Portieri classificati erroneamente', caption = 'Source = FBref', color = '')




# Random Forest -----------------------------------------------------------

#Utilizzo il dataset di partenza
x = read.csv('Dataset_imp.csv')[,-1]
x_numeriche = x %>%
  dplyr::select(-(Season_End_Year:Pos), -Born, -DummyCamp, -PK_percent)

x_numeriche$Pos =as.factor(x$Pos)

#Stesso traininig e stesso test della vecchia classificazione
training = x_numeriche[ixs$Resample1, ]
test = x_numeriche[-ixs$Resample1, ]
test_pl = x[-ixs$Resample1, ]

set.seed(2)
ixs.rf = createFolds(y = training$Pos, k = 10, list = T)
trnACC.rf = valACC.rf = numeric(length(ixs.rf))

#10-Fold Validation schema
for(k in 1:length(ixs.rf)){
  trnFold.rf = training[-ixs.rf[[k]],]
  valFold.rf = training[ixs.rf[[k]], ]
  
  #Training the model 
  model.rf = randomForest(Pos~., data = trnFold.rf)  
  
  cat('* Confusion Matrix on the training fold: \n')
  print(table(trnFold.rf$Pos, model.rf$predicted))
  trnACC.rf[k] = mean(trnFold.rf$Pos == model.rf$predicted)
  
  #Validating the model 
  preds.rf = predict(model.rf, newdata = valFold.rf) 
  
  cat('+ Confusion matrix on the validation fold: \n')
  print(table(valFold.rf$Pos, preds.rf))
  valACC.rf[k] = mean(valFold.rf$Pos == preds.rf)
  
}

plot(trnACC.rf, type = 'o', pch = 19, lwd = 3, col = 'blue', ylab = 'Accuracy', xlab = 'Fold',
     ylim = range(c(1, trnACC.rf, valACC.rf)))

lines(valACC.rf, col = 'green', type = 'o', pch = 19, lwd= 3)
abline(h = 1, col = 'red', lty = 2, lwd = 2)

legend('topright', legend = c('training', 'validation'), col = c('blue', 'green'), lwd = 3, pch = 19, cex = 1.3)

cat('> Generalization Error:', round(mean(1-valACC.rf), 4), '\n')
#0.1088


model_full = randomForest(Pos~., training)
fullDSAcc = mean(training$Pos == model_full$predicted) #Accuracy on the entire dataset
cat('> Empirical Error:', round(1-fullDSAcc, 4), '\n') #1099

#The final model is obtain using all the data and the specific empirical error. We can use it to provide prediction 

testPreds = predict(model_full, newdata = test)
testACC = mean(test$Pos == testPreds) #0.9036827 accuracy nel test set

confusionMatrix(testPreds, test$Pos)

#Visualizzo feature più importanti per la classificazione 
importance_matrix <- importance(model_full)
print(importance_matrix)
varImpPlot(model_full, n.var = 15, main = 'Feature più importanti secondo RF',
           pch = 19)

#Eseguo alcuni confronti e grafici 


#Confronto grafico tra denzel dumfries e le medie dei difensori 
var_imp = calcio %>%
  dplyr::select(Clr, Def.3rd_Touches, Sh_per_90_Standard,
         SoT_per_90_Standard, Dis_Carries, Mid.3rd_Tackles)

var_imp$Player = calcio$Player
var_imp$Pos = calcio$Pos

medie = var_imp %>%
  dplyr::select(-Player)%>%
  #  filter(Pos == 'DF') %>%
  group_by(Pos) %>%
  summarise(
    Clr = mean(Clr),
    Def.3rd_Touches = mean(Def.3rd_Touches),
    Sh_per_90_Standard = mean(Sh_per_90_Standard),
    SoT_per_90_Standard = mean(SoT_per_90_Standard),
    Dis_Carries = mean(Dis_Carries),
    Mid.3rd_Tackles = mean(Mid.3rd_Tackles))%>%
  mutate(Player = 'Medie')

trent = var_imp %>%
  filter(Player == 'Trent Alexander-Arnold')%>%
  dplyr::select(Player, Pos, Clr, Def.3rd_Touches, Sh_per_90_Standard,
         SoT_per_90_Standard, Dis_Carries, Mid.3rd_Tackles)


confronto = rbind(denzel, medie)

max = calcio %>%
  summarise(Clr = max(Clr),
            Def.3rd_Touches = max(Def.3rd_Touches),
            Sh_per_90_Standard = max(Sh_per_90_Standard),
            SoT_per_90_Standard = max(SoT_per_90_Standard),
            Dis_Carries = max(Dis_Carries),
            Mid.3rd_Tackles = max(Mid.3rd_Tackles)) %>%
  mutate(Player = 'Max', Pos = '')
min = calcio %>%
  summarise(Clr = min(Clr),
            Def.3rd_Touches = min(Def.3rd_Touches),
            Sh_per_90_Standard = min(Sh_per_90_Standard),
            SoT_per_90_Standard = min(SoT_per_90_Standard),
            Dis_Carries = min(Dis_Carries),
            Mid.3rd_Tackles = min(Mid.3rd_Tackles))%>%
  mutate(Player = 'Min', Pos = '')

max_min = rbind(max, min)
rownames(max_min) = c('max', 'min')  

confronto_fin = rbind(confronto, max_min)

colnames(confronto_fin) = c('Player', 'Pos', 'Clr', 'Def.3rd', 'Sh_per_90', 'Sot_per_90', 'Dis_Carries', 'Mid.3rd_Tkl')


create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.8,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 6,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.4), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "black", cglty = 1, cglwd = 1,
    # Customize the axis
    axislabcol = "black", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

par(mfrow=c(1,3))
op <- par(mar = c(1, 1, 1, 1))


create_beautiful_radarchart(
  data = confronto_fin[c('max', 'min', 1),-c(1,2)],
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)

legend(
  x = "bottom", legend = 'Statistiche Trent Alexander-Arnold', horiz = TRUE,
  bty = "n", pch = 20 , col = "#00AFBB",
  text.col = "black", cex = 1.2, pt.cex = 2
)

create_beautiful_radarchart(
  data = confronto_fin[c('max', 'min', 2),-c(1,2)],
  color =  "#E7B800"
)

legend(
  x = "bottom", legend = 'Medie Difensori', horiz = TRUE,
  bty = "n", pch = 20 , col = "#E7B800",
  text.col = "black", cex = 1.2, pt.cex = 2
)


create_beautiful_radarchart(
  data = confronto_fin[c('max', 'min', 5),-c(1,2)],
  color =  "red"
)

legend(
  x = "bottom", legend = 'Medie Centrocampisti', horiz = F,
  bty = "n", pch = 20 , col = "red",
  text.col = "black", cex = 1.2, pt.cex = 2
)




#varImpPlot#Osservo quali osservazioni sono missclassificate
test_pl$Predict.rf = testPreds
FW_err = test_pl[test$Pos != testPreds & test$Pos == 'FW', ]
MF_err = test_pl[test$Pos != testPreds & test$Pos == 'MF' , ]
DF_err = test_pl[test$Pos != testPreds & test$Pos == 'DF' , ]
GK_err = test[test$Pos != testPreds & test$Pos == 'GK' , ]

DF_test = test_pl[test$Pos == 'DF', ]
DF_test %>%
  ggplot(aes(x = Clr, y = Def.3rd_Touches))+
  geom_point(col = ifelse(DF_test$Predict.rf != DF_test$Pos, 'red', 'green'))+
  theme_bw()+
  theme_minimal(base_size = 12)+
  geom_label_repel(aes(label = ifelse(Predict.rf != Pos, Player, '')), box.padding = unit(0.4, 'lines'), min.segment.length = unit(0, 'lines'),
                   max.overlaps = Inf)+
  labs(x = 'CLR', y = 'Tocchi nella trequarti difensiva', title = 'Difensori classificati erroneamente', caption = 'Source = FBref')


#Osservo giocatori missclafficati sia nel random forest che nel svm 

length(testPreds.svm)
test_pl$predict.svm = as.factor(testPreds.svm)
test_pl$missing.rf = ifelse(test_pl$Predict.rf != test_pl$Pos, T, F)
test_pl$Miss.svm = ifelse(test_pl$predict.svm != as.factor(test_pl$Pos), T, F)

test_df = test_pl[test_pl$Pos == 'DF', ]

#Osservazioni miss classificate da entrambi i modelli
errati = test_pl[test$predict.rf != test$Pos & test$missing.rf == T & test$Miss.svm == T, ]
errati


FW_err = test[test$Pos != testPreds & test$Pos == 'FW', ]

test_pl %>%
  ggplot(aes(x = Clr, y = Mid.3rd_Touches))+
  geom_point(col = ifelse(test_pl$predict.rf != test$Pos & test_pl$missing.rf == T & test_pl$Miss.svm == T, 'red', 'green'))+
  theme_bw()+
  theme_minimal(base_size = 12)+
  geom_label_repel(aes(label = ifelse(predict.rf != Pos & missing.rf == T & Miss.svm == T, Player, '')), box.padding = unit(0.4, 'lines'), min.segment.length = unit(0, 'lines'),
                   max.overlaps = Inf)+
  labs(x = 'Clr', y = 'Mid 3rd Touches', title = 'Classificazioni errate da entrambi i modelli', caption = 'Source = FBref', color = '')





# CLUSTERING --------------------------------------------------------------

rm(list = ls())
calcio = read.csv('Dataset_imp.csv')[,-1]

#Rieseguo l'analisi in componenti principali 

calcio %>%
  group_by(Pos)%>%
  count()

calcio %>%
  ggplot(aes(x = Pos,y = TklW_Tackles), col = Pos)+
  geom_violin(scale = 'area')+
  scale_color_binned()

num = calcio %>%  
  dplyr::select(-c(Season_End_Year:Nation), -Born, -Comp, -Pos, -DummyCamp)

# Pca fatta sui dati standardizzati
str(num)
pca = princomp(num, cor=T) 

# Estrarre le componenti principali
principal_components <- pca$scores

# Varianza spiegata dalle componenti principali
explained_variance <- summary(pca)
fviz_eig(pca, addlabels = TRUE, choice = 'variance', barfill = 'darkorange',barcolor = 'black', xlab = '',ncp = 11)

q <- 11
cumsum(pca$sdev^2/sum(pca$sdev^2))[1:q] 

# Le prime 11 componenti spiegano l'86% della varianza

selected_components <- principal_components[, 1:q]

sel_pca = calcio %>%
  dplyr::select(Season_End_Year:Born) %>%
  dplyr::select(-Age)
data = data.frame(sel_pca, selected_components)


str(data)
#Analisi grafica di alcune componenti 


library(plotly)
data %>% 
  
  plot_ly(x=~Comp.1,y=~Comp.2,z= ~Comp.3,color = ~Pos ,hoverinfo = 'text', colors = c( 'green', 'red', 'orange', 'blue')) %>% 
  
  add_markers(opacity = 0.8) %>%
  
  layout(title = "",
         
         annotations=list(yref='paper',xref="paper",y=1.05,x=1.1, text="Posizioni",showarrow=F),
         
         scene = list(xaxis = list(title = 'Prima Componente'),
                      
                      yaxis = list(title = 'Seconda Componente'),
                      
                      zaxis = list(title = 'Terza Componente')))

par(mfrow = c(1,2))
dati = data %>%
  ggplot(aes(Comp.1,Comp.2, colour = Pos))+
  geom_point()+
  labs(x = 'Prima Componente Principale', y = 'Seconda Componente Principale')+
  theme_minimal()+
  scale_color_manual(values = c('red', 'green', 'orange', 'blue'))

#Si nota subito come i portieri siano un gruppo a se stante

#Alcune visualizzazioni grafiche
data %>%
  ggplot(aes(Comp.6,Comp.7, colour = Pos))+
  geom_point()

calcio %>%
  ggplot(aes(x = CPA_Carries, y = prezzo_numerico, colour = Pos))+
  geom_point()

data_clust = data %>%
  dplyr::select(-(Season_End_Year:Born))

#Metodo del gomito per la scelta del k ottimo
fviz_nbclust(data_clust, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +  # esempio: scegliendo 3 cluster
  labs(subtitle = "Elbow method")

# Metodo della silhouette
fviz_nbclust(data_clust, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#Algoritmo KMeans
km_out = kmeans(data_clust, centers = 4, iter.max = 500, nstart = 500)
km_out

table(km_out$cluster)


data_clustered <- data %>%
  mutate(cluster = km_out$cluster)

ggplot(data_clustered, aes(x = Comp.1, y = Comp.2, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering", x = "Prima Componente Principale", y = "Seconda Componente principale", color = "Cluster") +
  theme_minimal()


color_palette = c('green', 'blue', 'red', 'orange')
cluster = ggplot(data_clustered, aes(x = Comp.1, y = Comp.2, color = factor(cluster))) +
  geom_point() +
  stat_ellipse(type = "norm") +
  labs(title = "",
       x = "Prima Componente Principale",
       y = "Seconda Componente Principale",
       color = 'Cluster',
       shape = '') +
  theme_minimal()+
  scale_color_manual(values = color_palette)
grid.arrange(dati ,cluster, nrow = 2)

#Voglio visualizzare le percentuali delle posizioni nei cluster

cluster1 = data_clustered[data_clustered$cluster == 1, ]
prop.table(table(cluster1$Pos))
cluster1[cluster1$Pos == 'DF', ]
cluster1[cluster1$Pos == 'MF', ]

cluster2 = data_clustered[data_clustered$cluster == 2, ]
prop.table(table(cluster2$Pos))
cluster2[cluster2$Pos == 'FW', ]

cluster3 = data_clustered[data_clustered$cluster == 3, ]
prop.table(table(cluster3$Pos))
cluster3[cluster3$Pos == 'FW', ]

cluster4 = data_clustered[data_clustered$cluster == 4, ]
prop.table(table(cluster4$Pos))

cluster_summary <- data_clustered %>%
  dplyr::select(Pos, cluster) %>%
  group_by(cluster, Pos) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

#Meglio

ggplot(cluster_summary, aes(x = cluster, y = Percentage, fill = Pos, label = paste0(round(Percentage), "%"))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "", x = "Cluster", y = "Percentuale") +
  geom_text(position = position_stack(vjust = 0.5), aes(group = Pos), size = 3)+
  scale_fill_brewer(palette = "Set2") +  # Palette di colori
  theme_minimal()

# Visualize the k-means clusters
fviz_cluster(km_out, data = data_clust[,c(1,2)], geom = "point", main = '',
             xlab = 'Prima Componente', ylab = 'Seconda Componente')



#Risultati silhouette

sil = silhouette(km_out$cluster, dist(data_clust))
silhouette_df <- as.data.frame(sil)
# Visualizzazione dell'indice silhouette
fviz_silhouette(sil)

# Mixture Model Cluster ---------------------------------------------------

graphics.off()
#con ICL
mclustICL<-mclustICL(data_clust)
#Best ICL values:
#VVV,5       VVV,6       VVV,7
#ICL      -50109.26 -50164.0081 -50318.2657
#ICL diff      0.00    -54.7526   -209.0103
plot(mclustICL)

summary(mclustICL) #MODELLO MIGLIORE VVV

pca_clustICL<-Mclust(data_clust,modelNames="VVV", G=5)
summary(mclustICL) #Miglior cluster

#p indica le mixing probabilities
#primo cluster
media_1<-pca_clustICL$parameters$mean[,1] #media
var_1<-pca_clustICL$parameters$variance$sigma[, , 1]
p1<-pca_clustICL$parameters$pro [1]
#secondo cluster
media_2<-pca_clustICL$parameters$mean[,2]#media
var_2<-pca_clustICL$parameters$variance$sigma[, , 2]
p2<-pca_clustICL$parameters$pro[2]
#terzo cluster
media_3<-pca_clustICL$parameters$mean[,3]#media
var_3<-pca_clustICL$parameters$variance$sigma[, , 3]
p3<-pca_clustICL$parameters$pro[3]
#quarto cluster
media_4<-pca_clustICL$parameters$mean[,4]#media
var_4<-pca_clustICL$parameters$variance$sigma[, , 4]
p4<-pca_clustICL$parameters$pro[4]
#quinto cluster
media_5<-pca_clustICL$parameters$mean[,5]#media
var_5<-pca_clustICL$parameters$variance$sigma[, , 5]
p5<-pca_clustICL$parameters$pro[5]


#KL_S divergenza di kullback leiber 
KL_S<-function(mu1,mu2,sigma1,sigma2) {
  1/2*t(mu1-mu2)%*%(solve(sigma1)+solve(sigma2))%*%(mu1-mu2)
  +1/2*sum(diag(sigma1%*%solve(sigma2)+solve(sigma1)%*%sigma2))-length(mu1)
}

#Calcolare entropia, R^2, KLS 
#entropia
zi_j<- round(pca_clustICL$z)+0.0001
EN<- - sum(zi_j*log(zi_j))
EN/nrow(data_clust)*log(4) #0.00497

#R^2
1-det(p1*var_1+p2*var_2+p3*var_3+p4*var_4)/det(var(data_clust)) #0.9999

#KL_S

KL_S(media_1, media_2, var_1, var_2) #53.66581
KL_S(media_1, media_3, var_1, var_3) #42.85136
KL_S(media_1, media_4, var_1, var_4) #3250.822
KL_S(media_1, media_5, var_1, var_5) #8.08826

KL_S(media_3, media_2, var_3, var_2) #14.79639
KL_S(media_4, media_2, var_4, var_2)  #6212.315
KL_S(media_5, media_2, var_5, var_2) #21.40965

KL_S(media_3, media_4, var_3, var_4)  #7941.022
KL_S(media_3, media_5, var_3, var_5)  #21.16639

KL_S(media_4, media_5, var_4, var_5) #2276.711




#aggiungere etichette per cluster nel plot
coordProj(data=as.data.frame(data_clust), dimens=c(1,2), what="uncertainty", 
          parameters = pca_clustICL$parameters,z=pca_clustICL$z)


#Risultati silhouette

sil = silhouette(pca_clustICL$classification, dist(data_clust))
silhouette_df <- as.data.frame(sil)
# Visualizzazione dell'indice silhouette
fviz_silhouette(sil)

# Calcolo del valore medio dell'indice silhouette
mean_silhouette <- mean(silhouette_df[, "sil_width"])
print(mean_silhouette)

plot(mclustICL, what = "ICL")

data_icl = data
data_icl$cluster = pca_clustICL$classification
data_icl[data_icl$cluster == 3 & data_icl$Pos == 'MF', ]



cluster_summary_icl <- data_icl %>%
  dplyr::select(Pos, cluster) %>%
  group_by(cluster, Pos) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggplot(cluster_summary_icl, aes(x = cluster, y = Percentage, fill = Pos, label = paste0(round(Percentage), "%"))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "", x = "Cluster", y = "Percentuale") +
  geom_text(position = position_stack(vjust = 0.5), aes(group = Pos), size = 3)+
  scale_fill_brewer(palette = "Set2") +  # Palette di colori
  theme_minimal()





