#############
##Regression 
#############

library(ggcorrplot)
library(GGally)
library(randomForest)
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

#Caricamento del dataset 

calcio <- read.csv('Dataset_finale.csv')

# Tolgo la variabile Url

calcio <- dplyr::select(calcio, -Url)

# Riscalo la variabile prezzo numerico in Milioni di euro per avere maggiore interpretabilita dei risultati 

calcio <- calcio %>%
  mutate(prezzo_numerico = prezzo_numerico/1000000)

#Boxplot 


ggplot(calcio, aes(x = Comp, y = prezzo_numerico, fill = Comp)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot del Valore di Mercato per Competizione",
       x = "Comp",
       y = "Prezzo Numerico") +
  theme(legend.position = "none") 

#Premier ha mediana piu' alta 
#Creo una dummy per valutarne l'importanza 

calcio$DummyCamp <- as.factor(ifelse(calcio$Comp == "Premier League", 1, 0))

ggplot(calcio, aes(x = DummyCamp, y = prezzo_numerico, fill = DummyCamp)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Comp",
       y = "Prezzo Numerico") +
  scale_fill_manual(values = c("tomato", "dodgerblue")) +  # Sostituisci con i colori che preferisci
  theme(legend.position = "none")


# Divido in due dataset: uno con gli NA e uno senza

calcio_noNA <- calcio[complete.cases(calcio$prezzo_numerico),]
sum(is.na(calcio_noNA))
calcio_NA <- calcio[is.na(calcio$prezzo_numerico) == T, ]
sum(is.na(calcio_NA))


num <- dplyr::select(calcio_noNA, -c(Season_End_Year:Nation), -Age, -Born)

# Rendo la posizione una variabile numerica
num$Pos <- as.numeric(as.factor(num$Pos))

# Non devo coinvolgere la variabile prezzo numerico nella pca

X <- dplyr::select(num, -prezzo_numerico)

# Rendo DummyCamp numerica

X$DummyCamp <- as.numeric(X$DummyCamp)

# Pca fatta sui dati standardizzati

pca <- princomp(X, cor=T) 

# Estrarre le componenti principali
principal_components <- pca$scores

# Varianza spiegata dalle componenti principali
explained_variance <- summary(pca)

# Proporzione di varianza spiegata dalle prime q componenti principali

q <- 10
cumsum(pca$sdev^2/sum(pca$sdev^2))[1:q] 

# Le prime 10 componenti spiegano l'84% circa della varianza

selected_components <- principal_components[, 1:q]

feat.sel <- data.frame(selected_components, prezzo_numerico = num$prezzo_numerico)

cor = cor(feat.sel)
corrplot(cor)

# Divido in Training e Validation. Il Test sono quelle osservazioni che non hanno il prezzo_numerico

set.seed(123)
ixs = sample(1:nrow(feat.sel), size = 0.8*nrow(feat.sel), replace = F)
train = feat.sel[ixs, ] #Training
val = feat.sel[-ixs, ] #Validation


# SVM  Regression -------------------------------------------------------------

#Come test set utilizziamo i dati che hanno valore NA nella variabile prezzo_numerico 

#Model SVM piu' facile 

model.svm = svm(prezzo_numerico~.,
                data = train, scale = T,
                type = 'eps-regression',
                kernel = 'linear',
                epsilon = 0.5)

summary(model.svm)


#Empirical error
sqrt(mean((model.svm$fitted - train$prezzo_numerico)^2)) # 16.4057

#Generalization Error
y.val = predict(model.svm, val)
sqrt(mean((y.val-val$prezzo_numerico)^2)) # 19.24172

#Auto machine learning per l'ottimizzazione degli iperparametri in svm regression 

par.set = makeParamSet(
  makeDiscreteParam('kernel', values = c('linear', 'radial')),
  makeNumericParam('cost', lower = -2, upper = 2, trafo = function(x) 10^x),
  makeNumericParam('gamma', lower = -2, upper = 2, trafo = function(x) 10^x,
                   requires = quote(kernel == 'radial')),
  makeNumericParam('epsilon', lower = 0.001, upper = 0.999)
)

ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iter = 100)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

tune.ctrl = makeTuneControlMBO(mbo.control = ctrl)

task = makeRegrTask(data = feat.sel, target = 'prezzo_numerico')

run = tuneParams(makeLearner('regr.svm'), task, cv10, measures = rmse, 
                 par.set = par.set, control = tune.ctrl, show.info = T)

# Result: kernel=radial; cost=2.95; gamma=0.015; epsilon=0.318 
# rmse.test.rmse=16.4532835

y = getOptPathY(run$opt.path)
plot(y, type = 'o', col = 'green', ylab = 'RMSE', xlab = 'Iter', lwd = 2, pch = 19)
lines(cummin(y), col = 'red', type = 'o', lwd = 2)


#SVM con paramatri ottimali 

mod.opt.svm <- svm(prezzo_numerico~.,
                  data = feat.sel, scale = T,
                  type = 'eps-regression',
                  kernel = 'radial',
                  cost = 2.95,
                  epsilon = 0.318,
                  gamma =0.015)


#Osservo il numero di support vectors

mod.opt.svm$tot.nSV #566 Support Vector sono tanti 

#Overfitting? 
mod.opt.svm$tot.nSV/nrow(train)

# Empirical error:
y.opt.svm <- predict(mod.opt.svm, train)
cat("> Empirical error (RMSE):",sqrt(mean((y.opt.svm - train$prezzo_numerico)^2)),"\n") # 14.91249 

# Generalization error:
y.opt.svm <- predict( mod.opt.svm, val )
cat("> Generalization error (Hold-out RMSE):",sqrt(mean((y.opt.svm - val$prezzo_numerico)^2))) # 16.64196


# Processi Gaussiani ------------------------------------------------------

set.seed(123)
ixs = sample(1:nrow(feat.sel), size = 0.8*nrow(feat.sel), replace = F)
train = feat.sel[ixs, ] #Training
val = feat.sel[-ixs, ] #Validation

Gau_train = dplyr::select(train, -prezzo_numerico)

Gau_val = dplyr::select(val, -prezzo_numerico)

model.gp = km(design = data.frame(Gau_train), response = train$prezzo_numerico, covtype = 'gauss',
              nugget.estim = T, scaling = T)

#Previsione

y.gp = predict(model.gp, Gau_val, type = 'UK')
y.gpmean = y.gp$mean
gp.sd = y.gp$sd
gp.up = y.gp$upper95
gp.lb = y.gp$lower95

# Generalization Error
sqrt(mean((y.gpmean - val$prezzo_numerico)^2)) # 19.38734

plot(val$Comp.1, val$prezzo_numerico, type = 'l', 
     lty = 2, ylim = c(min(gp.lb), max(gp.up)))
polygon(c(val$Comp.2, rev(val$Comp.3)), c(gp.lb, rev(gp.lb)), col = adjustcolor('green', alpha.f = 0.15), border = NA)
lines(val$Comp.3, y.gpmean, col = 'green', lwd = 3)


# Knn ---------------------------------------------------------------------

knn.reg <- knnreg(prezzo_numerico~., data = train, scale = T) # 5-nn
y.pred <- predict(knn.reg, val)

# Generalization Error
sqrt(mean((y.pred - val$prezzo_numerico)^2)) # 18.61935

gen_err <- c()
for (i in 1:20){
  
  knn.reg <- knnreg(prezzo_numerico~., data = train, scale = T, k = i) # 5-nn
  y.pred <- predict(knn.reg, val)
  
  # Generalization Error
  gen_err[i] <- sqrt(mean((y.pred - val$prezzo_numerico)^2)) 
  
}

min_index <- which.min(gen_err)
min_value <- gen_err[min_index]

# Plotta il grafico con i valori di errore
plot(gen_err, type = "o", xlab = "k", ylab = "Generalization Error")

# Aggiungi un punto per il minimo
points(min_index, min_value, col = "red", pch = 19, cex = 1.5)


knn.reg <- knnreg(prezzo_numerico~., data = train, scale = T, k = 8) # 8-nn
y.pred <- predict(knn.reg, val)

# Generalization Error
sqrt(mean((y.pred - val$prezzo_numerico)^2)) # 18.1282


# Previsioni --------------------------------------------------------------

# Predico il valore di mercato dei gicatori per cui non era registrato

X.test <- calcio_NA %>%
  select(-prezzo_numerico, -c(Season_End_Year:Nation), -Born, -Comp)

X.test$Pos <- as.numeric(as.factor(X.test$Pos))

# Rendo DummyCamp numerica affinchè possa calcolare la matrice di correlazione

X.test$DummyCamp <- as.numeric(X.test$DummyCamp)

# Trasformare i nuovi dati utilizzando la stessa PCA
principal_components_new <- predict(pca, newdata = X.test)

# Selezionare le prime 10 componenti principali
selected_components_new <- principal_components_new[, 1:q]

# Fare previsioni utilizzando il modello k-NN addestrato

predictions <- predict(knn.reg, newdata = data.frame(selected_components_new))
summary(predictions)

# Fare previsioni utilizzando il processo Gaussiano

pred.gp = predict(model.gp, data.frame(selected_components_new), type = 'UK')
summary(pred.gp$mean)

# Previsioni con SVM

pred.svm = predict(mod.opt.svm, data.frame(selected_components_new))
summary(pred.svm)

# Controllo alcune statistiche descrittive delle previsioni

summary(predictions)
summary(pred.gp$mean)
summary(pred.svm)
summary(calcio$prezzo_numerico)


which.max(predictions)
which.max(pred.gp$mean)
which.max(pred.svm)


par(mfrow = c(1,4))
boxplot(predictions, xlab = "K-nn", col = "mistyrose", ylim = c(0,181))
boxplot(pred.gp$mean, xlab = "Gaussian Processes", col = "springgreen", ylim = c(0,181))
boxplot(pred.svm, xlab = "SVM", col = "deepskyblue", ylim = c(0,181))
boxplot(calcio$prezzo_numerico, xlab = "Dataset Originale", col = "khaki1", ylim = c(0,181))
par(mfrow = c(1,1))

calcio_NA[which.max(predictions), ]
calcio_NA$pred.knn = predictions
calcio_NA$pred.gp = pred.gp$mean
calcio_NA$pred.svm = pred.svm

dplyr::select(calcio_NA, Player, pred.knn, pred.gp, pred.svm)

# Scelgo di imputare tramite knn 

calcio.imp <- calcio
calcio.imp[is.na(calcio.imp$prezzo_numerico) == T, ]$prezzo_numerico <- predictions

