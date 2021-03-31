# Cargamos las librerias
library(readxl)
library(tidyverse)
library(skimr)
library(MASS)
library(rpart)    
library(rpart.plot)
library(e1071)
library(ggplot2)
library(ggpubr)
library(openxlsx)
library(pROC)
library(caret)

# Cargamos los datos
pi <- read_excel('pi.xls')

# Trannformaciones para fecha en la que se puso en circulacion el euro y añadimos la variacion de M3 de cada año
pi <- pi %>% 
  filter(año >= 2002) %>% 
  mutate(m3 = case_when(año <= 2002 ~ 7,
                            año <= 2003 ~ 7.2,
                            año <= 2004 ~ 6.6,
                            año <= 2005 ~ 7.3,
                            año <= 2006 ~ 10,
                            año <= 2007 ~ 11.5,
                            año <= 2008 ~ 7.7,
                            año <= 2009 ~ -0.3,
                            año <= 2010 ~ -1.2,
                            año <= 2011 ~ 1.6,
                            año <= 2012 ~ 3.6,
                            año <= 2013 ~ 1,
                            año <= 2014 ~ 3.8,
                            año <= 2015 ~ 4.7,
                            año <= 2016 ~ 5,
                            año <= 2017 ~ 4.7,
                            año <= 2018 ~ 4.2,
                            año <= 2019 ~ 4.9,
                            año <= 2020 ~ 12.4,))

# Vemos con que variables nos quedamos
datos <- pi[,c(1,2,3,4,5,7,8,9,10,11)]

# Guardamos el dataset ya listo para el modelaje
ds <- datos %>% 
  mutate(pi = case_when(pi <= 0.015 ~ 0,
                        pi > 0.015 ~ 1))
write.xlsx(ds, file="inflationid.xlsx", sheetName="model", append=TRUE)
ds <- ds[,-c(1,2)]
write.xlsx(ds, file="inflation.xlsx", sheetName="model", append=TRUE) # hay que saber previamente en que director estamos
dim(ds)

# Modelo lineal para ver la significación 
ml <- lm(pi~.,data = datos)
summary(ml)

#EDA
plot1 <- ggplot(data = datos, aes(x = deuda)) +
  geom_density()+ 
  theme_bw()
plot2 <- ggplot(data = datos, aes(x = paro)) +
  geom_density()+ 
  theme_bw()
plot3 <- ggplot(data = datos, aes(x = actividad)) +
  geom_density()+ 
  theme_bw()
plot4 <- ggplot(data = datos, aes(x = pi)) +
  geom_density()+ 
  theme_bw()
ggarrange(plot1, plot2, plot3, plot4, common.legend = TRUE, legend = "bottom")

# Boxplot
plot1 <- ggplot(data = datos, aes(x = deuda)) +
  geom_boxplot()+ 
  theme_bw()
plot2 <- ggplot(data = datos, aes(x = paro)) +
  geom_boxplot()+ 
  theme_bw()
plot3 <- ggplot(data = datos, aes(x = actividad)) +
  geom_boxplot()+ 
  theme_bw()
plot4 <- ggplot(data = datos, aes(x = pi)) +
  geom_boxplot()+ 
  theme_bw()
ggarrange(plot1, plot2, plot3, plot4, common.legend = TRUE, legend = "bottom")


# Transformacion de target
ds$pi <- as.factor(ds$pi)
table(ds$pi)
escalar <- as.data.frame(scale(ds[,-4]))
datos <- cbind(escalar, pi = ds$pi)

# Hacemos train y test
set.seed(1789)
ind <- sample(2, nrow(datos), replace = TRUE, prob = c(0.7, 0.3))
train <- datos[ind == 1, ]
test <- datos[ind == 2, ] 
table(train$pi)
table(test$pi)

testpi <- as.vector(test$pi)

#------------------------------------------------------------------------------- Hacemos un modelo logista
# Nosostros hemos escalado pero lo hace solo
model <- glm(pi ~., data=train, family = binomial(link = logit))
summary(model)

# pred train
predt <- round(predict(model, train, type="response"), 0)
matriz_confusiont <- table(predt, train$pi)
matriz_confusiont
sum(diag(matriz_confusiont))/sum(matriz_confusiont) # 64,8% // 65,3% // 63%

# pred test
predlog <- round(predict(model, test, type="response"), 0)
test_log<-table(pred = predlog, true = testpi)
confusionMatrix(test_log)


# ------------------------------------------------------------------------------ Hacemos un LDA
# Nosostros hemos escalado pero lo hace solo
ldamod <- lda(pi ~., data = train)

# pred train 
ldaResult <- predict(ldamod, newdata = train) 
tldamod<-table(ldaResult$class, train$pi) 
tldamod 
sum(diag(tldamod))/sum(tldamod) # 62,5%


# pred test
ldaResultr <- (predict(ldamod, test, type="response"))#predict(ldamod, newdata = test) 
test_lda <- table(pred = ldaResultr$class, true = test$pi)
confusionMatrix(test_lda)


# ------------------------------------------------------------------------------ Hacemos un RF
# Nosostros hemos escalado pero lo hace solo
rf <- rpart(pi ~ ., method = "class", data =train)
print(rf)               
rpart.plot(rf,extra=4)  

# pred train
pred <- predict(rf, newdata = train, type = "class")
t1<-table(pred, train$pi)
t1
sum(diag(t1))/sum(t1) # acierto del 78,5% // 79,12% // 79,5%

# pred test
predtrf <- predict(rf, newdata = test, type = "class")
test_rf<-table(predt = predtrf, true = testpi)
confusionMatrix(test_rf)




# ------------------------------------------------------------------------------ Hacemos un NB
# Nosostros hemos escalado pero lo hace solo
nb <- naiveBayes(pi ~ ., data = train)
nb 

# pred train
pred <- predict(nb, newdata=train, type = "class") # clase, es la que se meta en matriz confusion 
matrizconfusion <- table(train$pi, pred)
matrizconfusion 
sum(diag(matrizconfusion))/sum(matrizconfusion) # 64,3% // 50% // 61%

# pred test
predt <- predict(nb, newdata=test, type = "class") # clase, es la que se meta en matriz confusion 
test_nb<-table(predt = predt, true = testpi)
confusionMatrix(test_nb)


# ------------------------------------------------------------------------------ Hacemos un SVM
# Nosostros hemos escalado pero lo hace solo
svm <- svm(pi~., data=train, kernel="radial")
print(svm)
summary(svm)

# pred train
pred <- predict(svm, newdata = train, type = 'class')
preR <- predict(svm, test)
t1<-table(train$pi, pred)
t1
sum(diag(t1))/sum(t1) # 75,6% 

# pred test
predtsvm <- predict(svm, newdata = test, type = 'class')
#
test_svm<-table(predt = predtsvm, true = testpi)
confusionMatrix(test_svm)

#### CURVAS ROC 
ROC <- roc(pi ~ predlog, data = test)
plot(ROC)
roc_rf_test <- roc(response = testpi, predictor = as.numeric(predtrf))
plot(roc_rf_test, add = T, col = "blue", print.auc=F, print.auc.x = 0.5, print.auc.y = 0.3)
roc_svm_test <- roc(response = testpi, predictor = as.numeric(predtsvm))
plot(roc_svm_test, add = TRUE, col = "red", print.auc=F, print.auc.x = 0.5, print.auc.y = 0.3)

