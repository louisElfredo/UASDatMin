library(readxl)
library(MASS)
library(dplyr)
library(tidyr)
library(tidyverse)
library(caTools)
library(ROCR) 
library(caret)
library(WVPlots)
library(lmtest)

dt <- read_excel("C:/Users/Feelouis Elfredo/Downloads/20230113070740_STAT6157016_FIN_RCQuestion/STAT6157016 - Data Mining and Visualization - Copy/Breast Cancer Wisconsin (Diagnostic).xlsx")
View(dt)
head(dt)
summary(dt)

dt$diagnosis <- as.factor(dt$diagnosis)
summary(dt)
dt <- subset(dt, select = -c(id))
View(dt)

#Cek missing value
sum(is.na(dt$id))
sum(is.na(dt$diagnosis))
sum(is.na(dt$radius_mean))
sum(is.na(dt$texture_mean))
sum(is.na(dt$perimeter_mean))
sum(is.na(dt$area_mean))
sum(is.na(dt$smoothness_mean))
sum(is.na(dt$compactness_mean))
sum(is.na(dt$concavity_mean))
sum(is.na(dt$`concave points_mean`))
sum(is.na(dt$symmetry_mean))
sum(is.na(dt$fractal_dimension_mean))

#Data Splitting
set.seed(549)
samples <- createDataPartition(y = dt$diagnosis, p=0.7, list = FALSE)
trng <- dt[samples, ]
tst <- dt[-samples, ]

#logistic modelling
lgm <- glm(diagnosis ~., family = "binomial", data = trng,)
summary(lgm)

prob <- predict(lgm , newdata = tst , type = "response")
predicted <- ifelse(prob>0.5 , 1 , 0)
mean(predicted == tst$diagnosis)

acc <- sum(tst$diagnosis == predicted)/length(tst$diagnosis)
prcs <- sum(tst$diagnosis == 1 & predicted == 1)/(sum(predicted == 1))
rcl <- sum(tst$diagnosis == 1 & predicted == 1)/(sum(tst == 1))

acc
prcs
rcl

#ROC
predRO <- prediction(prob, tst$diagnosis)
perfRO <- performance(predRO, measure = "tpr" , 
                    x.measure = "fpr")
auc <- performance(predRO, measure ="auc")
auc <- auc@y.values[[1]]
auc