base = read.csv('credit_data.csv')
base$clientid <- NULL

#Valores inconsistentes
base$age = ifelse(base$age < 0, 40.92, base$age)

#Valores faltantes
base$age = ifelse(is.na(base$age),mean(base$age, na.rm = TRUE), base$age)

#Escalonamento
base[,1:3] = scale(base[, 1:3])

base$default = factor(base$default, levels= c(0,1))

#Treinamento e teste
library(caTools)
set.seed(1)
divisao = sample.split(base$income, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

library(rpart)
classificador = rpart(formula = default ~., data= base_treinamento)
print(classificador)

library(rpart.plot)
rpart.plot(classificador)

previsao = predict(classificador, newdata = base_teste[-4], type='class')
print(previsao)
matriz_confusao = table(base_teste[,4], previsao)
print(matriz_confusao)
library(caret)
confusionMatrix(matriz_confusao)