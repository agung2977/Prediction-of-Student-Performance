library(e1071)
library(ggplot2)
library(caret)
library(devtools)

df=read.csv("C:\\Users\\ASUS\\OneDrive\\Desktop\\xAPI-Edu-Data.csv") 

str(df)
summary(df)
attach(df)
qplot(raisedhands,Discussion, data = df, color=Topic)

df$Topic <- as.factor(df$Topic)  # Mengubah variabel dependen menjadi faktor

#Pengujian model
model=svm(Topic~., data = df)
summary(model)
prediks <- predict(model, df)
prediks
confusionMatrix(prediks, df$Topic)

#kernel radial
model_svm_radial=svm(formula=Topic~., data=df, type="C-classification", kernel="radial", 
                     cost = 1, scale = FALSE)
model_svm_radial
summary(model_svm_radial)
prediks_radial <- predict(model_svm_radial, df)
prediks_radial
confusionMatrix(prediks_radial, df$Topic)
#plot(model_svm_radial, data = df, raisedhands ~ Discussion, slice = list(raisedhands = 12, Discussion = 12))

#kernel polynominal
model_svm_polynomial <- svm(formula = Topic ~ ., data = df, type = "C-classification", kernel = "polynomial", cost = 1, scale = FALSE)
model_svm_polynomial
summary(model_svm_polynomial)
prediks_polynominal <- predict(model_svm_polynomial, df)
prediks_polynominal
confusionMatrix(prediks_polynominal, df$Topic)
plot(prediks_polynominal)
#plot(model_svm_polynomial,data = df,raisedhands~Discussion,slice = list(raisedhands = 12, Discussion = 12))

#kernel linear
model_svm_linear <- svm(formula = Topic ~ ., data = df, type = "C-classification", kernel = "linear", cost = 1, scale = FALSE)
model_svm_linear
summary(model_svm_linear)
prediks_linear <- predict(model_svm_linear, df)
prediks_linear
confusionMatrix(prediks_linear, df$Topic)
plot(prediks_linear)
#plot(model_svm_polynomial,data = df,raisedhands~Discussion,slice = list(raisedhands = 12, Discussion = 12))

