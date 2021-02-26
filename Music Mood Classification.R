library("scales")
library("purrr")
library("tidyverse")  # data manipulation
library("cluster")    # clustering algorithms
library("factoextra")
library("NbClust")
library("ggplot2")
library("dplyr")
library("mclust")
library("fpc")
library("plyr")
library("meltt")
library("class")
library("reshape")
library("reshape2")
library("ISLR")
library("caret")
library("ff")
memory.limit(size = 512000) 
final_data <- read.csv("C:/Users/buse/Desktop/final.csv", sep=",", encoding = "UTF-8")

trellis.par.set(caretTheme())

#*****COMPUTE METRÝCS*****
compute_metrics <- function(cm) {
  precision = diag(cm$table) / colSums(cm$table) 
  recall = diag(cm$table) / rowSums(cm$table) 
  f1 = 2 * precision * recall / (precision + recall) 
  data.frame(precision, recall, f1) 
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  data.frame(macroPrecision, macroRecall, macroF1)
}

#*******NAIVE BAYES**********
start_time <- Sys.time()
trControl <- trainControl(method="cv", number=5)
naiveBayes_model <- train(mood~., data=final_data, method = "nb",trControl=trControl)
end_time <- Sys.time()
end_time - start_time
print(naiveBayes_model)
cm<-confusionMatrix(naiveBayes_model)
compute_metrics(cm)
plot(naiveBayes_model)

#*******RANDOM FOREST*********
start_time <- Sys.time()
randomForest_model <- train(mood ~ .,data = final_data,  method="rf",trControl=trControl)
end_time <- Sys.time()
end_time - start_time
print(randomForest_model)
cm<-confusionMatrix(randomForest_model)
compute_metrics(cm)
plot(randomForest_model)

#******DECISION TREE******
start_time <- Sys.time()
trControl <- trainControl(method  = "cv",number  = 5)
decisionTree_model = train(mood ~ ., data=final_data, method="rpart2", trControl = trControl)
end_time <- Sys.time()
end_time - start_time
print(decisionTree_model)
cm<-confusionMatrix(decisionTree_model)
compute_metrics(cm)
plot(decisionTree_model)
#*****KNN ********
start_time <- Sys.time()
trControl <- trainControl(method  = "cv",number  = 5)
set.seed(3333)
knn_model <- train(mood ~ .,data= final_data,method = "kknn",trControl  = trControl)
end_time <- Sys.time()
end_time - start_time
print(knn_model)
cm<-confusionMatrix(knn_model)
compute_metrics(cm)
plot(knn_model)

#****** SVM ********
start_time <- Sys.time()
trControl <- trainControl(method="cv", number=5)
svm_model <-train (mood~., data=final_data, method= "lssvmRadial",trControl  = trControl)
end_time <- Sys.time()
end_time - start_time
print(svm_model)
cm<-confusionMatrix(svm_model)
compute_metrics(cm)
plot(svm_model)
summary(svm_model)

#*******LOGISTIC REGRESSION******
start_time <- Sys.time()
trControl <- trainControl(method="cv", number=5)
logisticReg_model <-train (mood~., data=final_data, method= "LogitBoost",trControl  = trControl)
end_time <- Sys.time()
end_time - start_time
print(logisticReg_model)
cm<-confusionMatrix(logisticReg_model)
compute_metrics(cm)
plot(logisticReg_model)

