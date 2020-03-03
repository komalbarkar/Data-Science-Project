final_data1 = read.csv("C:\\Users\\admin\\Documents\\dataframe.csv")

final_data = final_data1[,-1]
colnames(final_data)

#############################

TenYearCHD = factor(ifelse(final_data$V1==1 , "Y" ,"N"))

final_data2 = cbind(final_data , TenYearCHD)
final_data3 = final_data2[,-1]

##############################

set.seed(2019)
library(caret)
intrain = createDataPartition(final_data3$TenYearCHD , p=0.7 , list =F)

training = final_data3[intrain,]
validation = final_data3[-intrain,]

##############################

fitknn1 = knn3(TenYearCHD ~ . ,data = training , k=1)
pred.knn1 = predict(fitknn1 , newdata = validation , type = "class")
tbl_1=table(pred.knn1 , validation$TenYearCHD)
confusionMatrix(tbl_1)


fitknn3 = knn3(TenYearCHD ~ . ,data = training , k=3)
pred.knn3 = predict(fitknn3 , newdata = validation , type = "class")
tbl_3=table(pred.knn3 , validation$TenYearCHD)
confusionMatrix(tbl_3)

fitknn5 = knn3(TenYearCHD ~ . ,data = training , k=5)
pred.knn5 = predict(fitknn5 , newdata = validation , type = "class")
tbl_5=table(pred.knn5 , validation$TenYearCHD)
confusionMatrix(tbl_5)

fitknn7 = knn3(TenYearCHD ~ . ,data = training , k=7)
pred.knn7 = predict(fitknn7 , newdata = validation , type = "class")
tbl_7=table(pred.knn7 , validation$TenYearCHD)
confusionMatrix(tbl_7)

fitknn11 = knn3(TenYearCHD ~ . ,data = training , k=11)
pred.knn11 = predict(fitknn11 , newdata = validation , type = "class")
tbl_11=table(pred.knn11 , validation$TenYearCHD)
confusionMatrix(tbl_11)

fitknn13 = knn3(TenYearCHD ~ . ,data = training , k=13)
pred.knn13 = predict(fitknn13 , newdata = validation , type = "class")
tbl_13=table(pred.knn13, validation$TenYearCHD)
confusionMatrix(tbl_13)

fitknn17 = knn3(TenYearCHD ~ . ,data = training , k=17)
pred.knn17 = predict(fitknn17 , newdata = validation , type = "class")
tbl_17=table(pred.knn17, validation$TenYearCHD)
confusionMatrix(tbl_17)

fitknn19 = knn3(TenYearCHD ~ . ,data = training , k=19)
pred.knn19 = predict(fitknn19 , newdata = validation , type = "class")
tbl_19=table(pred.knn19, validation$TenYearCHD)
confusionMatrix(tbl_19)


#########################################

library(pROC)

pred.prob1 <- predict(fitknn1,newdata=validation,type="prob")
plot.roc(validation$TenYearCHD, pred.prob1[,1], print.auc=TRUE ,
         col="magenta", main="K=1",legacy.axes=TRUE)


pred.prob3 <- predict(fitknn3,newdata=validation,type="prob")
plot.roc(validation$TenYearCHD, pred.prob1[,1], print.auc=TRUE ,
         col="magenta", main="K=3",legacy.axes=TRUE)


pred.prob5 <- predict(fitknn5,newdata=validation,type="prob")
plot.roc(validation$TenYearCHD, pred.prob1[,1], print.auc=TRUE ,
         col="magenta", main="K=5",legacy.axes=TRUE)

pred.prob7 <- predict(fitknn7,newdata=validation,type="prob")
plot.roc(validation$TenYearCHD, pred.prob1[,1], print.auc=TRUE ,
         col="magenta", main="K=7",legacy.axes=TRUE)


pred.prob11 <- predict(fitknn11,newdata=validation,type="prob")
plot.roc(validation$TenYearCHD, pred.prob1[,1], print.auc=TRUE ,
         col="magenta", main="K=11",legacy.axes=TRUE)


pred.prob13 <- predict(fitknn13,newdata=validation,type="prob")
plot.roc(validation$TenYearCHD, pred.prob1[,1], print.auc=TRUE ,
         col="magenta", main="K=13",legacy.axes=TRUE)

pred.prob17<- predict(fitknn17,newdata=validation,type="prob")
plot.roc(validation$TenYearCHD, pred.prob1[,1], print.auc=TRUE ,
         col="magenta", main="K=17",legacy.axes=TRUE)


##############################
##### ALL IN ONE #############

plot.roc(validation$TenYearCHD, pred.prob1[,1] , col="magenta",
         main = "ROC for Each K",legacy.axes=TRUE)

plot.roc(validation$TenYearCHD, pred.prob3[,1] , col="blue" , add=TRUE )

plot.roc(validation$TenYearCHD, pred.prob5[,1] , col="red" , add=TRUE )

plot.roc(validation$TenYearCHD, pred.prob7[,1] , col="green" , add=TRUE )

plot.roc(validation$TenYearCHD, pred.prob11[,1] , col="yellow" , add=TRUE )

plot.roc(validation$TenYearCHD, pred.prob13[,1] , col="pink" , add=TRUE )

plot.roc(validation$TenYearCHD, pred.prob17[,1] , col="black" , add=TRUE )

legend("bottomright", legend=c("K=1","K=3", "K=5","K=7","K=11","K=13","K=17"),
       col=c("magenta", "blue", "red","green","yellow","pink","black"), lwd=3)


############### Alternative II (ROC) #######################

library(caTools)
colAUC(pred.prob5[,1], validation$TenYearCHD, plotROC = TRUE)

### ROC AUC using MLMetrics
library(MLmetrics)
validNum <- ifelse(validation$TenYearCHD == "Y",1,0)

## K = 5

AUC(y_pred = pred.prob5[,1],y_true = validNum)


