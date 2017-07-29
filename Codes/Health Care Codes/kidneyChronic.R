#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
kidney <- read.csv("C:/Users/USER/Desktop/AMAN/kidneyChronic.csv")
nrow(kidney)  
set.seed(9850)
gp<-runif(nrow(kidney))
kidney<-kidney[order(gp),]
idx<-sample(nrow(kidney),round(0.7*nrow(kidney)),replace = FALSE)
idx <- createDataPartition(y = kidney$class, p = 0.7, list = FALSE)
train<-kidney[idx,]
test<-kidney[-idx,]
svmmodel<-svm(class~.,train,kernel="radial")
psvm<-predict(svmmodel,test)
#plot(psvm)
table(psvm,test$class)
psvm
confusionMatrix(test$class, psvm)

#naiv bayes

nbmodel<-naiveBayes(class~.,train, laplace = 2, eps = 1)
pnbm<-predict(nbmodel,test)
confusionMatrix(test$class, pnbm)
table(pnbm,test$class)

#Classification tree

kidney_ctree <- ctree(class ~ .  ,data=train)
plot(kidney_ctree)
treepre <- predict(kidney_ctree,test)
confusionMatrix(test$class,treepre)
table(treepre,test$class)
head(kidney)
#Random Forest
rfmodel <- train(class~.,train, method = "rf", prox = TRUE)
prf<-predict(rfmodel,test)
confusionMatrix(test$class, prf)
table(prf,test$class)

#Gradient Boosting

gbmodel <- train(class~.,train, method = "gbm", verbose = FALSE)
yespgbm<-predict(gbmodel,test)
confusionMatrix(test$class, yespgbm)
table(yespgbm,test$class)

#Combining predictors

predDF <- data.frame(psvm, pnbm, prf, yespgbm, treepre,y = test$class)
combomodFit <- train(y ~ ., method = "gam", data = predDF)
combopred <- predict(combomodFit, predDF)
confusionMatrix(test$class, combopred)
table(combopred,test$class)
nrow(train)
nrow(kidney)
#neuralnet
nrow(kidney)
library(nnet)
set.seed(9850)
train1<- sample(1:400,280)
test1 <- setdiff(1:400,train1)
ideal <- class.ind(kidney$class)
kidneyANN = nnet(kidney[train1,-25], ideal[train1,], size=10, na.action= "na.omit", softmax=TRUE)
j <- predict(kidneyANN, kidney[test1,-9], type="class")
nrow(train1)
kidney[test1,]$class
table(predict(kidneyANN, kidney[test1,-9], type="class"),kidney[test1,]$class)
confusionMatrix(kidney[test1,]$class, j)