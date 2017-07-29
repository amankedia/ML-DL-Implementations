#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
hepatitis <- read.csv("C:/Users/USER/Desktop/AMAN/hepatitis.csv")

nrow(hepatitis)  
ncol(hepatitis)
set.seed(9850)
gp<-runif(nrow(hepatitis))
hepatitis<-hepatitis[order(gp),]
idx<-sample(nrow(hepatitis),round(0.7*nrow(hepatitis)),replace = FALSE)
idx <- createDataPartition(y = hepatitis$class, p = 0.7, list = FALSE)
train<-hepatitis[idx,]
test<-hepatitis[-idx,]
nrow(train)
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

hepatitis_ctree <- ctree(class ~ .  ,data=train)
plot(hepatitis_ctree)
treepre <- predict(hepatitis_ctree,test)
confusionMatrix(test$class,treepre)
table(treepre,test$class)
head(hepatitis)
#Random Forest
rfmodel <- train(class~.,train, method = "rf", prox = TRUE)
prf<-predict(rfmodel,test)
confusionMatrix(test$class, prf)
table(prf,test$class)

#Gradient Boosting
head(train)
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
ncol(train)
0.7*195
nrow(hepatitis)
#neuralnet
nrow(hepatitis)
library(nnet)
set.seed(9850)
train1<- sample(1:155,110)
test1 <- setdiff(1:110,train1)
ideal <- class.ind(hepatitis$class)
hepatitisANN = nnet(hepatitis[train1,-20], ideal[train1,], size=10, softmax=TRUE)
j <- predict(hepatitisANN, hepatitis[test1,-20], type="class")
nrow(train1)
hepatitis[test1,]$class
table(predict(hepatitisANN, hepatitis[test1,-20], type="class"),hepatitis[test1,]$class)
confusionMatrix(hepatitis[test1,]$class, j)
