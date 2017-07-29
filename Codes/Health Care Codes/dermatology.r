#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
dermatology <- read.csv("C:/Users/USER/Desktop/AMAN/dermatology.csv")
nrow(dermatology)  
ncol(dermatology)
set.seed(9850)
gp<-runif(nrow(dermatology))
dermatology<-dermatology[order(gp),]
idx<-sample(nrow(dermatology),round(0.7*nrow(dermatology)),replace = FALSE)
idx <- createDataPartition(y = dermatology$class, p = 0.7, list = FALSE)
train<-dermatology[idx,]
test<-dermatology[-idx,]
svmmodel<-svm(class~.,train,kernel="radial")
psvm<-predict(svmmodel,test)
plot(psvm)
table(psvm,test$class)
psvm
confusionMatrix(test$class, psvm)

#naiv bayes

nbmodel<-naiveBayes(class~.,train, laplace = 2, eps = 1)
pnbm<-predict(nbmodel,test)
confusionMatrix(test$class, pnbm)
table(pnbm,test$class)

#Classification tree

dermatology_ctree <- ctree(class ~ .  ,data=train)
plot(dermatology_ctree)
treepre <- predict(dermatology_ctree,test)
confusionMatrix(test$class,treepre)
table(treepre,test$class)
head(dermatology)
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
nrow(dermatology)
#neuralnet
nrow(train)
nrow(dermatology)
library(nnet)
set.seed(9850)
train1<- sample(1:366,259)
test1 <- setdiff(1:366,train1)
ideal <- class.ind(dermatology$class)
dermatologyANN = nnet(dermatology[train1,-35], ideal[train1,], size=20, softmax=TRUE)
j <- predict(dermatologyANN, dermatology[test1,-35], type="class")
nrow(train1)
dermatology[test1,]$class
table(predict(dermatologyANN, dermatology[test1,-35], type="class"),dermatology[test1,]$class)
confusionMatrix(dermatology[test1,]$class, j)
test1
train1

