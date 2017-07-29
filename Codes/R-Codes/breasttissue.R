#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
breasttissue <- read.csv("C:/Users/USER/Desktop/AMAN/BreastTissue.csv")
nrow(breasttissue)  
set.seed(9850)
gp<-runif(nrow(breasttissue))
breasttissue<-breasttissue[order(gp),]
idx<-sample(nrow(breasttissue),round(0.7*nrow(breasttissue)),replace = FALSE)
idx <- createDataPartition(y = breasttissue$class, p = 0.7, list = FALSE)
train<-breasttissue[idx,]
test<-breasttissue[-idx,]
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

breasttissue_ctree <- ctree(class ~ .  ,data=train)
plot(breasttissue_ctree)
treepre <- predict(breasttissue_ctree,test)
confusionMatrix(test$class,treepre)
table(treepre,test$class)
head(breasttissue)
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
0.7*195
nrow(breasttissue)
#neuralnet
nrow(breasttissue)
library(nnet)
set.seed(9850)
train1<- sample(1:106,77)
test1 <- setdiff(1:106,train1)
ideal <- class.ind(breasttissue$class)
breasttissueANN = nnet(breasttissue[train1,-10], ideal[train1,], size=10, softmax=TRUE)
j <- predict(breasttissueANN, breasttissue[test1,-10], type="class")
nrow(train1)
breasttissue[test1,]$class
table(predict(breasttissueANN, breasttissue[test1,-10], type="class"),breasttissue[test1,]$class)
confusionMatrix(breasttissue[test1,]$class, j)
test1
train1

