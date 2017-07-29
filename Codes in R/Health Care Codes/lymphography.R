#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
lymph <- read.csv("C:/Users/USER/Desktop/AMAN/lymphography.csv")
nrow(lymph)  
ncol(lymph)
set.seed(9850)
gp<-runif(nrow(lymph))
lymph<-lymph[order(gp),]
idx<-sample(nrow(lymph),round(0.7*nrow(lymph)),replace = FALSE)
idx <- createDataPartition(y = lymph$class, p = 0.7, list = FALSE)
train<-lymph[idx,]
test<-lymph[-idx,]
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

lymph_ctree <- ctree(class ~ .  ,data=train)
plot(lymph_ctree)
treepre <- predict(lymph_ctree,test)
confusionMatrix(test$class,treepre)
table(treepre,test$class)
head(lymph)
#Random Forest
train
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
nrow(lymph)
#neuralnet
nrow(lymph)
library(nnet)
set.seed(9850)
train1<- sample(1:148,75)
test1 <- setdiff(1:75,train1)
ideal <- class.ind(lymph$class)

lymphANN = nnet(lymph[train1,-19], ideal[train1,], size=10, softmax=TRUE)
j <- predict(lymphANN, lymph[test1,-19], type="class")
nrow(train1)
lymph[test1,]$class
table(predict(lymphANN, lymph[test1,-19], type="class"),lymph[test1,]$class)
confusionMatrix(lymph[test1,]$class, j)

