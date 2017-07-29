#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
cleveland <- read.csv("C:/Users/USER/Desktop/AMAN/cleveland.csv")
nrow(cleveland)  
ncol(cleveland)
set.seed(9850)
gp<-runif(nrow(cleveland))
cleveland<-cleveland[order(gp),]
idx<-sample(nrow(cleveland),round(0.7*nrow(cleveland)),replace = FALSE)
idx <- createDataPartition(y = cleveland$class, p = 0.7, list = FALSE)
train<-cleveland[idx,]
test<-cleveland[-idx,]
svmmodel<-svm(class~.,train,kernel="radial",scale= TRUE)
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

cleveland_ctree <- ctree(class ~ .  ,data=train)
plot(cleveland_ctree)
treepre <- predict(cleveland_ctree,test)
confusionMatrix(test$class,treepre)
table(treepre,test$class)
head(cleveland)
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
nrow(cleveland)
nrow(train)
#neuralnet
nrow(cleveland)
library(nnet)
set.seed(9850)
train1<- sample(1:303,213)
test1 <- setdiff(1:303,train1)
ideal <- class.ind(cleveland$class)
clevelandANN = nnet(cleveland[train1,-14], ideal[train1,], size=10, na.action = "na.omit", softmax=TRUE)
j <- predict(clevelandANN, cleveland[test1,-36], type="class")
nrow(train1)
cleveland[test1,]$class
table(predict(clevelandANN, cleveland[test1,-36], type="class"),cleveland[test1,]$class)
confusionMatrix(cleveland[test1,]$class, j)
test1
train1

