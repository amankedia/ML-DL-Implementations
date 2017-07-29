#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
breast <- read.csv("C:/Users/USER/Desktop/AMAN/breastwis.csv")
nrow(breast)
set.seed(9850)
gp<-runif(nrow(breast))
breast
nrow(breast)
breast<-breast[order(gp),]
idx<-sample(nrow(breast),round(0.7*nrow(breast)),replace = FALSE)
idx <- createDataPartition(y = breast$class, p = 0.7, list = FALSE)
train<-breast[idx,]
train
nrow(train)
test<-breast[-idx,]
test
nrow(test)
svmmodel<-svm(class~.,train,kernel="sigmoid")
psvm<-predict(svmmodel,test)
plot(psvm)
head(breast)
table(psvm,test$class)
confusionMatrix(test$class, psvm)

#naiv bayes

nbmodel<-naiveBayes(class~.,train, laplace = 2, eps = 1)
pnbm<-predict(nbmodel,test)
confusionMatrix(test$class, pnbm)
table(pnbm,test$class)

#classification tree

breast_ctree <- ctree(class ~ .  ,data=train)
plot(breast_ctree)
treepre <- predict(breast_ctree,test)
confusionMatrix(test$class,treepre)
table(treepre,test$class)

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


library(nnet)
set.seed(9850)
train1<- sample(1:699,490)
train1
test1 <- setdiff(1:699,train1)
test1
ideal <- class.ind(breast$class)
ILPDANN = nnet(breast[train1,-10], ideal[train1,], size=10, softmax=TRUE)
predict(breastANN, breast[train1,-10], type="class")
j <- predict(breastANN, breast[test1,-10], type="class")
j
breast[test1,]$class
table(predict(breastANN, breast[test1,-11], type="class"),breast[test1,]$class)
confusionMatrix(breast[test1,]$class, j)
