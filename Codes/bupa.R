#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
bupa <- read.csv("C:/Users/USER/Desktop/AMAN/bupa.csv")
set.seed(9850)
gp<-runif(nrow(bupa))
bupa
nrow(bupa)
bupa<-bupa[order(gp),]
idx<-sample(nrow(bupa),round(0.7*nrow(bupa)),replace = FALSE)
idx <- createDataPartition(y = bupa$selector, p = 0.7, list = FALSE)
train<-bupa[idx,]
test<-bupa[-idx,]
svmmodel<-svm(selector~.,train,kernel="sigmoid")
psvm<-predict(svmmodel,test)
#plot(psvm)
table(psvm,test$selector)
confusionMatrix(test$selector, psvm)

#naiv bayes

nbmodel<-naiveBayes(selector~.,train, laplace = 2, eps = 1)
pnbm<-predict(nbmodel,test)
confusionMatrix(test$selector, pnbm)
table(pnbm,test$selector)

#selectorification tree

bupa_ctree <- ctree(selector ~ .  ,data=train)
plot(bupa_ctree)
treepre <- predict(bupa_ctree,test)
confusionMatrix(test$selector,treepre)
table(treepre,test$selector)

#Random Forest

rfmodel <- train(selector~.,train, method = "rf", prox = TRUE)
prf<-predict(rfmodel,test)
confusionMatrix(test$selector, prf)
table(prf,test$selector)

#Gradient Boosting

gbmodel <- train(selector~.,train, method = "gbm", verbose = FALSE)
yespgbm<-predict(gbmodel,test)
confusionMatrix(test$selector, yespgbm)
table(yespgbm,test$selector)

#Combining predictors

predDF <- data.frame(psvm, pnbm, prf, yespgbm, treepre,y = test$selector)
combomodFit <- train(y ~ ., method = "gam", data = predDF)
combopred <- predict(combomodFit, predDF)
confusionMatrix(test$selector, combopred)
table(combopred,test$selector)


#neuralnet
library(nnet)
set.seed(9850)

train1<- sample(1:345,241)
test1 <- setdiff(1:345,train1)
ideal <- class.ind(bupa$selector)
bupaANN = nnet(bupa[train1,-7], ideal[train1,], size=10, softmax=TRUE)
predict(bupaANN, bupa[train1,-7], type="class")
j <- predict(bupaANN, bupa[test1,-7], type="class")
j
bupa[test1,]$selector
table(predict(bupaANN, bupa[test1,-7], type="class"),bupa[test1,]$selector)
confusionMatrix(bupa[test1,]$selector, j)

