#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
pimad <- read.csv("C:/Users/USER/Desktop/AMAN/pimad.csv")
nrow(pimad)  
set.seed(9850)
gp<-runif(nrow(pimad))
pimad<-pimad[order(gp),]
idx<-sample(nrow(pimad),round(0.7*nrow(pimad)),replace = FALSE)
idx <- createDataPartition(y = pimad$class, p = 0.7, list = FALSE)
train<-pimad[idx,]
test<-pimad[-idx,]
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

pimad_ctree <- ctree(class ~ .  ,data=train)
plot(pimad_ctree)
treepre <- predict(pimad_ctree,test)
confusionMatrix(test$class,treepre)
table(treepre,test$class)
head(pimad)
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
nrow(pimad)
#neuralnet
nrow(pimad)
library(nnet)
set.seed(9850)
train1<- sample(1:768,538)
test1 <- setdiff(1:768,train1)
ideal <- class.ind(pimad$class)
pimadANN = nnet(pimad[train1,-9], ideal[train1,], size=10, softmax=TRUE)
j <- predict(pimadANN, pimad[test1,-9], type="class")
nrow(train1)
pimad[test1,]$class
table(predict(pimadANN, pimad[test1,-9], type="class"),pimad[test1,]$class)
confusionMatrix(pimad[test1,]$class, j)