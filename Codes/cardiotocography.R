#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
breasttissue <- read.csv("C:/Users/USER/Desktop/AMAN/ctg.csv")
nrow(breasttissue)  
ncol(breasttissue)
set.seed(9850)
gp<-runif(nrow(breasttissue))
breasttissue<-breasttissue[order(gp),]
idx<-sample(nrow(breasttissue),round(0.7*nrow(breasttissue)),replace = FALSE)
idx <- createDataPartition(y = breasttissue$NSP, p = 0.7, list = FALSE)
train<-breasttissue[idx,]
test<-breasttissue[-idx,]
svmmodel<-svm(NSP~.,train,kernel="sigmoid",scale= TRUE)
psvm<-predict(svmmodel,test)
plot(psvm)
table(psvm,test$NSP)
psvm
confusionMatrix(test$NSP, psvm)

#naiv bayes

nbmodel<-naiveBayes(NSP~.,train, laplace = 2, eps = 1)
pnbm<-predict(nbmodel,test)
confusionMatrix(test$NSP, pnbm)
table(pnbm,test$NSP)

#Classification tree

breasttissue_ctree <- ctree(NSP ~ .  ,data=train)
plot(breasttissue_ctree)
treepre <- predict(breasttissue_ctree,test)
confusionMatrix(test$NSP,treepre)
table(treepre,test$NSP)
head(breasttissue)
#Random Forest
rfmodel <- train(NSP~.,train, method = "rf", prox = TRUE)
prf<-predict(rfmodel,test)
confusionMatrix(test$NSP, prf)
table(prf,test$NSP)

#Gradient Boosting

gbmodel <- train(NSP~.,train, method = "gbm", verbose = FALSE)
yespgbm<-predict(gbmodel,test)
confusionMatrix(test$NSP, yespgbm)
table(yespgbm,test$NSP)

#Combining predictors

predDF <- data.frame(psvm, pnbm, prf, yespgbm, treepre,y = test$NSP)
combomodFit <- train(y ~ ., method = "gam", data = predDF)
combopred <- predict(combomodFit, predDF)
confusionMatrix(test$NSP, combopred)
table(combopred,test$NSP)
nrow(train)
0.7*195
nrow(breasttissue)
nrow(train)
#neuralnet
nrow(breasttissue)
library(nnet)
set.seed(9850)
train1<- sample(1:2126,1490)
test1 <- setdiff(1:2126,train1)
ideal <- class.ind(breasttissue$NSP)
breasttissueANN = nnet(breasttissue[train1,-36], ideal[train1,], size=10, softmax=TRUE)
j <- predict(breasttissueANN, breasttissue[test1,-36], type="class")
nrow(train1)
breasttissue[test1,]$NSP
table(predict(breasttissueANN, breasttissue[test1,-36], type="class"),breasttissue[test1,]$NSP)
confusionMatrix(breasttissue[test1,]$NSP, j)
test1
train1

