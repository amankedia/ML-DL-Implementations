#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
ILPD <- read.csv("C:/Users/USER/Desktop/AMAN/parkinsons.csv")
nrow(ILPD)  
set.seed(9850)
gp<-runif(nrow(ILPD))
ILPD<-ILPD[order(gp),]
idx<-sample(nrow(ILPD),round(0.7*nrow(ILPD)),replace = FALSE)
idx <- createDataPartition(y = ILPD$status, p = 0.7, list = FALSE)
train<-ILPD[idx,]


test<-ILPD[-idx,]
svmmodel<-svm(status~.,train,kernel="sigmoid")
psvm<-predict(svmmodel,test)
#plot(psvm)
table(psvm,test$status)
psvm
confusionMatrix(test$status, psvm)

#naiv bayes

nbmodel<-naiveBayes(status~.,train, laplace = 2, eps = 1)
pnbm<-predict(nbmodel,test)
confusionMatrix(test$status, pnbm)
table(pnbm,test$status)

#Classification tree

ilpd_ctree <- ctree(status ~ .  ,data=train)
plot(ilpd_ctree)
treepre <- predict(ilpd_ctree,test)
confusionMatrix(test$status,treepre)
table(treepre,test$status)
head(ILPD)
#Random Forest

rfmodel <- train(status~.,train, method = "rf", prox = TRUE)
prf<-predict(rfmodel,test)
confusionMatrix(test$status, prf)
table(prf,test$status)

#Gradient Boosting

gbmodel <- train(status~.,train, method = "gbm", verbose = FALSE)
yespgbm<-predict(gbmodel,test)
confusionMatrix(test$status, yespgbm)
table(yespgbm,test$status)

#Combining predictors

predDF <- data.frame(psvm, pnbm, prf, yespgbm, treepre,y = test$status)
combomodFit <- train(y ~ ., method = "gam", data = predDF)
combopred <- predict(combomodFit, predDF)
confusionMatrix(test$status, combopred)
table(combopred,test$status)

0.7*195
#neuralnet
nrow(ILPD)
library(nnet)
set.seed(9850)
train1<- sample(1:195,135)
test1 <- setdiff(train)
ideal <- class.ind(ILPD$status)
ILPDANN = nnet(ILPD[train1,-25], ideal[train1,], size=10, softmax=TRUE)
predict(ILPDANN, ILPD[train1,-11], type="class")
j <- predict(ILPDANN, ILPD[test1,-25], type="class")
j
ILPD[test1,]$Class
table(predict(ILPDANN, ILPD[test1,-11], type="class"),ILPD[test1,]$Class)
confusionMatrix(ILPD[test1,]$Class, j)
test1
train1
