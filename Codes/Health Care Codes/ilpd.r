#svm here
library(class)
library(e1071)
library(caret)
library(party)
library(nnet)
ILPD <- read.csv("C:/Users/USER/Desktop/AMAN/ILPD.csv")
nrow(ILPD)  
set.seed(9850)
  gp<-runif(nrow(ILPD))
  ILPD<-ILPD[order(gp),]
  idx<-sample(nrow(ILPD),round(0.7*nrow(ILPD)),replace = FALSE)
  idx <- createDataPartition(y = ILPD$Class, p = 0.7, list = FALSE)
  train<-ILPD[idx,]
  test<-ILPD[-idx,]
  svmmodel<-svm(Class~.,train,kernel="sigmoid")
  psvm<-predict(svmmodel,test)
  #plot(psvm)
  table(psvm,test$Class)
  psvm
  confusionMatrix(test$Class, psvm)
  
  #naiv bayes
  
  nbmodel<-naiveBayes(Class~.,train, laplace = 2, eps = 1)
  pnbm<-predict(nbmodel,test)
  confusionMatrix(test$Class, pnbm)
  table(pnbm,test$Class)
  
  #Classification tree
  
  ilpd_ctree <- ctree(Class ~ .  ,data=train)
  plot(ilpd_ctree)
  treepre <- predict(ilpd_ctree,test)
  confusionMatrix(test$Class,treepre)
  table(treepre,test$Class)
  head(ilpd)
  #Random Forest
  
  rfmodel <- train(Class~.,train, method = "rf", prox = TRUE)
  prf<-predict(rfmodel,test)
  confusionMatrix(test$Class, prf)
  table(prf,test$Class)
  
  #Gradient Boosting
  
  gbmodel <- train(Class~.,train, method = "gbm", verbose = FALSE)
  yespgbm<-predict(gbmodel,test)
  confusionMatrix(test$Class, yespgbm)
  table(yespgbm,test$Class)
  
  #Combining predictors
  
  predDF <- data.frame(psvm, pnbm, prf, yespgbm, treepre,y = test$Class)
  combomodFit <- train(y ~ ., method = "gam", data = predDF)
  combopred <- predict(combomodFit, predDF)
  confusionMatrix(test$Class, combopred)
  table(combopred,test$Class)
  
  
  #neuralnet
  library(nnet)
  set.seed(9850)
  train1<- sample(1:579,406)
  test1 <- setdiff(1:579,train)
  test1
  ideal <- class.ind(ILPD$Class)
  ILPDANN = nnet(ILPD[train1,-11], ideal[train1,], size=10, softmax=TRUE)
  predict(ILPDANN, ILPD[train1,-11], type="class")
  j <- predict(ILPDANN, ILPD[test1,-11], type="class")
  j
  ILPD[test1,]$Class
  table(predict(ILPDANN, ILPD[test1,-11], type="class"),ILPD[test1,]$Class)
  confusionMatrix(ILPD[test1,]$Class, j)
  