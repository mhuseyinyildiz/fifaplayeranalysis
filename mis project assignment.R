fifa<-read.csv2("file:///C:/Users/teknosAA/Desktop/Mis 315/Final_Project_Data_Set(1).csv", sep=",", dec=".")
train<-fifa[1:1000,]
test<-fifa[1001:nrow(fifa),]
lm.fit=lm(eur_value~.,data=train)
summary(lm.fit)
testpredict<-predict(lm.fit, newdata=test)
#MSE
MSE<-(1/nrow(test))*sum((test$eur_value-testpredict)^2)
#f)
set.seed(1)
library(glmnet)
train.mat <- model.matrix(eur_value ~ ., data = train)[,-1]
test.mat <- model.matrix(eur_value ~ ., data = test)[,-1]
grid <- 10 ^ seq(10, -2, length = 100)
fit.ridge <- glmnet(train.mat, train$eur_value, alpha = 0, lambda = grid)

cv.ridge <- cv.glmnet(train.mat, train$eur_value, alpha = 0, lambda = grid)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge
plot(cv.ridge)

set.seed(1)
fit.lasso <- glmnet(train.mat, train$eur_value, alpha = 1, lambda = grid)
cv.lasso <- cv.glmnet(train.mat, train$eur_value, alpha = 1, lambda = grid)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
plot(cv.lasso)

#g)
predict(fit.lasso, s = bestlam.lasso, type = "coefficients")

plot(cv.lasso$glmnet.fit, "lambda", label=TRUE)

#h)
pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test.mat)
MSE2<-mean((pred.ridge - test$eur_value)^2)
MSE2

pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
MSE3<-mean((pred.lasso - test$eur_value)^2)
MSE3

#i)

library(tree)

set.seed(1)
tree.fifa = tree(eur_value~., data=train)
summary(tree.fifa)

cv.fifa=cv.tree(tree.fifa)
plot(cv.fifa$size,cv.fifa$dev,type='b')

prune.fifa=prune.tree(tree.fifa,best=8)
plot(prune.fifa)
text(prune.fifa,pretty=0)

#j)

yhat=predict(prune.fifa,newdata=test)
RMSE4<-sqrt(mean((yhat-test$eur_value)^2))
RMSE4

#k)

#random forest
library(randomForest)
set.seed(1)

cvtrain<-train[sample(nrow(train)),]
folds <- cut(seq(1,nrow(cvtrain)),breaks=10,labels=FALSE)

mse<-rep(NA,10)

for (l in 1:38){
  for(i in 1:10){
    set.seed(1)
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- cvtrain[testIndexes, ]
    trainData <- cvtrain[-testIndexes, ]
    
    model=randomForest(eur_value~., data=trainData,
                       mtry=l, n.trees=500)
    
    yhat = predict(model,newdata=testData,n.trees=500)
    mse[i]<-mean((yhat-testData$eur_value)^2)
    #Use the test and train data partitions however you desire...
  }
  print(mean(mse))
}

#l)
set.seed(1)
rf.train=randomForest(eur_value~.,data=train,
                      importance=TRUE, mtry=22)

windows()
importance(rf.train)
varImpPlot(rf.train)

#m)
yhat.rf = predict(rf.train,newdata=test)
mean((yhat.rf-test$eur_value)^2)

