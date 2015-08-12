#SVM train=100
library(e1071)
set.seed(2)
train = sample(1:nrow(dataFin),100)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])


#SVM train=1000
library(e1071)
set.seed(2)
train = sample(1:nrow(dataFin),1000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

#SVM train=2000
library(e1071)
set.seed(2)
train = sample(1:nrow(dataFin),2000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])


#SVM train=3000
library(e1071)
set.seed(2)
train = sample(1:nrow(dataFin),3000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])