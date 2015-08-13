
#SVM(linear)
#train=100

set.seed(2)
train = sample(1:nrow(dataFin),100)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

#train=500

set.seed(2)
train = sample(1:nrow(dataFin),500)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

#train=1000

set.seed(2)
train = sample(1:nrow(dataFin),1000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

#train=2000

set.seed(2)
train = sample(1:nrow(dataFin),2000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

#train=3000

set.seed(2)
train = sample(1:nrow(dataFin),3000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

#train = 4000
set.seed(2)
train = sample(1:nrow(dataFin),4000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])


#CV(k=10)

glm.fit = glm(rating~.,data = dataFin, family = binomial)
# I am not sure whether I have to add up type = 'class'
library(boot)
cv.error = cv.glm(dataFin, glm.fit,K = 10)$delta[1]

#Boosting(Roocv)
library(ROCR)
rocplot = function(pred,truth){
  
}
