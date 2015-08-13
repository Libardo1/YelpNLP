#Random Forest and baggings
#Bagging(train = 1:100)

library(randomForest)
set.seed(1)
train = 1:100
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                        subset=train, mtry = 102, importance =TRUE) #,ntree =)
#1.how to determine ntree 2. there is 102 predictors 
#If ntree exists, I can get rid of the importance = TRUE


yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])

                   
table(yhat.bag, High.test)
mean(yhat.bag == High.test)




#Bagging(train = 1:200)
set.seed(2)
train = 1:200
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, importance =TRUE) #,ntree =)
#1.how to determine ntree 2. there is 102 predictors 
#If ntree exists, I can get rid of the importance = TRUE


yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])


table(yhat.bag, High.test)
mean(yhat.bag==High.test)

#Bagging(train = 1:500)
set.seed(3)
train = 1:500
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, importance =TRUE) #,ntree =)
#1.how to determine ntree 2. there is 102 predictors 
#If ntree exists, I can get rid of the importance = TRUE


yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])


table(yhat.bag, High.test)
mean(yhat.bag==High.test)



#Bagging(train = 1:1000)
set.seed(4)
train = sample(1:nrow(dataFin),1000)
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, importance =TRUE) #,ntree =)
#1.how to determine ntree 2. there is 102 predictors 
#If ntree exists, I can get rid of the importance = TRUE


yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])

table(yhat.bag, High.test)
mean(yhat.bag==High.test)

#Bagging(train = 1:5000)
set.seed(5)
train = sample(1:nrow(dataFin),5000)
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, importance =TRUE) #,ntree =)
#1.how to determine ntree 2. there is 102 predictors 
#If ntree exists, I can get rid of the importance = TRUE


yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])

table(yhat.bag, High.test)
mean(yhat.bag==High.test)

#Bagging(train = 1:10000)
set.seed(6)
train = sample(1:nrow(dataFin),10000)
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, importance =TRUE) #,ntree =)
#1.how to determine ntree 2. there is 102 predictors 
#If ntree exists, I can get rid of the importance = TRUE


yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])

table(yhat.bag, High.test)
mean(yhat.bag==High.test)


#Bagging(train = 1:1000, ntree =25)
set.seed(4)
train = sample(1:nrow(dataFin),1000)
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, n.tree = 100)
#1.how to determine ntree 2. there is 102 predictors 
#If ntree exists, I can get rid of the importance = TRUE


yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])

table(yhat.bag, High.test)
mean(yhat.bag==High.test)

#Bagging(train = 1:10000, ntree =100)
set.seed(4)
train = sample(1:nrow(dataFin),10000)
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, n.tree = 50)
#1.how to determine ntree 2. there is 102 predictors 
#If ntree exists, I can get rid of the importance = TRUE


yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])

table(yhat.bag, High.test)
mean(yhat.bag==High.test)

#n.tree does not affect the test error

#Random Forest
set.seed(3)
train = sample(1:nrow(dataFin), nrow(dataFin)/20-7)
dataFin = na.omit(dataFin)
dataFin$rating = ifelse(dataFin$rating=='high', 1,0)
rf.yelp = randomForest(rating~., data = dataFin, subset = train, 
                       mtry = 10, importance = TRUE)
                       
#how to determine ntree
#square root of 102 is approximately 10.
#If ntree exists, I can get rid of the importance = TRUE

yhat.rf = predict(rf.yelp, newdata = dataFin[-train,])
 


importance(rf.yelp)
#Check which variables are relatively important


table(yhat.rf,High.test)
mean(yhat.rf==High.test)

#rf train =5000, n.tree = 50
set.seed(4) 
train  =sample(1:nrow(dataFin),5000)
High.test = dataFin[-train,'rating']
rf.yelp = randomForest(rating~., data = dataFin, subset = train,
                       mtry = 10, n.tree = 50)

yhat.rf = predict(rf.yelp, newdata = dataFin[-train,])

im = importance(rf.yelp)
sort(importance(rf.yelp))

table(yhat.rf,High.test)
mean(yhat.rf==High.test)
View(sort(im[,c('MeanDecreaseGini')]))



#Boosting
dataFin$rating = ifelse(dataFin$rating == 'high', 1,0)
High.test = dataFin[-train,'rating']
library(gbm)
set.seed(1)
train = sample(1:nrow(dataFin), 100)
boost.yelp = gbm(rating~., data = dataFin[train,], distribution = 'adaboost', 
                 cv.folds = 5,
                 interaction.depth = 1,
                 verbose = FALSE,
                 n.tree=1000
                 )
                 
# Default of shrinkageis 0.001                 
# How to determine n.tree
# How to determine interaction.depth
# How to determine shrinkage
# I am not sure whether I have to add up verbose = F

summary(boost.yelp)

yhat.prob = predict(boost.yelp,newdata = dataFin[-train,]  
                    ,type = 'response' )

# Ask whether I have to add type = 'response'

yhat.boost =rep(0,length(yhat.prob))
yhat.boost[yhat.prob>0.7] = 1
                     




table(yhat.boost, High.test)
mean(yhat.boost==High.test)

#Boosting does not work


#SVM(kernal)
#train = 100
library(e1071)
set.seed(2)
train = sample(1:nrow(dataFin),100)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

#train = 500
library(e1071)
set.seed(2)
train = sample(1:nrow(dataFin),500)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])




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
