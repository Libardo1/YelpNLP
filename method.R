#LR(training)

dataFin =na.omit(dataFin)


glm.fit = glm(rating ~., data = dataFin, family = binomial)
summary(glm.fit) # It seems all varibles works for this model.
glm.prob = predict(glm.fit,type= 'response')
glm.pred = rep("low",length(glm.prob))
glm.pred[glm.prob>0.5] = 'high'
table(glm.pred, dataFin$rating)
mean(glm.pred==dataFin$rating)
trainingError = 1 - mean(glm.pred==dataFin$rating)
trainingError

summary(glm.fit)$coef[,4]
#LR(Test Error rate)

set.seed(1)
train =  sample(1:nrow(dataFin),nrow(dataFin)/2-1)
glm.fit2 = glm(rating~.,data = dataFin, family = binomial, subset = train)
glm.prob2 = predict(glm.fit2, dataFin[-train,], type = 'response')
glm.pred2 = rep('low', length(glm.prob2))
glm.pred2[glm.prob2>0.5]='high'
table(glm.pred2,dataFin[-train,'rating'])
mean(glm.pred2==dataFin[-train,'rating'])
testError = 1 - mean(glm.pred2==dataFin[-train,'rating'])
testError

summary(glm.fit2)$coef[,4]

#LDA(test error)
library(MASS)
set.seed(1)

lda.fit = lda(rating ~ ., data = dataFin, subset = train)
lda.fit
lda.pred = predict(lda.fit, dataFin[-train,])
lda.class = lda.pred$class
table(lda.class, dataFin[-train,'rating'])
mean(lda.class==dataFin[-train,'rating'])
testError2 = 1 - mean(lda.class==dataFin[-train,'rating'])
testError2

#Applying a 50 % threshold to the posterior probabilities
#sum(lda.pred$posterior[,1]>=.5)
#sum(lda.pred$posterior[,1]<.5)


#QDA(test error)
set.seed(1)
qda.fit = qda(rating~., data = dataFin, subset = train)
qda.fit
qda.class = predict(qda.fit, dataFin[-train,])$class
table(qda.class, dataFin[-train,'rating'])
mean(qda.class==dataFin[-train,'rating'])
testError3 = 1 - mean(qda.class==dataFin[-train,'rating'])
testError3
"
#knn
set.seed(1)
library(class)
train.X = dataFin[train,-1]
test.X = dataFin[-train,-1]
train.rating = dataFin$rating[train]
test.rating = dataFin$rating[-train]

#k=1

knn.pred = knn(train.X,test.X,train.rating,k=1)
table(knn.pred, test.rating)
mean(knn.pred == test.rating)
testError = 1 - mean(knn.pred == test.rating) 

#k=5
knn.pred2 = knn(train.X,test.X,train.rating,k=10)
table(knn.pred2, test.rating)
mean(knn.pred2==test.rating)
testError = 1 - mean(knn.pred2 == test.rating) 

#k=10
knn.pred3 = knn(train.X,test.X,train.rating,k=100)
table(knn.pred3, test.rating)
mean(knn.pred3==test.rating)
testError = 1 - mean(knn.pred3 == test.rating) 
"


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
mean(yhat.bag==High.test)




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
