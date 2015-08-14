dataFin =na.omit(dataFin)
# Omit NA values

#Logistic Regression (find which variable are statisticallysignificant)

glm.fit = glm(rating ~., data = dataFin, family = binomial)
summary(glm.fit) # It seems all varibles are statistically significant.



#Logistic Regression(Train Set)


set.seed(1)
train =  sample(1:nrow(dataFin),nrow(dataFin)/2-1)
glm.fit2 = glm(rating ~., data = dataFin[train,], family = binomial)
glm.prob = predict(glm.fit2, dataFin[train,], type= 'response')
glm.pred = rep("low",length(glm.prob))
glm.pred[glm.prob>0.5] = 'high'
table(glm.pred, dataFin$rating[train])
mean(glm.pred==dataFin$rating[train])
trainingErrorLR = 1 - mean(glm.pred==dataFin$rating[train])
trainingErrorLR

#Logistic Regreesion(Test Set)

glm.prob2 = predict(glm.fit2, dataFin[-train,], type = 'response')
glm.pred2 = rep('low', length(glm.prob2))
glm.pred2[glm.prob2>0.5]='high'
table(glm.pred2,dataFin[-train,'rating'])
mean(glm.pred2==dataFin[-train,'rating'])
testErrorLR = 1 - mean(glm.pred2==dataFin[-train,'rating'])
testErrorLR


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#LDA(Training set)
library(MASS)
set.seed(1)

lda.fit = lda(rating ~ ., data = dataFin)
lda.pred = predict(lda.fit)
lda.class = lda.pred$class
table(lda.class, dataFin$rating)
mean(lda.class==dataFin$rating)
trainErrorLDA = 1 - mean(lda.class==dataFin$rating)
trainErrorLDA

#LDA(Test Set)
library(MASS)
set.seed(1)
lda.fit = lda(rating ~ ., data = dataFin, subset = train)
lda.pred = predict(lda.fit, dataFin[-train,])
lda.class = lda.pred$class
table(lda.class, dataFin[-train,'rating'])
mean(lda.class==dataFin[-train,'rating'])
testErrorLDA = 1 - mean(lda.class==dataFin[-train,'rating'])
testErrorLDA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##QDA(Train Set)
set.seed(1)
qda.fit = qda(rating~., data = dataFin)
qda.class = predict(qda.fit)$class
table(qda.class, dataFin$rating)
mean(qda.class==dataFin$rating)
trainErrorQDA = 1 - mean(qda.class==dataFin$rating)
trainErrorQDA

#QDA(Test Set)
set.seed(1)
qda.fit = qda(rating~., data = dataFin, subset = train)
qda.class = predict(qda.fit, dataFin[-train,])$class
table(qda.class, dataFin[-train,'rating'])
mean(qda.class==dataFin[-train,'rating'])
testErrorQDA = 1 - mean(qda.class==dataFin[-train,'rating'])
testErrorQDA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Bagging
#Determine how many training observations we should choose


#Bagging(Train Observations =100)
library(randomForest)
set.seed(2)
train = sample(1:nrow(dataFin), 100)
High.test = dataFin[-train,'rating']
bag.yelp100=randomForest(rating~., data = dataFin,
                         subset=train, mtry = 102, importance =TRUE)

yhat.bag100 = predict(bag.yelp100, newdata = dataFin[-train,])


table(yhat.bag100, High.test)
mean(yhat.bag100==High.test)
testErrorBG100 = 1 - mean(yhat.bag100==High.test)
testErrorBG100


#Bagging(Train Observations = 500)
set.seed(3)
train = sample(1:nrow(dataFin), 500)
High.test = dataFin[-train,'rating']
bag.yelp500=randomForest(rating~., data = dataFin,
                         subset=train, mtry = 102, importance =TRUE)

yhat.bag500 = predict(bag.yelp500, newdata = dataFin[-train,])


table(yhat.bag500, High.test)
mean(yhat.bag500==High.test)
testErrorBG500 = 1 - mean(yhat.bag500==High.test)
testErrorBG500 


#Bagging(Train Observations = 1000)
set.seed(4)
train = sample(1:nrow(dataFin), 1000)
High.test = dataFin[-train,'rating']
bag.yelp1000=randomForest(rating~., data = dataFin,
                          subset=train, mtry = 102, importance =TRUE)

yhat.bag1000 = predict(bag.yelp1000, newdata = dataFin[-train,])


table(yhat.bag1000, High.test)
mean(yhat.bag1000==High.test)
testErrorBG1000 = 1 - mean(yhat.bag1000==High.test)
testErrorBG1000 



#Bagging(Train Observations = 5000)
set.seed(5)
train = sample(1:nrow(dataFin), 5000)
High.test = dataFin[-train,'rating']
bag.yelp5000=randomForest(rating~., data = dataFin,
                          subset=train, mtry = 102, importance =TRUE)

yhat.bag5000 = predict(bag.yelp5000, newdata = dataFin[-train,])


table(yhat.bag5000, High.test)
mean(yhat.bag5000==High.test)
testErrorBG5000 = 1 - mean(yhat.bag5000==High.test)
testErrorBG5000 

#Bagging(Train Observations = 10000)
set.seed(6)
train = sample(1:nrow(dataFin), 10000)
High.test = dataFin[-train,'rating']
bag.yelp10000=randomForest(rating~., data = dataFin,
                           subset=train, mtry = 102, importance =TRUE)

yhat.bag10000 = predict(bag.yelp10000, newdata = dataFin[-train,])


table(yhat.bag10000, High.test)
mean(yhat.bag10000==High.test)
testErrorBG10000 = 1 - mean(yhat.bag10000==High.test)
testErrorBG10000 




#Bagging(Train Observations = 15000)
set.seed(7)
train = sample(1:nrow(dataFin), 15000)
High.test = dataFin[-train,'rating']
bag.yelp15000=randomForest(rating~., data = dataFin,
                           subset=train, mtry = 102, importance =TRUE)

yhat.bag15000 = predict(bag.yelp15000, newdata = dataFin[-train,])


table(yhat.bag15000, High.test)
mean(yhat.bag15000==High.test)
testErrorBG15000 = 1 - mean(yhat.bag15000==High.test)
testErrorBG15000 

#make a graph to choose number of observation
testErrorBG = c(testErrorBG100,testErrorBG500,testErrorBG1000, testErrorBG5000, testErrorBG10000, testErrorBG15000)

numberBG = c(100,500,1000,5000,10000,15000)

plot(x = numberBG, y = testErrorBG, type = "b", col = 'red', xlab = 'Number of Observation', ylab = 'Test Error')


#According to the graph, the test error is decrasing as number of observation is increasing until 5000.
#The test error line is relative flat when the number of observatin is more than 5000. 
#If we choose 15000, it spent more than 45 minitues and the there are no diffirences between two test errors. 
#Although the test error line is flat between 5000 and 10000, more information is better in general. 
#Also, it does not spent a long time running for number of observation is 10000.
#Therefore, we choose number of observation = 10000

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Now, we have to check whether n.tree influence the test error rate.


#Bagging(Train Set= 10000, n.tree =100)
set.seed(6)
train = sample(1:nrow(dataFin),10000)
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, n.tree = 100)

yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])

table(yhat.bag, High.test)
mean(yhat.bag==High.test)

testErrorBG10000_100 = 1 - mean(yhat.bag==High.test)
testErrorBG10000_100


#Bagging(Train = 10000, n.tree = 1000)
set.seed(6)
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, n.tree = 1000)

yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])

table(yhat.bag, High.test)
mean(yhat.bag==High.test)

testErrorBG10000_1000 = 1 - mean(yhat.bag==High.test)
testErrorBG10000_1000


#Bagging(Train Set= 10000, n.tree = 5000)
set.seed(6)
High.test = dataFin[-train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, n.tree = 5000)

yhat.bag = predict(bag.yelp, newdata = dataFin[-train,])

table(yhat.bag, High.test)
mean(yhat.bag==High.test)

testErrorBG10000_5000 = 1 - mean(yhat.bag==High.test)
testErrorBG10000_5000

#According to the three value, there are not diffirences bewteen three n.tree.
#The three test errors are approximately 22.7%
#We let n.tree default and use importance = TRUE

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Therefore, the training observation = 10000, n.tree is default. 
#Bagging(train Observation = 10000, n.tree is default)(Training Error)

set.seed(6)
train = sample(1:nrow(dataFin),10000)
High.train = dataFin[train,'rating']
bag.yelp=randomForest(rating~., data = dataFin,
                      subset=train, mtry = 102, importance =TRUE)

yhat.bag = predict(bag.yelp)

table(yhat.bag, High.train)
mean(yhat.bag==High.train)
trainBagging = 1-mean(yhat.bag==High.train)
trainBagging

#Bagging(Train Observations = 10000)(Test Error)
set.seed(6)
High.test = dataFin[-train,'rating']
bag.yelp10000=randomForest(rating~., data = dataFin,
                           subset=train, mtry = 102, importance =TRUE)

yhat.bag10000 = predict(bag.yelp10000, newdata = dataFin[-train,])


table(yhat.bag10000, High.test)
mean(yhat.bag10000==High.test)
testErrorBG10000 = 1 - mean(yhat.bag10000==High.test)
testErrorBG10000 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Random Forest
#Determine how many training observations we should choose


#Random Forest(Train Set = 100)
set.seed(8) 
train  =sample(1:nrow(dataFin),100)
High.test = dataFin[-train,'rating']
rf.yelp100 = randomForest(rating~., data = dataFin, subset = train,
                          mtry = 10, importance = TRUE)

yhat.rf100 = predict(rf.yelp100, newdata = dataFin[-train,])


table(yhat.rf100,High.test)
testErrorRF100 = 1 - mean(yhat.rf100==High.test)
testErrorRF100

#Random Forest(Train Set = 500)
set.seed(9) 
train  =sample(1:nrow(dataFin),500)
High.test = dataFin[-train,'rating']
rf.yelp500 = randomForest(rating~., data = dataFin, subset = train,
                          mtry = 10, importance = TRUE)

yhat.rf500 = predict(rf.yelp500, newdata = dataFin[-train,])


table(yhat.rf500,High.test)
testErrorRF500 = 1 - mean(yhat.rf500==High.test)
testErrorRF500



#Random Forest(Train Set =1000)
set.seed(1) 
train  =sample(1:nrow(dataFin),1000)
High.test = dataFin[-train,'rating']
rf.yelp1000 = randomForest(rating~., data = dataFin, subset = train,
                           mtry = 10, importance = TRUE)

yhat.rf1000 = predict(rf.yelp1000, newdata = dataFin[-train,])


table(yhat.rf1000,High.test)
testErrorRF1000 = 1 - mean(yhat.rf1000==High.test)
testErrorRF1000

#Random Forest(Train Set =5000)
set.seed(2) 
train  =sample(1:nrow(dataFin),5000)
High.test = dataFin[-train,'rating']
rf.yelp5000 = randomForest(rating~., data = dataFin, subset = train,
                           mtry = 10, importance = TRUE)

yhat.rf5000 = predict(rf.yelp5000, newdata = dataFin[-train,])

table(yhat.rf5000,High.test)
testErrorRF5000 = 1 - mean(yhat.rf5000==High.test)
testErrorRF5000

#Random Forest(Train Set =7000)
set.seed(1)  
train  =sample(1:nrow(dataFin),7000)
High.test = dataFin[-train,'rating']
rf.yelp7000 = randomForest(rating~., data = dataFin, subset = train,
                           mtry = 10, importance = TRUE)

yhat.rf7000 = predict(rf.yelp7000, newdata = dataFin[-train,])


table(yhat.rf7000,High.test)
testErrorRF7000 = 1 - mean(yhat.rf7000==High.test)
testErrorRF7000

#graphing
testErrorRF = c(testErrorRF100, testErrorRF500, testErrorRF1000, testErrorRF5000, testErrorRF7000)
observation = c(100,500,1000,5000,7000)

plot(x = observation, y = testErrorRF,
     type = 'b', xlab = 'number of observations',
     ylab = 'Test Error Rate'
)

#According to the graph, the test error is decreasing until n = 5000, 
#and the test error does not change a lot when n is greater than 5000. 
#Therefore, I choose 5000 observations as training set.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Now, we have to check whether n.tree influences the test error rate. 
#In the same time, we would check which variables are relatively important for this prediction.


#Random Forest(Train Set = 5000, n.tree = 25)
set.seed(11)
train = sample(1:nrow(dataFin), 5000)
High.test = dataFin[-train,'rating']

rf.yelp = randomForest(rating~., data = dataFin, subset = train,
                       mtry = 10, n.tree = 25)

yhat.rf = predict(rf.yelp, newdata = dataFin[-train,])

table(yhat.rf,High.test)
mean(yhat.rf==High.test)

im = importance(rf.yelp)
View(sort(im[,c('MeanDecreaseGini')]))



#Random Forest(Train Set = 5000, n.tree = 1000)
set.seed(11) 

rf.yelp = randomForest(rating~., data = dataFin, subset = train,
                       mtry = 10, n.tree = 1000)

yhat.rf = predict(rf.yelp, newdata = dataFin[-train,])

table(yhat.rf,High.test)
mean(yhat.rf==High.test)

im = importance(rf.yelp)
View(sort(im[,c('MeanDecreaseGini')]))




#Random Forest(Train Set = 5000, n.tree = 5000)
set.seed(11) 

rf.yelp = randomForest(rating~., data = dataFin, subset = train,
                       mtry = 10, n.tree = 5000)

yhat.rf = predict(rf.yelp, newdata = dataFin[-train,])

table(yhat.rf,High.test)
mean(yhat.rf==High.test)

im = importance(rf.yelp)
View(sort(im[,c('MeanDecreaseGini')]))

#According to the three values, the correct rates are approximately identical. 
#The n.tree does not influence the test error significantly.
#Hence, we let n.tree be default.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#According to the data above, training Observation should be 5000, and n.tree is default.

#Random Forest (Train Set = 5000)(Train Error)
set.seed(11) 
train  =sample(1:nrow(dataFin),5000)
High.train = dataFin[train,'rating']
rf.yelp = randomForest(rating~., data = dataFin, subset = train,
                       mtry = 10, importance = TRUE)

yhat.rf = predict(rf.yelp)

table(yhat.rf,High.train)
mean(yhat.rf==High.train)


trainErrorRF = 1- mean(yhat.rf==High.train)
trainErrorRF

#Random Forest (Train Set = 5000)(Test Error)
set.seed(11) 
High.test = dataFin[-train,'rating']
rf.yelp = randomForest(rating~., data = dataFin, subset = train,
                       mtry = 10, importance = TRUE)

yhat.rf = predict(rf.yelp, dataFin[-train,])

table(yhat.rf,High.test)
mean(yhat.rf==High.test)


testErrorRF = 1- mean(yhat.rf==High.test)
testErrorRF

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SVM(Radial Kernel)
#Determine how many training observations we should choose


#SVM(Radial Kernel)(Train Observations = 100)
library(e1071)
set.seed(20)
train = sample(1:nrow(dataFin),100)
tune.out = tune(svm,rating~., data = dataFin[train,], kernel = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmR100 = 1-mean(ypred==dataFin[-train,'rating'])
testErrorSvm1R00

#SVM(Radial Kernel)(Train Observations = 500)
library(e1071)
set.seed(21)
train = sample(1:nrow(dataFin),500)
tune.out = tune(svm,rating~., data = dataFin[train,], kernel = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmR500 = 1-mean(ypred==dataFin[-train,'rating'])
testErrorSvmR500

#SVM(Radial Kernel)(Train Observations = 1000)
library(e1071)
set.seed(22)
train = sample(1:nrow(dataFin),1000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernel = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmR1000 = 1-mean(ypred==dataFin[-train,'rating'])
testErrorSvmR1000

#SVM(Radial Kernel)(Train Observations = 2000)
library(e1071)
set.seed(23)
train = sample(1:nrow(dataFin),2000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernel = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))

ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmR2000 = 1- mean(ypred==dataFin[-train,'rating'])
testErrorSvmR2000

#graphing

plot(x = c(100,500,1000,2000), y = c(testErrorSvmR100,testErrorSvmR500,
                                     testErrorSvmR1000,testErrorSvmR2000),
     xlab = 'Number of Observations',
     ylab = 'Test Error Rate',
     type = 'b', col = 'red')

# According to the graph, the test error is decraesing as the number of observation is increasing until 2000
# If choose more than 2000 such as 3000, it spent more than an hour running the codes
# Therefore, we choose 2000 observations as training set.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Therefore, the training observations = 2000
#SVM(Radial Kernel)(Train Observations = 2000)(Train Error)


library(e1071)
set.seed(23)
train = sample(1:nrow(dataFin),2000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernel = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
ypred = predict(tune.out$best.model, dataFin[train,])
table(ypred,dataFin[train,'rating'])
mean(ypred==dataFin[train,'rating'])

trainErrorSvmR = 1- mean(ypred==dataFin[train,'rating'])
trainErrorSvmR



#SVM(Radial Kernel)(Train Observations = 2000)(Test Error)
library(e1071)
set.seed(23)
train = sample(1:nrow(dataFin),2000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernel = 'radial', 
                ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmR= 1- mean(ypred==dataFin[-train,'rating'])
testErrorSvmR

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SVM(linear)
##Determine how many training observations we should choose


#SVM(linear)(Training Observations = 100)
library(e1071)
set.seed(30)
train = sample(1:nrow(dataFin),100)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmL100 = 1 - mean(ypred==dataFin[-train,'rating'])
testErrorSvmL100



#SVM(linear)(Training Observations = 500)

set.seed(31)
train = sample(1:nrow(dataFin),500)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmL500 = 1 - mean(ypred==dataFin[-train,'rating'])
testErrorSvmL500

#SVM(linear)(Training Observations = 1000)

set.seed(32)
train = sample(1:nrow(dataFin),1000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmL1000 = 1 - mean(ypred==dataFin[-train,'rating'])
testErrorSvmL1000



#SVM(linear)(Training Observations = 2000)

set.seed(33)
train = sample(1:nrow(dataFin),2000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmL2000 = 1 - mean(ypred==dataFin[-train,'rating'])
testErrorSvmL2000



#SVM(linear)(Training Observations = 3000)

set.seed(34)
train = sample(1:nrow(dataFin),3000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmL3000 = 1 - mean(ypred==dataFin[-train,'rating'])
testErrorSvmL3000

#SVM(linear)(Training Observations = 4000)

set.seed(35)
train = sample(1:nrow(dataFin),4000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmL4000 = 1 - mean(ypred==dataFin[-train,'rating'])
testErrorSvmL4000
#graphing
plot(x = c(100,500,1000,2000,3000,4000), 
     y = c(testErrorSvmL100, testErrorSvmL500, testErrorSvmL1000, testErrorSvmL2000, testErrorSvmL3000, testErrorSvmL4000), 
     xlab = 'Number of Observatios',
     ylab = 'Test Error Rate',
     type = 'b', col = 'red')


# According to the graph, the test error is decreasing as the number of observation is decreasing until 2000.
# The line of the test error is just decreasing slightly when the number of observation is more than 2000(There is only 1% diffirence.)
# If we choose more than 3000, it spent more than 45 minutes to running the code.  
# Therefore, I choose 3000 as training set. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##SVM(linear)(Training Observations = 3000)(Training Error) 

set.seed(34)
train = sample(1:nrow(dataFin),3000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

ypred = predict(tune.out$best.model, dataFin[train,])
table(ypred,dataFin[train,'rating'])
mean(ypred==dataFin[train,'rating'])

trainErrorSvmL = 1 - mean(ypred==dataFin[train,'rating'])
trainErrorSvmL

#SVM(linear)(Training Observations = 3000)(Test Error)

set.seed(33)
train = sample(1:nrow(dataFin),3000)
tune.out = tune(svm,rating~., data = dataFin[train,], kernal ='linear', 
                range = list(cost = c(0.001,0.01,0.1,1,5,10,100)))

ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred,dataFin[-train,'rating'])
mean(ypred==dataFin[-train,'rating'])

testErrorSvmL = 1 - mean(ypred==dataFin[-train,'rating'])
testErrorSvmL

#Compare the test error and training error
LG = c(trainErrorLR, testErrorLR)
LDA = c(trainErrorLDA, testErrorLDA)
QDA = c(trainErrorQDA, testErrorQDA)
RF = c(trainErrorRF, testErrorRF)
BG = c(0.2266037,0.2361)
SvmL = c(trainErrorSvmL,0.1992929 )
SvmR = c(0.097,0.2217504)
all = c(LG, LDA, QDA, RF, BG, SvmL,SvmR)

barplot(all,col = c('aquamarine3', 'coral'))

legend('topleft', c('Train', 'Test'), pch = 15, col = c('aquamarine3', 'coral'), bty = 'n')
  
  
  
  
  
  
  
  
  
  
  






