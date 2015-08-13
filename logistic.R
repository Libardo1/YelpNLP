load("dataFin.rda")

set.seed(1)
train =  sample(1:nrow(dataFin), nrow(dataFin)/2 - 1)

# Logistic Regression (Training)

glm.fit = glm(rating ~., data = dataFin[train,], family = binomial)
summary(glm.fit) # It seems all varibles works for this model.
glm.prob = predict(glm.fit, dataFin[train,], type= 'response')
glm.pred = rep("low",length(glm.prob))
glm.pred[glm.prob>0.5] = 'high'
table(glm.pred, dataFin$rating[train])
mean(glm.pred == dataFin$rating[train])
trainingError = 1 - mean(glm.pred == dataFin$rating[train])
trainingError
summary(glm.fit)$coef[,4]

# Logistic Regression (Test)

glm.prob2 = predict(glm.fit, dataFin[-train,], type = 'response')
glm.pred2 = rep('low', length(glm.prob2))
glm.pred2[glm.prob2>0.5] = 'high'
table(glm.pred2, dataFin[-train,'rating'])
mean(glm.pred2 == dataFin[-train,'rating'])
testError = 1 - mean(glm.pred2 == dataFin[-train,'rating'])
testError
summary(glm.fit2)$coef[,4]