load("dataFin.rda")

# SVM train = 800000
library(e1071)
set.seed(1)
train = sample(1:nrow(dataFin), 800000)
tune.out = tune(svm, rating ~ ., data = dataFin[train,], kernel = 'radial', 
                ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                              gamma = c(0.5, 1, 2, 3, 4)))
ypred = predict(tune.out$best.model, dataFin[-train,])
table(ypred, dataFin[-train, 'rating'])
mean(ypred == dataFin[-train, 'rating'])