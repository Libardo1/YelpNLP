load("data/dataFin.rda")

# Load relevant packages
library(e1071)

# Create vector of numbers of observations to use
nObs = c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000)
set.seed(1)

# Linear kernel
svmErrLinear = numeric(length(nObs))
svmTimeLinear = numeric(length(nObs))
for (i in 1:length(nObs)) {
  startTime = proc.time()[3]
  train = sample(1:nrow(dataFin), nObs[i])
  tuneOut = tune(svm, rating ~ ., data = dataFin[train,], kernel = "linear", 
                 range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
  svmPred = predict(tuneOut$best.model, dataFin[-train,])
  table(svmPred, dataFin[-train, "rating"])
  svmErrLinear[i] = 1 - mean(svmPred == dataFin[-train, "rating"])
  svmTimeLinear[i] = proc.time()[3] - startTime
}

# Radial kernel
svmErrRadial = numeric(length(nObs))
svmTimeRadial = numeric(length(nObs))
for (i in 1:length(nObs)) {
  startTime = proc.time()[3]
  train = sample(1:nrow(dataFin), nObs[i])
  tuneOut = tune(svm, rating ~ ., data = dataFin[train,], kernel = "radial", 
                 range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
  svmPred = predict(tuneOut$best.model, dataFin[-train,])
  table(svmPred, dataFin[-train, "rating"])
  svmErrRadial[i] = 1 - mean(svmPred == dataFin[-train, "rating"])
  svmTimeRadial[i] = proc.time()[3] - startTime
}

save(svmErrLinear, svmTimeLinear, svmErrRadial, svmErrRadial,
     file = "data/SVM.rda")