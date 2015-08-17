load("../data/dataFin.rda")

# Load relevant packages
library(e1071)

# Randomize data for multiple validation set approach
set.seed(314159)
data = dataFin[sample(1:nrow(dataFin), nrow(dataFin)),]

# Linear kernel
svmErrLinear = numeric(10)
for (i in 1:10) {
  train = 1:5000 + nrow(data) * (i - 1) / 10
  test = 5001:6000 + nrow(data) * (i - 1) / 10
  tuneOut = tune(svm, rating ~ ., data = data[train,], kernel = "linear", 
                 range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
  svmPred = predict(tuneOut$best.model, data[test,])
  svmErrLinear[i] = 1 - mean(svmPred == data[test, "rating"])
}
svmErrLinear = mean(svmErrLinear)

# Radial kernel
svmErrRadial = numeric(10)
for (i in 1:10) {
train = 10001:15000 + nrow(data) * (i - 1) / 10
  test = 15001:16000 + nrow(data) * (i - 1) / 10
  tuneOut = tune(svm, rating ~ ., data = data[train,], kernel = "radial", 
                 range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
  svmPred = predict(tuneOut$best.model, data[test,])
  svmErrRadial[i] = 1 - mean(svmPred == data[test, "rating"])
}
svmErrRadial = mean(svmErrRadial)

save(svmErrLinear, svmErrRadial, file = "../data/svmErr.rda")