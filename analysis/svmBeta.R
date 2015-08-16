load("../data/dataFin.rda")

# Load relevant packages
library(e1071)

# Create suitable subset for 10-fold cross-validation
set.seed(1)
data = dataFin[sample(1:nrow(dataFin), 10 * floor(nrow(dataFin) / 10)),]

# Linear kernel
svmErrLinear = numeric(10)
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  tuneOut = tune(svm, rating ~ ., data = data[-test,], kernel = "linear", 
                 range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
  svmPred = predict(tuneOut$best.model, dataFin[test,])
  svmErrLinear[i] = 1 - mean(svmPred == dataFin[test, "rating"])
}

# Radial kernel
svmErrRadial = numeric(10)
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  tuneOut = tune(svm, rating ~ ., data = data[-test,], kernel = "radial", 
                 range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
  svmPred = predict(tuneOut$best.model, dataFin[test,])
  svmErrRadial[i] = 1 - mean(svmPred == dataFin[test, "rating"])
}

save(svmErrLinear, svmErrRadial, file = "../data/SVM.rda")