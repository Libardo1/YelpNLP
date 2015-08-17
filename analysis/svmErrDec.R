# This script is solely for investigating RSS error of SVMs as a function of
# training set size. These results are not used to assess the predictive
# capability of SVMs. For that, please see the script named `svm.R`

load("../data/dataFin.rda")

# Load relevant packages
library(e1071)

# Randomize data for multiple validation set approach
set.seed(602214129)
data = dataFin[sample(1:nrow(dataFin), nrow(dataFin)),]
size = 2 ^ seq(0, 5) * 100

# Linear kernel
svmErrLinear = matrix(0, nrow = 10, ncol = 6)
for (j in 1:6) {
  for (i in 1:10) {
    train = 1:(1 + size[j]) + nrow(data) * (i - 1) / 10
    test = 5001:6000 + nrow(data) * (i - 1) / 10
    tuneOut = tune(svm, rating ~ ., data = data[train,], kernel = "linear", 
                   range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
    svmPred = predict(tuneOut$best.model, data[test,])
    svmErrLinear[i, j] = 1 - mean(svmPred == data[test, "rating"])
  }
}
svmErrLinear = apply(svmErrLinear, 2, mean)

# Radial kernel
svmErrRadial = matrix(0, nrow = 10, ncol = 6)
for (j in 1:6) {
  for (i in 1:10) {
    train = 1:(1 + size[j]) + nrow(data) * (i - 1) / 10
    test = 5001:6000 + nrow(data) * (i - 1) / 10
    tuneOut = tune(svm, rating ~ ., data = data[train,], kernel = "radial", 
                   range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
    svmPred = predict(tuneOut$best.model, data[test,])
    svmErrRadial[i, j] = 1 - mean(svmPred == data[test, "rating"])
  }
}
svmErrRadial = apply(svmErrRadial, 2, mean)

save(size, svmErrLinear, svmErrRadial, file = "../data/svmErrDec.rda")