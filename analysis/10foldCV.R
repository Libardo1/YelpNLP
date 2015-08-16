load("../data/dataFin.rda")

# Load relevant packages
library(MASS)

# Create suitable subset for 10-fold cross-validation
set.seed(1)
data = dataFin[sample(1:nrow(dataFin), 10 * floor(nrow(dataFin) / 10)),]
threshold = seq(.99, .01, -.02)

# Logistic regression
logErr = matrix(0, ncol = 50, nrow = 10)
logSum = list()
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  logFit = glm(rating ~ ., data = data[-test,], family = binomial)
  logProb = predict(logFit, data[test,], type = "response")
  logPred = rep("low", length(logProb))
  for (j in 1:50) {
    logPred[logProb > threshold[j]] = "high"
    logErr[i, j] = 1 - mean(logPred == data[test, "rating"])
  }
  logSum[[i]] = summary(logFit)$coef
}
colnames(logErr) = threshold
logErr = apply(logErr, 2, mean)
logVars = matrix(0, nrow = 103, ncol = 10)
logVars = sapply(logSum, function(x) names(sort(x[,4])))
save(logErr, logSum, logVars, file = "../data/logistic.rda")

# Linear discriminant analysis
ldaErr = matrix(0, ncol = 50, nrow = 10)
ldaSum = list()
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  ldaFit = lda(rating ~ ., data = data[-test,])
  ldaProb = predict(ldaFit, data[test,], type = "response")$posterior[,2]
  ldaPred = rep("low", length(ldaProb))
  for (j in 1:50) {
    ldaPred[ldaProb > threshold[j]] = "high"
    ldaErr[i, j] = 1 - mean(ldaPred == data[test, "rating"])
  }
  ldaSum[[i]] = ldaFit
}
colnames(ldaErr) = threshold
ldaErr = apply(ldaErr, 2, mean)
save(ldaErr, ldaSum, file = "../data/LDA.rda")

# Quadratic discriminant analysis
qdaErr = matrix(0, ncol = 50, nrow = 10)
qdaSum = list()
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  qdaFit = qda(rating ~ ., data = data[-test,])
  qdaProb = predict(qdaFit, data[test,], type = "response")$posterior[,2]
  qdaPred = rep("low", length(qdaProb))
  for (j in 1:50) {
    qdaPred[qdaProb > threshold[j]] = "high"
    qdaErr[i, j] = 1 - mean(qdaPred == data[test, "rating"])
  }
  qdaSum[[i]] = qdaFit
}
colnames(qdaErr) = threshold
qdaErr = apply(qdaErr, 2, mean)
save(qdaErr, qdaSum, file = "../data/QDA.rda")