load("../data/dataFin.rda")

# Load relevant packages
library(MASS)

# Create suitable subset for 10-fold cross-validation
set.seed(1)
data = dataFin[sample(1:nrow(dataFin), 10 * floor(nrow(dataFin) / 10)),]
threshold = seq(.95, .05, -.05)

# Logistic regression
logErr = matrix(0, ncol = 19, nrow = 10)
logSum = list()
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  logFit = glm(rating ~ ., data = data[-test,], family = binomial)
  logProb = predict(logFit, data[test,], type = "response")
  logPred = rep("low", length(logProb))
  for (j in 1:19) {
    logPred[logProb > threshold[j]] = "high"
    logErr[i, j] = 1 - mean(logPred == data[test, "rating"])
  }
  logSum[[i]] = summary(logFit)$coef
}
colnames(logErr) = threshold
logVars = matrix(0, nrow = 103, ncol = 10)
logVars = sapply(logSum, function(x) names(sort(x[,4])))
save(logErr, logSum, logVars, file = "data/logistic.rda")

# Linear discriminant analysis
ldaErr = matrix(0, ncol = 19, nrow = 10)
ldaSum = list()
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  ldaFit = lda(rating ~ ., data = data[-test,])
  ldaPred = rep("low", length(ldaProb))
  for (j in 1:19) {
    ldaPred[ldaProb > threshold[j]] = "high"
    ldaErr[i, j] = 1 - mean(ldaPred == data[test, "rating"])
  }
  ldaSum[[i]] = ldaFit
}
save(ldaErr, ldaSum, file = "data/LDA.rda")

# Quadratic discriminant analysis
qdaErr = matrix(0, ncol = 19, nrow = 10)
qdaSum = list()
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  qdaFit = qda(rating ~ ., data = data[-test,])
  qdaPred = rep("low", length(qdaProb))
  for (j in 1:19) {
    qdaPred[qdaProb > threshold[j]] = "high"
    qdaErr[i, j] = 1 - mean(qdaPred == data[test, "rating"])
  }
  qdaSum[[i]] = qdaFit
}
save(qdaErr, qdaSum, file = "data/QDA.rda")