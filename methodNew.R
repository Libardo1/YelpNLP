load("dataFin.rda")

# Load relevant packages
library(leaps)
library(MASS)

# Create suitable subset for 10-fold cross-validation
set.seed(1)
data = dataFin[sample(1:nrow(dataFin), 10 * floor(nrow(dataFin) / 10)),]

# Logistic regression
logErr = numeric(10)
logSum = list()
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  logFit = glm(rating ~ ., data = data[-test,], family = binomial)
  logProb = predict(logFit, data[test,], type = "response")
  logPred = rep("low", length(logProb))
  logPred[logPred > .5] = "high"
  logErr[i] = 1 - mean(logPred == data[test, "rating"])
  logSum[[i]] = summary(logFit)$coef
}
save(logErr, logSum, file = "logistic.rda")

# Linear discriminant analysis
ldaErr = numeric(10)
ldaSum = list()
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  ldaFit = lda(rating ~ ., data = data[-test,])
  ldaPred = predict(ldaFit, data[test,])$class
  ldaErr[i] = 1 - mean(ldaPred == data[test, "rating"])
  ldaSum[[i]] = ldaFit
}
save(ldaErr, ldaSum, file = "LDA.rda")

# Quadratic discriminant analysis
qdaErr = numeric(10)
qdaSum = list()
for (i in 1:10) {
  test = 1:(nrow(data) / 10) + nrow(data) * (i - 1) / 10
  qdaFit = qda(rating ~ ., data = data[-test,])
  qdaPred = predict(qdaFit, data[test,])$class
  qdaErr[i] = 1 - mean(qdaPred == data[test, "rating"])
  qdaSum[[i]] = qdaFit
}
save(qdaErr, qdaSum, file = "QDA.rda")