load("../data/dataFin.rda")

# Load relevant packages
library(randomForest)

# Prepare data for bootstrapping (sanely)
set.seed(271828)
data = dataFin[sample(1:nrow(dataFin), nrow(dataFin)),]
mTry = c(4, 7, 9, 10, 12, 15, 20, 30, 60, 93)

# Random forests
forestErr = matrix(0, nrow = 10, ncol = 10)
for (i in 1:10) {
  train = 1:5000 + nrow(data) * (i - 1) / 10
  test = 5001:6000 + nrow(data) * (i - 1) / 10
  for (j in 1:10) {
    treeRating = randomForest(rating ~ ., data[train,], importance = TRUE,
                             mTry = mTry[j])
    predTree = predict(treeRating, newdata = data[test,])
    forestErr[i, j] = 1 - mean((predTree == data[test, "rating"])^2)
  }
}
colnames(forestErr) = mTry
forestErr = apply(forestErr, 2, mean)
save(forestErr, file = "../data/forestErr.rda")