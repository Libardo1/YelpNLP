load("../data/dataRev.rda")

# Load relevant packages
library(glmnet)
library(leaps)

# Create appropriate dummy variables
set.seed(1)
test = sample(1:nrow(dataRev), floor(nrow(dataRev) / 10))
x = model.matrix(rating ~ ., dataRev)
y = as.numeric(dataRev$rating) - 1

# Forward and backward stepwise selection
fwdSelect = regsubsets(rating ~ ., dataRev, really.big = TRUE, nvmax = 102,
                          method = "forward")
bwdSelect = regsubsets(rating ~ ., dataRev, really.big = TRUE, nvmax = 102,
                          method = "backward")

# Subset selection by validation set approach
fwdErr = numeric(nrow(dataRev))
for (i in 1:(ncol(dataRev) - 1)) {
  coefi = coef(fwdSelect, i)
  pred = x[test, names(coefi)] %*% coefi
  fwdErr[i] = mean((y[test] - pred)^2)
}
bwdErr = numeric(nrow(dataRev))
for (i in 1:(ncol(dataRev) - 1)) {
  coefi = coef(fwdSelect, i)
  pred = x[test, names(coefi)] %*% coefi
  bwdErr[i] = mean((y[test] - pred)^2)
}
save(fwdSelect, bwdSelect, fwdErr, bwdErr, file = "subSelect.rda")

# # Ridge regression
# grid = 10^seq(10, -2, length = 100)
# ridge = glmnet(x[-test], y[-test], alpha = 0, lambda = grid, thresh = 1e-12)
# cvRidge = cv.glmnet(x[-test,], y[-test,], alpha = 0)
# bestLambda = cvRidge$lambda.min
# ridgePred = predict(ridge, s = bestLambda, newx = x[test,])
# ridgeErr = ((ridgePred - y[test])^2)
# save(ridgeErr, file = "ridge.rda")
# 
# # The lasso
# lasso = glmnet(x[-test], y[-test], alpha = 1, lambda = grid)
# cvLasso = cv.glmnet(x[-test,], y[-test,], alpha = 1)
# bestLambda = cvLasso$lambda.min
# lassoPred = predict(lasso, s = bestLambda, newx = x[test,])
# lassoErr = ((lassoPred - y[test])^2)
# save(lassoErr, file = "lasso.rda")