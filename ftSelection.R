load("data/dataFin.rda")

# Load relevant packages
library(glmnet)
library(leaps)

# Forward and backward stepwise selection
fwdSelection = regsubsets(rating ~ ., dataFin, really.big = TRUE,
                          method = "forward")
bwdSelection = regsubsets(rating ~ ., dataFin, really.big = TRUE,
                          method = "backward")
save(fwdSelection, bwdSelection, file = "temp.rda")
# 
# # Create appropriate dummy variables
# set.seed(1)
# test = sample(1:nrow(dataFin), floor(nrow(dataFin) / 10))
# x = model.matrix(rating ~ ., dataFin)
# y = as.numeric(dataFin$rating) - 1
# 
# # Check with a validation set
# fwdErr = numeric(ncol(dataFin) - 1)
# for (i in 1:(ncol(dataFin) - 1)) {
#   coefi = coef(fwdSelection, id = i)
#   pred = x[test, names(coefi)] %*% coefi
#   fwdErr[i] = mean((y[test] - pred)^2)
# }
# bwdErr = numeric(ncol(dataFin) - 1)
# for (i in 1:(ncol(dataFin) - 1)) {
#   coefi = coef(bwdSelection, id = i)
#   pred = x[test, names(coefi)] %*% coefi
#   bwdErr[i] = mean((y[test] - pred)^2)
# }
# save(fwdErr, bwdErr, file = "subset.rda")
# 
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