#If high is more than low, it adds up to 1, otherwise is 0. If # of high is equal to # of low, it is 0.5
win = function(x){
  count = table(data$rating, x)
  total = 0
  for(i in 1:dim(count)[2]){
    
    if(count[1,i] < count[2,i]){
      
      total = total + 1
    }
    
    else if (count[1,i] == count[2,i]){
      
      total = total + 0.5 
    }
    else
      total = total + 0 # is it necessary?
  }
  proportions = total / ncol(count)
  return(proportions)
}

# Set a threshod such as the value must grater than 0.7 or less than 0.3 

# then K-means clustering to select variable
set.seed(1)
km.out = kmeans(#somehting,k=2,nstart = 20)
plot(#somehting, col=(km.out$cluster +1), 
       main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)




# Drop categorical variables with less than 2 units of predicting power

categoricalLess2 = categorical[categorical<2]
newdata = data[,!(names(data) %in% names(categoricalLess2))]

x = model.matrix(rating~.,data)[,-1]
y = data$rating

library(glmnet)
prr=cv.glmnet(x,y,family="binomial",type.measure="auc")
#yy=predict(prr,newx, s="lambda.min")
#lassopre2=predict(prr,newx, type="response")

#Then use penalized logistic regression to select variable



#100, 80, 60 predictors to check it.


predictor = function(x){
  plot(as.numeric(names(x[2,]/(x[1,]+x[2,]))), x[2,]/(x[1,]+x[2,]))
}

