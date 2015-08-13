load("dataFin.rda")

set.seed(1)
train =  sample(1:nrow(dataFin), nrow(dataFin)/2 - 1)

# LDA

library(MASS)
lda.fit = lda(rating ~ ., data = dataFin, subset = train)
lda.fit
lda.pred = predict(lda.fit, dataFin[-train,])
lda.class = lda.pred$class
table(lda.class, dataFin[-train,'rating'])
mean(lda.class == dataFin[-train,'rating'])
testError2 = 1 - mean(lda.class == dataFin[-train,'rating'])
testError2

#Applying a 50 % threshold to the posterior probabilities
#sum(lda.pred$posterior[,1]>=.5)
#sum(lda.pred$posterior[,1]<.5)
