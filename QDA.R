load("dataFin.rda")

set.seed(1)
train =  sample(1:nrow(dataFin), nrow(dataFin)/2 - 1)

# QDA

set.seed(1)
qda.fit = qda(rating ~ ., data = dataFin, subset = train)
qda.fit
qda.class = predict(qda.fit, dataFin[-train,])$class
table(qda.class, dataFin[-train,'rating'])
mean(qda.class==dataFin[-train,'rating'])
testError3 = 1 - mean(qda.class==dataFin[-train,'rating'])
testError3