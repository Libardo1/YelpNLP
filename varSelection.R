load("data.rda")

options(scipen = 999)

highLow = function(x) {
  counts = table(data$rating, x)
  low = counts[1,2]/(counts[1,1] + counts[1,2])
  high = counts[2,2]/(counts[2,1] + counts[2,2])
  times = max(high, low) / min(high, low)
  return(times)
}

temp = which(sapply(data[,-1], is.numeric))
categorical = numeric(length(temp))
for (i in 1:length(temp)) {
  categorical[i] = highLow(data[,temp[i]])
}
names(categorical) = names(data)[temp]