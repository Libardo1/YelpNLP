load("dataRev.rda")

# Disable scientific notation
options(scipen = 999)

# Function to compute categorical variables' "preference" for high/low rating
highLow = function(x) {
  counts = table(dataRev$rating, x)
  low = counts[1,2]/(counts[1,1] + counts[1,2])
  high = counts[2,2]/(counts[2,1] + counts[2,2])
  times = max(high, low) / min(high, low)
  return(times)
}

# Create vector of categorical variables by helpfulness in predicting rating
temp = which(sapply(dataRev, is.factor))[-1]
categorical = numeric(length(temp))
for (i in 1:length(temp)) {
  categorical[i] = highLow(dataRev[,temp[i]])
}
names(categorical) = names(dataRev)[temp]
categorical = sort(categorical, decreasing = TRUE)
save(categorical, file = "categorical.rda")

# Drop categorical variables with less than 2 units of predicting power
lessThan2 = categorical[categorical < 2]
dataFin = dataRev[,!(names(dataRev) %in% names(lessThan2))]

dataFin = na.omit(dataFin)
save(dataFin, file = "dataFin.rda")
