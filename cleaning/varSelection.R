load("../data/dataRough.rda")

# Disable scientific notation
options(scipen = 999)

# Function to compute categorical variables' "preference" for high/low rating
highLow = function(x) {
  counts = table(dataRough$rating, x)
  low = counts[1,2]/(counts[1,1] + counts[1,2])
  high = counts[2,2]/(counts[2,1] + counts[2,2])
  times = max(high, low) / min(high, low)
  return(times)
}

# Create vector of categorical variables by helpfulness in predicting rating
temp = which(sapply(dataRough, is.factor))[-1]
categorical = numeric(length(temp))
for (i in 1:length(temp)) {
  categorical[i] = highLow(dataRough[,temp[i]])
}
names(categorical) = names(dataRough)[temp]
categorical = sort(categorical, decreasing = TRUE)
save(categorical, file = "../data/categorical.rda")

# Drop categorical variables with less than 2 units of predicting power
lessThan2 = categorical[categorical < 2]
dataRev = dataRough[,!(names(dataRough) %in% names(lessThan2))]

dataRev = na.omit(dataRev)
save(dataRev, file = "../data/dataRev.rda")