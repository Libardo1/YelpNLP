load("data.rda")

# Disable scientific notation
options(scipen = 999)

# Function to compute categorical variables' "preference" for high/low rating
highLow = function(x) {
  counts = table(data$rating, x)
  low = counts[1,2]/(counts[1,1] + counts[1,2])
  high = counts[2,2]/(counts[2,1] + counts[2,2])
  times = max(high, low) / min(high, low)
  return(times)
}

# Create vector of categorical variables by helpfulness in predicting rating
temp = which(sapply(data, is.factor))[-1]
categorical = numeric(length(temp))
for (i in 1:length(temp)) {
  categorical[i] = highLow(data[,temp[i]])
}
names(categorical) = names(data)[temp]
categorical = sort(categorical, decreasing = TRUE)

# Drop categorical variables with less than 2 units of predicting power
###data[,!(names(data) %in% names(categorical))]

#
