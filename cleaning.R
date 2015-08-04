# Load package RJSONIO to read in JSON files
library(RJSONIO, quietly = TRUE)

# Vector of levels for response
levels = c("low", "high")

# Extract business IDs of restaurants only
temp = as.data.frame(t(sapply(readLines("business.json"), fromJSON)))
restaurant = sapply(temp[,5], function(x) "Restaurants" %in% x)
businessID = as.character(temp$business_id[restaurant])

# Extract rating and review data from challenge dataset
temp = matrix(sapply(readLines("train.json"), function(x) unlist(fromJSON(x))),
              ncol = 10, byrow = TRUE)
temp = temp[temp[,10] %in% businessID,]
train = data.frame(stars = numeric(nrow(temp)), starsF = NA, text = NA)
train$stars = as.numeric(temp[,6])
train$starsF = cut(train$stars, c(.5, 3.5, 5.5), labels = levels)
train$text = temp[,8]
save(train, file = "train.rda")

# Extract rating and review data from academic dataset
temp = matrix(sapply(readLines("test.json"), function(x) unlist(fromJSON(x))),
              ncol = 10, byrow = TRUE)
test = data.frame(stars = numeric(nrow(temp)), starsF = NA, text = NA)
test$stars = as.numeric(temp[,8])
test$starsF = cut(test$stars, c(.5, 3.5, 5.5), labels = levels)
test$text = temp[,7]
save(test, file = "test.rda")