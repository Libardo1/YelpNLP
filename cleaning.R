# Load package RJSONIO to read in JSON files
library(RJSONIO, quietly = TRUE)

# Vector of levels for response
levels = c("low", "mid", "high")

# Extract business IDs of restaurants only
temp = as.data.frame(t(sapply(readLines("business.json"), fromJSON)))
restaurant = sapply(temp[,5], function(x) "Restaurants" %in% x)
businessID = as.character(temp$business_id[restaurant])

# Extract rating and review data from challenge dataset
temp = matrix(sapply(readLines("train.json"), function(x) unlist(fromJSON(x))),
              ncol = 10, byrow = TRUE)
temp = temp[temp[,10] %in% businessID,]
train = data.frame(stars = numeric(nrow(temp)), starsF = NA, text = NA)
for (i in 1:nrow(temp)) {
  train$stars[i] = as.numeric(temp[i,6])
  train$text[i] = temp[i,8]
}
train$starsF = cut(train$stars, seq(.5, 5.5, 1.66), labels = levels)
save(train, file = "train.rda")

# Extract rating and review data from academic dataset
temp = matrix(sapply(readLines("test.json"), function(x) unlist(fromJSON(x))),
              ncol = 10, byrow = TRUE)
test = data.frame(stars = numeric(nrow(temp)), starsF = NA, text = NA)
for (i in 1:nrow(temp)) {
  test$stars[i] = as.numeric(temp[i,8])
  test$text[i] = temp[i,7]
}
test$starsF = cut(test$stars, seq(.5, 5.5, 1.66), labels = levels)
save(test, file = "test.rda")