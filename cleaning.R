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

# Extract rating and review data from academic dataset
temp = matrix(sapply(readLines("test.json"), function(x) unlist(fromJSON(x))),
              ncol = 10, byrow = TRUE)
test = data.frame(stars = numeric(nrow(temp)), starsF = NA, text = NA)
test$stars = as.numeric(temp[,8])
test$starsF = cut(test$stars, c(.5, 3.5, 5.5), labels = levels)
test$text = temp[,7]

# Remove line breaks
train$text = gsub("\n", "", train$text)
test$text = gsub("\n", "", test$text)

# Remove quotations and backslashing out of quotations
train$text = gsub('\"', "", train$text)
train$text = gsub('"', "", train$text)
train$text = gsub("'", "", train$text)
test$text = gsub('\"', "", test$text)
test$text = gsub('"', "", test$text)
test$text = gsub("'", "", test$text)

# Remove "c" with cedilla
train$text = gsub("\xe7", "c", train$text)
test$text = gsub("\xe7", "c", test$text)

# Remove "e" with acute accent
train$text = gsub("\xe9", "e", train$text)
test$text = gsub("\xe9", "e", test$text)

# Remove registered trademark symbol
train$text = gsub("\xae", "", train$text)
test$text = gsub("\xae", "", test$text)

save(train, file = "train.rda")
save(test, file = "test.rda")