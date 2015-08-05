# Load package RJSONIO to read in JSON files
library(RJSONIO, quietly = TRUE)

# Vector of levels for response
levels = c("low", "high")

# Extract business IDs of restaurants in contiguous US only
temp = as.data.frame(t(sapply(readLines("business.json"), fromJSON)))
temp = temp[temp[,10] > -124.785 & temp[,10] < -66.947028 &
              temp[,13] > 24.446667 & temp[,13] < 49.384472,]
temp = temp[sapply(temp[,5], function(x) "Restaurants" %in% x),]
businessID = as.character(temp[,1])

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
train$text = gsub("\r", "", train$text)
test$text = gsub("\n", "", test$text)
test$text = gsub("\r", "", test$text)

# Remove quotations and backslashing out of quotations
train$text = gsub('\"', "", train$text)
train$text = gsub('"', "", train$text)
train$text = gsub("'", "", train$text)
test$text = gsub('\"', "", test$text)
test$text = gsub('"', "", test$text)
test$text = gsub("'", "", test$text)

# Remove common unsupported characters
train$text = gsub("\x86", "", train$text)
train$text = gsub("\x92", "", train$text)
train$text = gsub("\x94", "", train$text)
train$text = gsub("\x99", "", train$text)
train$text = gsub("\xa0", "", train$text)
train$text = gsub("\xa1", "!", train$text)
train$text = gsub("\xa2", " cents ", train$text)
train$text = gsub("\xac", "", train$text)
train$text = gsub("\xae", "", train$text)
train$text = gsub("\xb0", " degrees ", train$text)
train$text = gsub("\xb4", "", train$text)
train$text = gsub("\xb6", "", train$text)
train$text = gsub("\xbc", "", train$text)
train$text = gsub("\xbe", "", train$text)
train$text = gsub("\xc0", "A", train$text)
train$text = gsub("\xc1", "A", train$text)
train$text = gsub("\xc9", "E", train$text)
train$text = gsub("\xd7", "", train$text)
train$text = gsub("\xdc", "U", train$text)
train$text = gsub("\xe0", "a", train$text)
train$text = gsub("\xe1", "a", train$text)
train$text = gsub("\xe3", "a", train$text)
train$text = gsub("\xe7", "c", train$text)
train$text = gsub("\xe8", "e", train$text)
train$text = gsub("\xe9", "e", train$text)
train$text = gsub("\xea", "e", train$text)
train$text = gsub("\xeb", "e", train$text)
train$text = gsub("\xec", "i", train$text)
train$text = gsub("\xed", "i", train$text)
train$text = gsub("\xef", "i", train$text)
train$text = gsub("\xf1", "n", train$text)
train$text = gsub("\xf2", "o", train$text)
train$text = gsub("\xf3", "o", train$text)
train$text = gsub("\xf4", "o", train$text)
train$text = gsub("\xf6", "o", train$text)
train$text = gsub("\xfa", "u", train$text)
train$text = gsub("\xfb", "u", train$text)
train$text = gsub("\xfc", "u", train$text)
train$text = gsub("(]\x8coJDWKc_)", "", train$text)
train$text = gsub("8\x8cF", "", train$text)
train$text = gsub("4n|G", "", train$text)
train$text = gsub("\005\x92\177\xdc\002", "", train$text)

# Remove most reviews in French, German, Italian, Spanish
temp = c(" [Jj]e ", " [Nn][[:alpha:]]+ [[:alpha:]]* pas ", " [Cc]est ", " [Ii]l[s]? ", " [Ee]lle[s] ", " [Nn]ous ", " qui ", " [Ii]ch ", " [Dd]as ", " [Ii]st ", " nicht ", " und ", " [Ee]in ", " [Dd]er? ", " [Ss]ie ", "\xdf", " y ", " por ", " [Qq]ue ", " muy ", " [Ee]lla ", " bien[ .!]", " dans ", " l[eo]s? ", " esta? ", " ser ", " buena? ", " [Uu]n[aoe]? ", " [Ii]kke ", " og ", "\xd8", "\xe4", "\xee")
for (i in 1:length(temp)) {
  train = train[-grep(temp[i], train$text),]
}

# Remove unhelpful individual reviews
temp = -c(490870, 498999, 804560, 820239)
train = train[temp,]

save(train, file = "train.rda")
save(test, file = "test.rda")