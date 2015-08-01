# Load package RJSONIO to read in JSON files
library(RJSONIO, quietly = TRUE)

# Vector of levels for response
starsF = c("low", "mid", "high")

# Extract business IDs of restaurants only
temp = as.data.frame(t(sapply(readLines("business.json"), fromJSON)))
restaurant = logical(nrow(temp))
for (i in 1:nrow(temp)) {
  restaurant[i] = "Restaurants" %in% temp[,5][[i]]
}
businessID = as.character(temp$business_id[restaurant])

# Extract rating and review data from challenge dataset
temp = data.frame(fromJSON(readLines("...", n = 1)), stringsAsFactors = FALSE)
temp = temp[seq(3, nrow(temp), 3),]
temp = temp[temp$business_id %in% businessID,]
train = data.frame(
  stars = numeric(nrow(temp)),
  starsF = NA,
  text = NA
)
for (i in 1:nrow(temp)/3) {
  training$stars[i] = temp[,4][3 * i]
  training$text[i] = temp[,6][3 * i]
}
training$starsF = cut(training$stars, seq(.5, 5.5, 1.66), labels = starsF)
save(training, "train.rda")

# Extract rating and review data from academic dataset
temp = data.frame(fromJSON(readLines("...", n = 1)), stringsAsFactors = FALSE)
temp = temp[seq(3, nrow(temp), 3),]
test = data.frame(
  stars = numeric(nrow(temp)/3),
  starsF = NA,
  text = NA
)
for (i in 1:nrow(temp)/3) {
  test$stars[i] = temp[,6][3 * i]
  test$text[i] = temp[,5][3 * i]
}
test$starsF = cut(test$stars, seq(.5, 5.5, 1.66), labels = starsF)
save(test, "test.rda")