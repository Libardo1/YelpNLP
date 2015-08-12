# Load package RJSONIO to read in JSON files
library(RJSONIO, quietly = TRUE)

# Vector of levels for response
levels = c("low", "high")

# Extract business IDs of restaurants west of Montreal only
temp = as.data.frame(t(sapply(readLines("business.json"), fromJSON)))
temp = temp[temp[,10] < -75,]
temp = temp[sapply(temp[,5], function(x) "Restaurants" %in% x),]
businessID = as.character(temp[,1])

# Extract rating and review data from challenge dataset
temp = matrix(sapply(readLines("review.json"), function(x) unlist(fromJSON(x))),
              ncol = 10, byrow = TRUE)
temp = temp[temp[,10] %in% businessID,]
data = data.frame(stars = numeric(nrow(temp)), starsF = NA, text = NA)
data$stars = as.numeric(temp[,6])
data$starsF = cut(data$stars, c(.5, 3.5, 5.5), labels = levels)
data$text = temp[,8]

# Remove line breaks
data$text = gsub("\n", "", data$text)
data$text = gsub("\r", "", data$text)

# Remove quotations and backslashing out of quotations
data$text = gsub('\"', "", data$text)
data$text = gsub('"', "", data$text)
data$text = gsub("'", "", data$text)

# Remove common unsupported characters
data$text = gsub("\x86", "", data$text)
data$text = gsub("\x92", "", data$text)
data$text = gsub("\x94", "", data$text)
data$text = gsub("\x99", "", data$text)
data$text = gsub("\xa0", "", data$text)
data$text = gsub("\xa1", "!", data$text)
data$text = gsub("\xa2", " cents ", data$text)
data$text = gsub("\xac", "", data$text)
data$text = gsub("\xae", "", data$text)
data$text = gsub("\xb0", " degrees ", data$text)
data$text = gsub("\xb4", "", data$text)
data$text = gsub("\xb6", "", data$text)
data$text = gsub("\xbc", "", data$text)
data$text = gsub("\xbe", "", data$text)
data$text = gsub("\xc0", "A", data$text)
data$text = gsub("\xc1", "A", data$text)
data$text = gsub("\xc9", "E", data$text)
data$text = gsub("\xd7", "", data$text)
data$text = gsub("\xdc", "U", data$text)
data$text = gsub("\xe0", "a", data$text)
data$text = gsub("\xe1", "a", data$text)
data$text = gsub("\xe3", "a", data$text)
data$text = gsub("\xe7", "c", data$text)
data$text = gsub("\xe8", "e", data$text)
data$text = gsub("\xe9", "e", data$text)
data$text = gsub("\xea", "e", data$text)
data$text = gsub("\xeb", "e", data$text)
data$text = gsub("\xec", "i", data$text)
data$text = gsub("\xed", "i", data$text)
data$text = gsub("\xef", "i", data$text)
data$text = gsub("\xf1", "n", data$text)
data$text = gsub("\xf2", "o", data$text)
data$text = gsub("\xf3", "o", data$text)
data$text = gsub("\xf4", "o", data$text)
data$text = gsub("\xf6", "o", data$text)
data$text = gsub("\xfa", "u", data$text)
data$text = gsub("\xfb", "u", data$text)
data$text = gsub("\xfc", "u", data$text)
data$text = gsub("(]\x8coJDWKc_)", "", data$text)
data$text = gsub("8\x8cF", "", data$text)
data$text = gsub("4n|G", "", data$text)
data$text = gsub("\005\x92\177\xdc\002", "", data$text)

# Remove most reviews in French, German, Italian, Norweigian, Spanish
temp = c(" [Jj]e ", " [Nn][[:alpha:]]+ [[:alpha:]]* pas ", " [Cc]est ", " [Ii]l[s]? ", " [Ee]lle[s] ", " [Nn]ous ", " qui ", " [Ii]ch ", " [Dd]as ", " [Ii]st ", " nicht ", " und ", " [Ee]in ", " [Dd]er? ", " [Ss]ie ", "\xdf", " y ", " por ", " [Qq]ue ", " muy ", " [Ee]lla ", " bien[ .!]", " dans ", " l[eo]s? ", " esta? ", " ser ", " buena? ", " [Uu]n[aoe]? ", " [Ii]kke ", " og ", "\xd8", "\xe4", "\xee")
for (i in 1:length(temp)) {
  data = data[-grep(temp[i], data$text),]
}

# Remove an individual unhelpful observation
data = data[-780072,]

save(data, file = "data.rda")