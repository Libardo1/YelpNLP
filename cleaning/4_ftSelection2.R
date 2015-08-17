load("../data/dataRev.rda")

# Load relevant packages
library(leaps)

# Create appropriate dummy variables
x = model.matrix(rating ~ ., dataRev)
y = as.numeric(dataRev$rating) - 1

# Forward stepwise, backward stepwise, and hybrid selection
fwdSelect = regsubsets(rating ~ ., dataRev, really.big = TRUE, nvmax = 102,
                       method = "forward")
bwdSelect = regsubsets(rating ~ ., dataRev, really.big = TRUE, nvmax = 102,
                       method = "backward")
hbdSelect = regsubsets(rating ~ ., dataRev, really.big = TRUE, nvmax = 102,
                       method = "seqrep")
save(fwdSelect, bwdSelect, hbdSelect, file = "../data/subSelect.rda")

dataFin = dataRev[, colnames(x) %in% names(coef(bwdSelect, 93))]
save(dataFin, file = "../data/dataFin.rda")