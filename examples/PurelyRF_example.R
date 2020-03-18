#### MAIN ####

source("../algorithms/PurelyRF.R")
set.seed(8368)

ntable<-data.frame()
test_table <-data.frame()

### TRAIN and TEST datasets synthesis
# Normally distrubted 10000 points with mean = 0 and SD = 1

## TRAIN set
a <- round(rnorm(10000, mean = 0, sd = 1),6)
b <- round(rnorm(10000, mean = 0, sd = 1),6)
c <- round(rnorm(10000, mean = 0, sd = 1),6)
d <- round(rnorm(10000, mean = 0, sd = 1),6)
e <- round(rnorm(10000, mean = 0, sd = 1),6)
f <- round(rnorm(10000, mean = 0, sd = 1),6)
g <- round(rnorm(10000, mean = 0, sd = 1),6)
h <- round(rnorm(10000, mean = 0, sd = 1),6)

data <- data.frame(cbind(a,b,c,d,e,f,g,h))
colnames(data) <- c('A','B', 'C', 'D','E', 'F', 'G','H')

data$I <- data$A + data$B + data$C + data$D + data$E + data$F + data$G + data$H

eq <- I~A+B+C+D+E+F+G+H

##TEST set
ta <- round(rnorm(1000, mean = 0, sd = 1),4)
tb <- round(rnorm(1000, mean = 0, sd = 1),4)
tc <- round(rnorm(1000, mean = 0, sd = 1),4)
td <- round(rnorm(1000, mean = 0, sd = 1),4)
te <- round(rnorm(1000, mean = 0, sd = 1),4)
tf <- round(rnorm(1000, mean = 0, sd = 1),4)
tg <- round(rnorm(1000, mean = 0, sd = 1),4)
th <- round(rnorm(1000, mean = 0, sd = 1),4)
test <- data.frame(cbind(ta,tb,tc,td,te,tf,tg,th))
colnames(test) <- c('A','B', 'C', 'D','E', 'F', 'G','H')

## PURELY RANDOM FOREST
## apply random_reg_tree function 100 times and each time it using a different seed
## there is no sampling of any kind
## minsize = 5 means the tree will stop splitting when either or both of the daughter nodes have less than or equal to 5 samples. Daughter nodes are then declared as leaf nodes.
trees <- plyr::raply(
  100,
  random_reg_tree(formula = eq, data = data, minsize = 5),
  .progress = "text",
)
# extract fit
fits <- do.call("cbind", trees[,2])
## second column is the fits column in trees output. This makes a table/dataset of size 10000 X 100 where each column is predicted label value from each tree

# calculate the final fit as a mean of all regression trees
rf_fit <- apply(fits, MARGIN = 1, mean, na.rm = TRUE)

## Output
ntable <- cbind(rf_fit,data$I)
colnames(ntable)<-c('Xval','Yval')
#print(ntable)
write.table(ntable, "Performance_Training_Set_nodesize_5", quote = FALSE, row.names = FALSE, col.names = TRUE)

## Testing phase
## Get predictions from each tree
## Final prediction for test set is the mean of all tree predictions

result <- array()
for (n in seq(1:100)) {
    result[n] <- prediction_test(test = test, tree = trees[n,]$tree, nodepred = trees[n,]$nodepred)
}

testpreds <- do.call("cbind", result)
pred <- apply(testpreds, MARGIN = 1, mean, na.rm = TRUE)

# Ground Truth
Test_GT <- test$A + test$B + test$C + test$D + test$E + test$F + test$G + test$H
test_table <- cbind(pred, Test_GT)
colnames(test_table)<-c('Xval','Yval')

write.table(test_table, "Performance_Test_Set_nodesize_5", quote = FALSE, row.names = FALSE, col.names = TRUE)
