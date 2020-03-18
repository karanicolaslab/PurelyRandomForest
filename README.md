# PurelyRandomForest
A tree model is built by randomly choosing both the feature on which to split as well as the split itself. A training set of 10000 points is used for building 100 individual trees (ntree=100) starting from a different random seed each time. In this case, unlike Random Forest, no bootstrapping is performed between different trees. In this example we built two sets of purely random forest model, one where individual tree splitting is parameterized by a minimum of 5 sample points that must appear in both the daughter nodes (nodesize =5) and the other where each tree was built until there is one sample on each of the leaf nodes (nodesize =1). The performance of these models is tested on the remaining 1000 data points comprising the test set.

Example 1: Purely Random Forest (ntree = 100, nodesize = 5, mtry = number of features)
Path: ../examples/PurelyRF_example.R

Example 2: Purely Random Forest (ntree = 100, nodesize = 1, mtry = number of features)
Path: ../examples/FullyGrown_PurelyRF_example.R

Sample Commands:
cd ~/PurelyRandomForest/examples/
Rscript FullyGrown_PurelyRF_example.R
