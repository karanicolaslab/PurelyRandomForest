### RF Regression ###

## Randomly select a split point
random_var <- function(x, y) {
  ## No Gini index being involved
  ## Randomly select one of the x values as split point
  sortedx <- sort(unique(x[]))
  n = length(sortedx) - 1
  splitn <- sample(1:n,1)
  split_at <- (sortedx[splitn] + sortedx[splitn + 1])/2
  return(c(split = split_at))
}

random_reg_tree <- function(formula, data, minsize) {
  # A different random seed for each tree (as there is no dataset sampling)
  sd = sample(1:10000,1)
  print(sd)
  set.seed(sd)
  
  # coerce to data.frame
  data <- as.data.frame(data)
  
  # handle formula
  formula <- terms.formula(formula)
  
  # get the design matrix
  X <- model.matrix(formula, data)
  
  # extract target
  y <- data[, as.character(formula)[2]]
  
  # initialize while loop
  do_splits <- TRUE
  
  # create output data.frame with splitting rules and observations
  tree_info <- data.frame(NODE = 1, NOBS = nrow(data), FILTER = NA, TERMINAL = "SPLIT",
                           SPLIT = NA, stringsAsFactors = FALSE)
  
  # keep splitting until there are only leafs left
  while(do_splits) {
    
    # which parents have to be splitted
    to_calculate <- which(tree_info$TERMINAL == "SPLIT")
    
    for (j in to_calculate) {
      
      # handle root node
      if (!is.na(tree_info[j, "FILTER"])) {
        # subset data according to the filter
        this_data <- subset(data, eval(parse(text = tree_info[j, "FILTER"])))
        # get the design matrix
        X <- model.matrix(formula, this_data)
      } else {
        this_data <- data
      }
      tree_info[j, "TERMINAL"] <- ifelse(((nrow(this_data))==1), "LEAF", "PARENT")
      if (tree_info[j, "TERMINAL"] == "PARENT") {
          ##Sample a random variable
          rv <- sample(c(2:9), 1)
          Input <- data.frame(X[,rv])
          colnames(Input) <- all.vars(formula)[(rv)]
          # Splitting of a random point for each of the feature
          splitting <- apply(Input,  MARGIN = 2, FUN = random_var, y = this_data[, all.vars(formula)[1]])
      
      # Randomly select one feature-split point
      tmp_splitter <- splitting
     
      # define maxnode
      mn <- max(tree_info$NODE)
      
      # paste filter rules
      current_filter <- c(paste(names(tmp_splitter), ">=",
                            splitting[names(tmp_splitter)]),
                      paste(names(tmp_splitter), "<", 
                            splitting[names(tmp_splitter)]))
                            
      # Error handling! check if the splitting rule has already been invoked
      split_here  <- !sapply(current_filter,
                             FUN = function(x,y) any(grepl(x, x = y)),
                             y = tree_info$FILTER)
      
      # append the splitting rules
      if (!is.na(tree_info[j, "FILTER"])) {
        current_filter  <- paste(tree_info[j, "FILTER"], 
                             current_filter, sep = " & ")
      } 
      
      # calculate metrics within the children
      metr <- lapply(current_filter,
                         FUN = function(i, x, data, formula) {
                          df <- subset(x = x, subset = eval(parse(text = i)))
                          nobs <- nrow(df)
                          w <- nobs/nrow(data)
                          y <- df[, all.vars(formula)[1]]
                          return(c(nobs))
                         },
                         x = this_data, data = data, formula = formula)  
      
      # extract relevant information
      current_nobs <- sapply(metr, function(x) x[[1]])
      current_y <- this_data[, all.vars(formula)[1]]
      
      # create children data frame
      children <- data.frame(NODE = c(mn+1, mn+2),
                             NOBS = current_nobs,
                             FILTER = current_filter,
                             TERMINAL = rep("SPLIT", 2),
                             SPLIT = NA,
                             row.names = NULL)[split_here,]
      
      # overwrite state of current node, add gini importance and split variable
      #tree_info[j, "TERMINAL"] <- ifelse((nrow(this_data))<=minsize, "LEAF", "PARENT")
      
      #if (tree_info[j, "TERMINAL"] == "PARENT") {
      tree_info[j, "SPLIT"] <- names(tmp_splitter)
      #}
      
      # bind everything
      tree_info <- rbind(tree_info, children)
      
      }
      #print(tree_info)
      # check if there are any open splits left
      do_splits <- !all(tree_info$TERMINAL != "SPLIT")
    } # end for
  } # end while
  
  # calculate fitted values and predictions at each node
  pred <- data.frame()
  fitted <-c()
  for (i in seq_len(nrow(tree_info['TERMINAL']))) {
      if (tree_info['TERMINAL']$TERMINAL[i] == "LEAF"){
          ind <- as.numeric(rownames(subset(data, eval(parse(text = tree_info['FILTER']$FILTER[i])))))
          fitted[ind] <- mean(y[ind])
          prediction <- data.frame(cbind(i,mean(y[ind])))
      }else{
          if (i == 1){
              prediction <- data.frame(cbind(i,mean(y[])))
          }else{
              ind <- as.numeric(rownames(subset(data, eval(parse(text = tree_info['FILTER']$FILTER[i])))))
              prediction <- data.frame(cbind(i,mean(y[ind])))
          }
      }
      pred <- rbind(pred, prediction)
  }
  
  # return everything
  return(list(tree = tree_info, fit = fitted, nodepred = pred, formula = formula, data = data))
}
prediction_test <- function(test, tree, nodepred) {
    tfitted <-c()
    for (i in seq_len(nrow(tree))) {
        if (tree['TERMINAL']$TERMINAL[i] == "LEAF"){
            ind <- as.numeric(rownames(subset(test, eval(parse(text = tree['FILTER']$FILTER[i])))))
            tfitted[ind] <- nodepred[i,2]
            #prediction <- data.frame(cbind(i,mean(y[ind])))
        }else{
            #if (i == 1){
                #prediction <- data.frame(cbind(i,mean(y[])))
            #}else{
                #ind <- as.numeric(rownames(subset(test, eval(parse(text = trees$tree['FILTER']$FILTER[i])))))
                #prediction <- data.frame(cbind(i,mean(y[ind])))
            #}
        }
    }
    
    return(list(testfit = tfitted))
}
