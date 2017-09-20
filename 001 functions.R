# Install and load the packages
install_load <- function (package1, ...) {   
  # convert arguments to vector
  packages <- c(package1, ...)
  # start loop to determine if each package is installed
  for(package in packages){
    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  }
}

## sample
#install_load("randomForest", "glmnet", "grpreg")

# create dummy coding for category data
dummy_cat <- function(column_name, dat) {
  eval(parse(text = paste("idx <- sort(unique( ", dat , "$", column_name, "))", sep = "")))
  eval(parse(text = paste("dummy <- mat.or.vec(length(", dat, "$", column_name, "),length(idx))", sep = "")))
  for (j in 1:length(idx)) {
    eval(parse(text = paste("dummy[,j] <- as.integer(", dat, "$", column_name, " == idx[j])", sep = "")))
  }
  colnames(dummy) <- paste(column_name, idx, sep="_")
  return(dummy)
}

##Sample
a1 <- c(1,2,3,4,1,2,3)
a2 <- c('a','b','c','a','b','a', 'Unkown')
data <- data.frame(a1, a2)
a2_gp <- dummy_cat('a2', "data")


# Numeric Variable to group based on the quantiles
NumericToCat <- function(variable, dat) {
  eval(parse(text = paste("t <- ", dat , "$", variable, sep = "")))
  cutvalue <- quantile(t, type = 3)
  t_l <- unique(cutvalue)
  t_gp <- cut(t, breaks = t_l, label = seq(1, length(t_l)-1),
              include.lowest = TRUE)
  output <- list(cutpoint = t_l,
                 cutgroup = t_gp)
  return(output)
}

##Sample
a <- NumericToCat("a1", "data")

#Descriptive Analysis
descriptiveAnalysis <- function(variables, dat, response) {
  #response variable
  eval(parse(text = paste("res <- ", dat, "$", response, sep = "")))
  eval(parse(text = paste("all_var <- names(", dat, ")", sep = "")))
  #variables descriptive
  desc <- numeric()
  for (var in variables) {
    if ((var %in% all_var) == FALSE) {
      cat("\nERROR! variable:", var, "is not in dataset. \n")
    } else {
      eval(parse(text = paste("t <- ", dat, "$", var, sep = "")))
      mean <- round(mean(t, na.rm = T), 5)
      median <- median(t, na.rm = T)
      cnt <- length(t)
      posCnt <- sum(t)
      responseRate <- mean(res[which(t == 1)])
      r_t <- cbind(variable = var,
                   totalcnt = cnt,
                   PosCnt = posCnt,
                   PostiveResponseRate = responseRate,
                   Mean = mean,
                   Median = median)
      desc <- rbind(desc, r_t)
    }
  }
  return(as.data.frame(desc))
}

data1 <- data.frame(data, a2_gp, a1_q = a$cutgroup) 
a1_gp <- dummy_cat('a1', "data1")
data2 <- data.frame(data1, a1_gp)
data2$response <- c(1,0,0,1,0,1,1)
dropVar <- c("a1", "a2", "a1_q", "response")

out <- descriptiveAnalysis(variables = setdiff(names(data2), dropVar), 
                           dat = "data2", 
                           response = "response")


