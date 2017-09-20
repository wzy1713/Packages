

rm(list = ls())

dir <- "H:/"
setwd(dir)
runid <- 1
sampleCnt <- 4

#Import the functions
source("./01_Codes/001 functions.R")

#Load the packages
install_load("dplyr")


allnumericutpoint <- numeric()
for (runid in 1:sampleCnt) {
  #Read the raw data
  rawdat <- read.csv(paste0("./02_Data/rawdata_", runid,".csv"), 
                     header = T, 
                     stringsAsFactors = FALSE,
                     na.strings = c("NA", ""))
  names(rawdat) <- tolower(names(rawdat))
  dim(rawdat)
  
  numericVar <- c("v3")
  catgoryVar <- c("v1", "v2")
  response <- c("response")
  
  ##Group Numeric variables
  numtovar <- numeric()
  numCutPoint <- numeric()
  for (var in numericVar) {
    eval(parse(text = paste0("t<- rawdat$", var)))
    medianValue <- median(t, na.rm = T)
    t[which(is.na(t) == TRUE)] <- medianValue
    dt <- data.frame(t)
    t_gp <- NumericToCat("t", "dt")
    numtovar <- cbind(numtovar, t_gp$cutgroup)
    numCutPoint <- rbind(numCutPoint, t_gp$cutpoint)
  }
  
  numtovar <- as.data.frame(numtovar)
  names(numtovar) <- numericVar
  
  numCutPoint <- data.frame(var = numericVar, numCutPoint)
  row.names(numCutPoint) <- NULL
  names(numCutPoint) <- c("var",paste0("q_", seq(0, 1, 0.25)))
  allnumericutpoint <- rbind(allnumericutpoint, numCutPoint)
  
  ##combine the grouped variables
  rawdat2 <- data.frame(rawdat[, setdiff(names(rawdat), numericVar)], numtovar)
  
  ##Category variable to dummy variable
  for (catvar in c(catgoryVar, numericVar)){
    eval(parse(text = paste0("r <- rawdat2$", response)))
    
    eval(parse(text = paste0("t <- rawdat2$", catvar)))
    cnt <- table(t)
    maxCat <- names(cnt)[which(cnt == max(cnt))]
    eval(parse(text = paste0("rawdat2[which(is.na(rawdat2$",catvar, ") == TRUE),'", catvar,"'] <- '", maxCat, "'")))
    
    eval(parse(text = paste0("tempCat <- dummy_cat('", catvar, "','rawdat2')")))
    rawdat2 <- data.frame(rawdat2, tempCat)
  }
  
  rawdat2 <- rawdat2[, setdiff(names(rawdat2), c(catgoryVar, numericVar))]
  
  ##Deacriptive Analysis
  out <- descriptiveAnalysis(variables = setdiff(names(rawdat2), response), 
                             dat = "rawdat2", 
                             response = response) %>%
         as.data.frame()

  write.csv(out, paste0("./03_Output/DescriptiveTable_", runid, ".csv"), 
            row.names = FALSE)
}

write.csv(allnumericutpoint, "./03_Output/NumCutPoint.csv", row.names = FALSE)


