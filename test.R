

rm(list = ls())

dir <- "F:/ADA/ADA/ADA_Model_rebuild"
setwd(dir)

#Import the functions
source("./02_codes/001 functions.R")
source("./02_codes/003 Stepwise Model.R")
source("./02_codes/004 Lasso Model.R")
source("./02_codes/004_2 ElasticNet Model.R")
source("./02_codes/005 RF Model.R")

install_load("doParallel", "Metrics", "caret", "dplyr")

#Parameters
parallel <- TRUE
cores <- detectCores() - 1

#parameter from outside
args <- commandArgs(TRUE)
runid <- as.numeric(args[1])
numberOfpart <-as.numeric(args[2])
folds <- as.numeric(args[3])  # should be larger than 3
seed <- as.numeric(args[4])

#Read raw data
filename = "sample_2_TRAIN"
rawdata <- read.csv(paste0("./05_output/", filename ,".csv"), header = T)
dim(rawdata)
rawdata$y <- rawdata$LEAD_APPR

set.seed(seed)
id <- sample(1:nrow(rawdata), 1000)
rawdata <- rawdata[id,]

##Need to update
dropvar <- c('LEAD_APPR',"X_dataobs_")
modeldata <- rawdata[,setdiff(names(rawdata), dropvar)]
names(modeldata) <- tolower(names(modeldata))
names(modeldata)

#Sub-folder for ouput result
subfolder <- paste0("./05_output/Step_I")

if (file.exists(subfolder) == FALSE) {
  dir.create(subfolder, showWarnings = TRUE, recursive = TRUE, mode = "0777")
}

#output folder
targetFolder <- paste0("./05_output/Step_I/Run_", runid)

if (file.exists(targetFolder)==FALSE) {
  dir.create(targetFolder, showWarnings = TRUE, recursive = TRUE, mode = "0777")
}

#Log file
log_file <- paste0(targetFolder,"/Model_log.txt")
cat("Step I: Using the data on all products:\n", file = log_file, append = T)

# M1: Run the logistic regression and stepwise model
cat(" 1: StepWise logistic regression Model:\n", file = log_file, append = T)

start <- Sys.time()
cat("\tStart at", as.character(start) ,"\n", file = log_file, append = T)
stepwise_output <- stepwiseModel(numberOfpart = numberOfpart, 
                                 parallel = parallel, 
                                 cores = cores)

end <- Sys.time()
cat("\tEnd at", as.character(end) , "\n", file = log_file, append = T)
cat("\tUsing time:", end - start, attr(end - start, "units"), "\n",
    file = log_file,
    append = T)


# M2: Run the Lasso and Group Lasso model

cat(" 2: LASSO Model:\n",file = log_file, append = T)

start <- Sys.time()
cat("\tStart at", as.character(start) ,"\n", file = log_file, append = T)

lasso_output <- LassoModel(numberOfpart = numberOfpart, 
                           folds = folds,
                           parallel = parallel, 
                           cores = cores)

end <- Sys.time()
cat("\tEnd at", as.character(end) , "\n", file = log_file, append = T)
cat("\tUsing time:", end - start,  attr(end - start, "units"), "\n",
    file = log_file,
    append = T)



# M3: Run the Elastic Net model

cat(" 3: Elastic-Net Model:\n",file = log_file, append = T)

start <- Sys.time()
cat("\tStart at", as.character(start) ,"\n", file = log_file, append = T)

EN_output <- ElasticNetModel(train_test_split = 10,  
                             alphalist = c(0, 0.5, 1),     
                             numberOfpart = numberOfpart,      
                             folds = folds,             
                             parallel = TRUE,          
                             cores = cores)

end <- Sys.time()
cat("\tEnd at", as.character(end) , "\n", file = log_file, append = T)
cat("\tUsing time:", end - start,  attr(end - start, "units"), "\n",
    file = log_file,
    append = T)

# M4: Run random forest model

cat(" 4: Random Forest Model:\n",file = log_file, append = T)

start <- Sys.time()
cat("\tStart at", as.character(start) ,"\n", file = log_file, append = T)

###Need to update
RF_output <- RFModel(numberOfpart = numberOfpart, 
                     folds = folds,        
                     parallel = parallel,     
                     cores = cores,        
                     ntree_min = 100,    
                     ntree_max = 200,    
                     mtry_min = 5,     
                     mtry_max = 7,     
                     nodesize_min = 30, 
                     nodesize_max = 40) 

end <- Sys.time()
cat("\tEnd at", as.character(end) , "\n", file = log_file, append = T)
cat("\tUsing time:", end - start,  attr(end - start, "units"), "\n",
    file = log_file,
    append = T)



#Summarize all model results
train_auc <- data.frame(data = "Train",
                        reg_auc = stepwise_output$train_AUC,
                        setp_auc = stepwise_output$train_AUC_step,
                        lasso_auc = lasso_output$train_AUC_lasso,
                        elastic_auc = EN_output$train_auc[1],
                        rf_auc = RF_output$train_AUC_rf
)

test_auc <- data.frame(data = "Test",
                       reg_auc = stepwise_output$test_AUC,
                       setp_auc = stepwise_output$test_AUC_step,
                       lasso_auc = lasso_output$test_AUC_lasso,
                       elastic_auc = EN_output$test_auc[1],
                       rf_auc = RF_output$test_AUC_rf
)

all_auc <- rbind(train_auc, test_auc)
write.csv(all_auc, paste0(targetFolder,"/", filename, "_", seed, ".csv"), row.names =  F)















