
#-------------------------------------------------------------------------#
#                                                                         #
# Step 2: based on the results in step 1, run the model for each product  #
#                                                                         #
#-------------------------------------------------------------------------#

rm(list = ls())

dir <- "G:/zywang/CVM/ËÕÄþ"
setwd(dir)

#Import the functions
source("./01_Codes/001 functions.R")
source("./01_Codes/006 Final Model.R")

install_load("doParallel", "Metrics", "caret", "dplyr")

#Parameters
numberOfpart <- 10
parallel <- TRUE
cores <- detectCores() - 1
runid <- 2

prod_var <- c("flag_GA_IFN", "flag_IFN_GA","flag_NT_GA")

#Read raw data from step 1 which includes the customers' purchasing 
#probability on all products
rawdata <- read.csv(paste0("./03_Output/Step_I/Run_", runid , "/resultdata_1.csv"), 
                          header = T)

#Sub-folder for ouput result
subfolder <- paste0("./03_Output/Step_II")

if (file.exists(subfolder) == FALSE) {
  dir.create(subfolder, showWarnings = TRUE, recursive = TRUE, mode = "0777")
}

#output folder
targetFolder <- paste0("./03_Output/Step_II/Run_", runid)

if (file.exists(targetFolder)==FALSE) {
  dir.create(targetFolder, showWarnings = TRUE, recursive = TRUE, mode = "0777")
}

#Log file
log_file <- paste0(targetFolder, "/Model_log.txt")
cat("Step II: Using the data on every product:\n", file = log_file, append = T)

##For Product 1
modeldata <- rawdata
eval(parse(text = paste0("modeldata$y <- rawdata$",prod_var[1])))
modeldata <- modeldata[, setdiff(names(modeldata), 
                                 c(prod_var,"pred_Overall_gp"))]

select_var <- names(modeldata)

cat(" Product 1: logistic regression Model:\n", file = log_file, append = T)

start <- Sys.time()
cat("\tStart at", as.character(start) ,"\n", file = log_file, append = T)
final_output_p1 <- finalModel(numberOfpart = numberOfpart, 
                              parallel = parallel, 
                              cores = cores)

end <- Sys.time()
cat("\tEnd at", as.character(end) , "\n", file = log_file, append = T)
cat("\tUsing time:", end - start, attr(end - start, "units"), "\n",
    file = log_file,
    append = T)

auc_prod1 <- data.frame(prod = prod_var[1],
                        auc_train = final_output_p1$train_AUC,
                        auc_test = final_output_p1$test_AUC)

#plot(final_output_p1$test_perf, col = "red")
#plot(final_output_p1$test_perf_pr, add = T, col = "black")

coef_p1 <- final_output_p1$coefficient
write.csv(coef_p1, paste0(targetFolder, "/Coef_prod1.csv"), row.names = F)

pred_prod1 <- final_output_p1$pred

##For Product 2
modeldata <- rawdata
eval(parse(text = paste0("modeldata$y <- rawdata$",prod_var[2])))
modeldata <- modeldata[, setdiff(names(modeldata), 
                                 c(prod_var,"pred_Overall_gp"))]

select_var <- names(modeldata)

cat(" Product 2: logistic regression Model:\n", file = log_file, append = T)

start <- Sys.time()
cat("\tStart at", as.character(start) ,"\n", file = log_file, append = T)
final_output_p2 <- finalModel(numberOfpart = numberOfpart, 
                              parallel = parallel, 
                              cores = cores)

end <- Sys.time()
cat("\tEnd at", as.character(end) , "\n", file = log_file, append = T)
cat("\tUsing time:", end - start, attr(end - start, "units"), "\n",
    file = log_file,
    append = T)

auc_prod2 <- data.frame(prod = prod_var[2],
                        auc_train = final_output_p2$train_AUC,
                        auc_test = final_output_p2$test_AUC)

#plot(final_output_p2$test_perf, col = "red")
#plot(final_output_p2$test_perf_pr, col = "black", add = T)

coef_p2 <- final_output_p2$coefficient
write.csv(coef_p2, paste0(targetFolder, "/Coef_prod2.csv"), row.names = F)

pred_prod2 <- final_output_p2$pred

##For Product 3
modeldata <- rawdata
eval(parse(text = paste0("modeldata$y <- rawdata$",prod_var[3])))
modeldata <- modeldata[, setdiff(names(modeldata), 
                                 c(prod_var,"pred_Overall_gp"))]

select_var <- names(modeldata)

cat(" Product 3: logistic regression Model:\n", file = log_file, append = T)

start <- Sys.time()
cat("\tStart at", as.character(start) ,"\n", file = log_file, append = T)
final_output_p3 <- finalModel(numberOfpart = numberOfpart, 
                              parallel = parallel, 
                              cores = cores)

end <- Sys.time()
cat("\tEnd at", as.character(end) , "\n", file = log_file, append = T)
cat("\tUsing time:", end - start, attr(end - start, "units"), "\n",
    file = log_file,
    append = T)

auc_prod3 <- data.frame(prod = prod_var[3],
                        auc_train = final_output_p3$train_AUC,
                        auc_test = final_output_p3$test_AUC)

#plot(final_output_p3$test_perf, col = "red")
#plot(final_output_p3$test_perf_pr, col = "black", add = T)

coef_p3 <- final_output_p3$coefficient
write.csv(coef_p3, paste0(targetFolder, "/Coef_prod3.csv"), row.names = F)

pred_prod3 <- final_output_p3$pred

##All AUC on product

auc_prod <- rbind(auc_prod1,
                  auc_prod2,
                  auc_prod3)
write.csv(auc_prod, paste0(targetFolder, "/Prod_AUC.csv"), row.names = F)

##Final results
resultdata_2 <- data.frame(rawdata, 
                           pred_prod1,
                           pred_prod2,
                           pred_prod3)

write.csv(resultdata_2, paste0(targetFolder, "/resultdata_2.csv"), row.names = F)

save.image(paste0(targetFolder, "/Model_Run_step2.R"))

