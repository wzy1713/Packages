
#---------------------------------------------------------------#
#                                                               #
# Step 1: This the main codes to run all models.                #
#                                                               #
#---------------------------------------------------------------#

rm(list = ls())

dir <- "G:/zywang/CVM/ËÕÄþ"
setwd(dir)

#Read raw data
rawdata <- read.csv("./02_Data/final_model_data_comb.csv", header = T)
rawdata$y <- rawdata$response
rawdata <- rawdata[, setdiff(names(rawdata), "response")]

##Need to update
dropvar <- c('age4',
             'der_sexM',
             'flag_IFN_IFN',
             'flag_NT_IFN',
             'idx_IFN',
             'idx_paytypeSMRU',
             'idx_prodtypeS',
             'idx_spec5',
             'num_pre_cort_iv3',
             'num_pre_cort_oral3',
             'num_pre_meds4',
             'num_pre_mri_any4',
             'num_pre_op_dx4',
             'num_pre_relapse_1yr2',
             'pat_regionW',
             'pchrlson3',
             'pre_idx_NOMS',
             'pre_ms_medical_allowed4',
             'pre_ms_pharmacy_allowed2',
             'pre_ms_total_allowed4',
             'pre_non_ms_medical_allowed4',
             'pre_non_ms_pharmacy_allowed4',
             'pre_non_ms_total_allowed4')

modeldata <- rawdata[,setdiff(names(rawdata), dropvar)]

#Import the functions
source("./01_Codes/functions.R")
source("./01_Codes/Stepwise Model.R")
source("./01_Codes/Lasso Model.R")
source("./01_Codes/RF Model.R")
source("./01_Codes/Final Model.R")

install_load("doParallel", "Metrics", "caret", "dplyr")

#Parameters
numberOfpart <- 10
folds <- 10  # should be larger than 3
parallel <- TRUE
cores <- detectCores() - 1
runid <- 2

#Sub-folder for ouput result
subfolder <- paste0("./03_Output/Step_I")

if (file.exists(subfolder) == FALSE) {
  dir.create(subfolder, showWarnings = TRUE, recursive = TRUE, mode = "0777")
}

#output folder
targetFolder <- paste0("./03_Output/Step_I/Run_", runid)

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

cat(" 2: Group LASSO Model:\n",file = log_file, append = T)

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


# M3: Run random forest model

cat(" 3: Random Forest Model:\n",file = log_file, append = T)

start <- Sys.time()
cat("\tStart at", as.character(start) ,"\n", file = log_file, append = T)

###Need to update
RF_output <- RFModel(numberOfpart = numberOfpart, 
                     folds = folds,        
                     parallel = parallel,     
                     cores = cores,        
                     ntree_min = 100,    
                     ntree_max = 500,    
                     mtry_min = 5,     
                     mtry_max = 11,     
                     nodesize_min = 10, 
                     nodesize_max = 30) 

end <- Sys.time()
cat("\tEnd at", as.character(end) , "\n", file = log_file, append = T)
cat("\tUsing time:", end - start,  attr(end - start, "units"), "\n",
    file = log_file,
    append = T)


#Combine all results

## AUC Results
auc_result1 <- data.frame(Type = "logistic",
                          train_auc = stepwise_output$train_AUC,
                          test_auc = stepwise_output$test_AUC)
auc_result2 <- data.frame(Type = "stepwise",
                          train_auc = stepwise_output$train_AUC_step,
                          test_auc = stepwise_output$test_AUC_step)

auc_result3 <- data.frame(Type = "Lasso",
                          train_auc = lasso_output$train_AUC_lasso,
                          test_auc = lasso_output$test_AUC_lasso)
auc_result4 <- data.frame(Type = "GroupLasso",
                          train_auc = lasso_output$train_AUC_gplasso,
                          test_auc = lasso_output$test_AUC_gplasso)

auc_result5 <- data.frame(Type = "RandomForest",
                          train_auc = RF_output$train_AUC,
                          test_auc = RF_output$test_AUC)

all_result <- rbind(auc_result1, 
                    auc_result2,
                    auc_result3,
                    auc_result4,
                    auc_result5)

##Coefficients or variables importance

write.csv(stepwise_output$coefficient, 
          paste0(targetFolder, "/logistic_coef.csv"), 
          row.names = F)

write.csv(stepwise_output$coefficient_step, 
          paste0(targetFolder, "/stepwise_coef.csv"), 
          row.names = F)

write.csv(lasso_output$coefficient_lasso, 
          paste0(targetFolder, "/lasso_coef.csv"), 
          row.names = F)

write.csv(lasso_output$coefficient_gplasso, 
          paste0(targetFolder, "/gplasso_coef.csv"), 
          row.names = F)

write.csv(RF_output$variableImportance_rf, 
          paste0(targetFolder, "/RF_importance.csv"), 
          row.names = F)



#Identify the selected variables according to the best model: Need to update
select_var <- c('y',
                'age1',
                'age2',
                'age3',
                'idx_spec1',
                'idx_spec2',
                'idx_spec3',
                'idx_spec4',
                'num_pre_cort_iv1',
                'num_pre_cort_iv2',
                'num_pre_cort_oral1',
                'num_pre_cort_oral2',
                'num_pre_meds1',
                'num_pre_meds2',
                'num_pre_meds3',
                'num_pre_op_dx1',
                'num_pre_op_dx2',
                'num_pre_op_dx3',
                'num_pre_relapse_1yr1',
                'pat_regionE',
                'pat_regionMW',
                'pat_regionS',
                'pre_ampyra1',
                'pre_comor11',
                'pre_comor13',
                'pre_comor16',
                'pre_comor4',
                'pre_comor6',
                'pre_comor7',
                'pre_comor8',
                'pre_ms_pharmacy_allowed1',
                'pre_non_ms_pharmacy_allowed1',
                'pre_non_ms_pharmacy_allowed2',
                'pre_non_ms_pharmacy_allowed3',
                'flag_GA_IFN',
                'flag_IFN_GA',
                'flag_NT_GA',
                'num_pre_mri_any1',
                'num_pre_mri_any2',
                'num_pre_mri_any3',
                'pre_comor15',
                'pre_comor17',
                'pre_comor25')

# M4: Run the original logistic regression using the selected variables
cat(" 4: final logistic regression Model:\n", file = log_file, append = T)

start <- Sys.time()
cat("\tStart at", as.character(start) ,"\n", file = log_file, append = T)
final_output <- finalModel(numberOfpart = numberOfpart, 
                           parallel = parallel, 
                           cores = cores)

end <- Sys.time()
cat("\tEnd at", as.character(end) , "\n", file = log_file, append = T)
cat("\tUsing time:", end - start, attr(end - start, "units"), "\n",
    file = log_file,
    append = T)

auc_result6 <- data.frame(Type = "finalModel",
                          train_auc = final_output$train_AUC,
                          test_auc = final_output$test_AUC)

all_result <- rbind(all_result, auc_result6)

write.csv(all_result, 
          paste0(targetFolder, "/AUC_Result.csv"), row.names = F)

##AUC Curve
png(filename = paste0(targetFolder, "/AUC.png"),
    width = 600, height = 500, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))

plot(stepwise_output$test_perf, col = "black", lwd = 2)
plot(stepwise_output$test_perf_step, add = T, col = "red")
plot(lasso_output$test_perf_lasso, add = T, col = "blue")
plot(lasso_output$test_perf_gplasso, add = T, col = "green")
plot(RF_output$test_perf_rf, add = T, col = "gray")
plot(final_output$test_perf, lwd = 2, add = T, col = "orange")

title("AUC Values Comparasion")
legend(0.7, 0.3, 
       legend = c("Logistic", "Stepwise","Lasso", "GroupLasso", "RandomForest", "FinalResult"), 
       col = c("black", "red", "blue", "green", "gray", "orange"),
       lty = 1, lwd = 1,cex = 1,
       merge = TRUE)
dev.off()


##Coefficients
coef <- final_output$coefficient
write.csv(coef, paste0(targetFolder, "/Final_coef.csv"), row.names = F)

##Predict probability
pred_value <- final_output$pred
pred_value_gp <- cut(pred_value, 
                     breaks = quantile(pred_value, probs = seq(0, 1, 0.1)), 
                     include.lowest = T,
                     label = seq(10,1,-1))
rawdata$pred_Overall <- pred_value
rawdata$pred_Overall_gp <- pred_value_gp

resultdata_1 <- rawdata[, c(select_var, "pred_Overall","pred_Overall_gp")] #need to update
write.csv(resultdata_1, paste0(targetFolder, "/resultdata_1.csv"), row.names = F)

act_pred <- resultdata_1 %>% 
            group_by(., pred_Overall_gp) %>%
            summarize(., actM = mean(y), predM = mean(pred_Overall)) %>%
            as.data.frame()

write.csv(act_pred, 
          paste0(targetFolder, "/predict_act_Comparasion.csv"),
          row.names = F)

save.image(paste0(targetFolder, "/Model_Run_step1.R"))




