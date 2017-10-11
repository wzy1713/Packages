
xgboostModel <- function(numberOfpart, #Number of part
                         folds,        #folds in CV
                         parallel,     #parallel or not
                         cores,        #parallel Cores
                         eta,          #the learning rate list should be 0-1
                         depth,        #depth of tree
                         mtry,         #subsample ratio of columns
                         objective,    #the learning task
                         ..) {
  
  install_load("doParallel", "Metrics", "caret", "dplyr", "ROCR", "pROC", "xgboost")
  
  data <- modeldata
  y <- data$y
  
  set.seed(1234)
  parId <- createFolds(y, k = numberOfpart, 
                       list = TRUE, 
                       returnTrain = FALSE)
  
  pars <- expand.grid(eta = eta, depth = depth, mtry = mtry, objective = objective)
  
  
  result <- NULL
  for (i in 1:numberOfpart) {
    
    train <- data[-parId[[i]],]
    test <- data[parId[[i]],]

    x_train <- train[, setdiff(names(train), c(dropVar,"y"))]
    x_train[] <- lapply(x_train, as.numeric)
    y_train <- train[,"y"]
    x_test <- test[, setdiff(names(test), c(dropVar,"y"))]
    x_test[] <- lapply(x_test, as.numeric)
    y_test <- test[,"y"]
    
    
    bParallel <- parallel #Whether or not to do parallel Run
    
    if (bParallel) {
      cl <- makeCluster(cores)
      registerDoParallel(cl, cores = cores)
    }
    
    xgboost_cv <- foreach(j = 1:nrow(pars), 
                          .packages = c("xgboost",  "caret", "Metrics", "ROCR")) %dopar% {
                            peta <- pars$eta[j]
                            pdepth <- pars$depth[j]
                            pmtry <- pars$mtry[j]
                            pobj <- pars$objective[j]
                            
                            xgb_model<- xgb.cv(params = list(eta = peta,  
                                                             max_depth = pdepth,
                                                             colsample_bytree = pmtry,
                                                             objective = pobj),
                                               data = xgb.DMatrix(as.matrix(x_train), label = y_train),
                                               nrounds = 100,
                                               verbose = 0,
                                               nfold = folds,
                                               stratified = TRUE,
                                               metrics = "auc")
                            
                            return(as.data.frame(xgb_model$evaluation_log)[100,])
                          }
    
    if (bParallel) {
      stopCluster(cl)
    }
    
    auc_result <- NULL
    for (id in 1:nrow(pars)) {
      auc_result <- rbind(auc_result, xgboost_cv[id][[1]])
    }
    
    best_id <- which(auc_result$test_auc_mean == max(auc_result$test_auc_mean))
    best_para <- pars[best_id,]
    best_auc <- auc_result[best_id,]
    
    xgb_final <- xgboost(data = xgb.DMatrix(as.matrix(x_train), label = y_train),
                         eta = best_para$eta,
                         max_depth = best_para$depth,
                         colsample_bytree = best_para$mtry,
                         objective = best_para$objective,
                         eval_metric = "auc",
                         nrounds = 100)
    
    dtest <- xgb.DMatrix(as.matrix(x_test), label = y_test)
    test_pred <- predict(xgb_final,
                         newdata = dtest)
    ##Calculate the AUC on train data
    pred_test<- prediction(test_pred, y_test)
    test_auc <- performance(pred_test,'auc')@y.values[[1]] 
    
    ##Output
    output <- data.frame(eta = best_para$eta,
                         depth = best_para$depth,
                         mtry = best_para$mtry,
                         objection = best_para$objective,
                         train_auc = best_auc$test_auc_mean,
                         val_auc = best_auc$test_auc_mean,
                         test_auc = test_auc)
    result <- rbind(result, output)
  }
  
  result_f <- result[, setdiff(names(result), c("objection"))]
  avg_value <- data.frame(apply(result_f, 2,mean))
  names(avg_value) <- c('metrics')
  sd_value <- apply(result_f, 2,sd)[c('train_auc','val_auc','test_auc')]
  names(sd_value) <- c('train_auc_sd','val_auc_sd','test_auc_sd')
  sd_value <- as.data.frame(sd_value)
  names(sd_value) <- c('metrics')
  
  final_result <- t(rbind(avg_value, sd_value))
  return(final_result)
}
