
RFModel <- function(numberOfpart, #Number of part
                    folds,        #folds in CV
                    parallel,     #parallel or not
                    cores,        #parallel Cores
                    ntree_min,    #Minimun ntrees
                    ntree_max,    #Maximum ntrees
                    mtry_min,     #Minimun mtry
                    mtry_max,     #Maximum mtry
                    nodesize_min, #Minimun nodesize
                    nodesize_max, #Maximum nodesize
                    ..) {
  
  install_load("doParallel", "Metrics", "caret", "dplyr", "ROCR", "pROC", "randomForest")
  
  data <- modeldata
  y <- data$y
  
  set.seed(1234)
  parId <- createFolds(y, k = numberOfpart, 
                       list = TRUE, 
                       returnTrain = FALSE)
  
  pars <- expand.grid(ntree = round(seq(ntree_min, ntree_max, 
                                        length = round((ntree_max - ntree_min)/100)+1)),
                      mtry = round(seq(mtry_min, mtry_max, 
                                       length = round((mtry_max - mtry_min)/2 +1))),
                      nodesize = round(seq(nodesize_min, nodesize_max, 
                                           length = round((nodesize_max - nodesize_min)/5) +1))  
                      )

  ypred <- rep(0, nrow(data))
  train_auc <- rep(0, numberOfpart)
  imp <- numeric()
  
  for (i in 1:numberOfpart) {
    
    train <- data[-parId[[i]],]
    test <- data[parId[[i]],]
    
    x_train <- as.matrix(train[, setdiff(names(train), c(dropVar,"y"))])
    y_train <- train[,"y"]
    x_test <- as.matrix(test[, setdiff(names(test), c(dropVar,"y"))])
    
    bParallel <- parallel #Whether or not to do parallel Run
    
    if (bParallel) {
      cl <- makeCluster(cores)
      registerDoParallel(cl, cores = cores)
    }
    
    rf_cv <- foreach(j = 1:nrow(pars), 
          .packages = c("randomForest",  "caret", "Metrics", "ROCR")) %dopar% {
            
            pntree <- pars$ntree[j]
            pmtry <- pars$mtry[j]
            pnodesize <- pars$nodesize[j]
            
            set.seed(5678)
            subparId <- createFolds(y_train, k = folds, 
                                    list = TRUE, 
                                    returnTrain = FALSE)
            
            auc_sub <- rep(0, folds)
            for (kcv in 1:folds) {
              x_train_t <- x_train[-subparId[[kcv]],]
              x_train_val <- x_train[subparId[[kcv]],]
              
              y_train_t <- y_train[-subparId[[kcv]]]
              y_train_val <- y_train[subparId[[kcv]]]
              rf <- randomForest(x = x_train_t,
                                 y = as.factor(y_train_t),
                                 ntree = pntree,
                                 mtry = pmtry,
                                 nodesize = pnodesize)
              
              y_train_val_pred <- predict(rf, 
                                          newdata = x_train_val,
                                          type = "prob")
              
              ##Calculate the AUC on validation data
              pred_val <- prediction(y_train_val_pred[,2], y_train_val)
              val_auc <- performance(pred_val,'auc')@y.values[[1]]
              
              auc_sub[kcv] <- val_auc
            }
            auc_avg <- mean(auc_sub)
            return(auc_avg)
          }
    
    if (bParallel) {
      stopCluster(cl)
    }
    
    all_auc = unlist(rf_cv)
    
    best_par <- pars[which(all_auc==max(all_auc)),]
    
    final_rf <- randomForest(x = x_train,
                             y = as.factor(y_train),
                             ntree = best_par$ntree,
                             mtry = best_par$mtry,
                             nodesize = best_par$nodesize)
    
    train_pred <- predict(final_rf, x_train, type = "prob")
    test_pred <- predict(final_rf, x_test, type = "prob")
    
    ##Calculate the AUC on train data
    pred_train <- prediction(train_pred[,2], y_train)
    train_auc[i] <- performance(pred_train,'auc')@y.values[[1]]
    
    ##Output the probility on test data
    ypred[parId[[i]]] <- test_pred[,2]
    
    ##Variables importance
    importance <- data.frame(final_rf$importance)
    importance$variables <- row.names(importance)
    row.names(importance) <- NULL
    importance <- importance[,c("variables", "MeanDecreaseGini")]
    imp <- rbind(imp, importance)
    
  }
  
  ##calculate the AUC on test data
  pred_test <- prediction(ypred, y)
  test_AUC <- performance(pred_test,'auc')@y.values[[1]]
  perf_test_rf <- performance(pred_test,"tpr","fpr")
  
  ##Final variables Importance
  final_imp <- imp %>% 
    group_by(variables) %>% 
    summarize(MeanDecreaseGini = mean(MeanDecreaseGini), count = n()) %>%
    as.data.frame()
  final_imp <- final_imp[with(final_imp, order(-MeanDecreaseGini)),]
  
  ##Output
  output <- list(train_AUC_rf = mean(train_auc),
                 test_AUC_rf = test_AUC,
                 variableImportance_rf= final_imp,
                 test_perf_rf = perf_test_rf)
  return(output)
}
