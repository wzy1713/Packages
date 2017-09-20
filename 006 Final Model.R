finalModel <- function(numberOfpart, #Number of part
                       parallel,     #parallel or not
                       cores,        #parallel Cores
                       ...){
  
  install_load("doParallel", "Metrics", "caret", "dplyr", "ROCR", "pROC")
  
  data <- modeldata[, select_var]
  y <- data$y
  
  set.seed(1234)
  parId <- createFolds(y, k = numberOfpart, 
                       list = TRUE, 
                       returnTrain = FALSE)
  
  all_coefs <- numeric()
  ypred <- rep(0, nrow(data))
  train_auc <- rep(0, numberOfpart)
  
  bParallel <- parallel #Whether or not to do parallel Run
  
  if (bParallel) {
    cl <- makeCluster(cores)
    registerDoParallel(cl, cores = cores)
  }
  
  steprun <- foreach (i = 1:numberOfpart,
                      .packages = c("dplyr", "Metrics", "ROCR")) %dopar% {
                        
                        train <- data[-parId[[i]],]
                        test <- data[parId[[i]],]
                        
                        ##Original Logistic Regression
                        glm.model<- glm(y ~ ., data = train, family = "binomial")
                        
                        coef = data.frame(partid = i, 
                                          variables = names(glm.model$coefficients),
                                          coefficients = glm.model$coefficients)
                        
                        train_pred <- predict(glm.model, train, type = "response")
                        test_pred <- predict(glm.model, test, type = "response")
                        
                        ##Calculate the AUC on train data
                        pred_train <- prediction(train_pred, train$y)
                        train_auc <- performance(pred_train,'auc')@y.values[[1]] 
                        
                        out <- list(train_auc, 
                                    test_pred, 
                                    coef
                        )
                        return(out)
                      }
  
  if (bParallel) {
    stopCluster(cl)
  }
  
  train_auc <- numeric()
  pred_test <- rep(0, nrow(data))
  coef <- numeric()
  
  for (j in 1:numberOfpart){
    train_auc <- cbind(train_auc, steprun[[j]][[1]])
    pred_test[parId[[j]]] <- steprun[[j]][[2]]
    coef <- rbind(coef, steprun[[j]][[3]])
  }
  
  ##calculate the AUC on test data
  pred_ts <- prediction(pred_test, data$y)
  test_AUC <- performance(pred_ts,'auc')@y.values[[1]] 
  perf_test_auc <- performance(pred_ts,"tpr","fpr")
  perf_test_pr <- performance(pred_ts,"prec","rec")

  ##Combine all coefficients
  coef1 <- group_by(coef, variables) %>%
    summarise(., coefficients = mean(coefficients), count = n()) %>%
    as.data.frame()
  output <- list(train_AUC = mean(train_auc),
                 test_AUC = test_AUC,
                 coefficient = coef1[with(coef1, order(-count)),],
                 test_perf= perf_test_auc,
                 test_perf_pr = perf_test_pr,
                 pred = pred_test)
  return(output)
}




