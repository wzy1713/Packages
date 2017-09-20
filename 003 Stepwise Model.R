
stepwiseModel <- function(numberOfpart, #Number of part
                          parallel,     #parallel or not
                          cores,        #parallel Cores
                          ...){
  
  install_load("doParallel", "Metrics", "caret", "dplyr", "ROCR", "pROC")
  
  data <- modeldata
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
          
          #Train the stepwise model:both forward and backward
          glm_step <- step(glm.model, 
                           direction = "both",
                           trace = 0)
          coef_step = data.frame(partid = i, 
                                 variables = names(glm_step$coefficients),
                                 coefficients = glm_step$coefficients)
          row.names(coef_step) <- NULL
          
          train_pred_step <- predict(glm_step, train, type = "response")
          test_pred_step <- predict(glm_step, test, type = "response")
          
          ##Calculate the AUC on train data
          pred_train_step <- prediction(train_pred_step, train$y)
          train_auc_step <- performance(pred_train_step,'auc')@y.values[[1]] 
          
          out <- list(train_auc, 
                      test_pred, 
                      coef,
                      train_auc_step, 
                      test_pred_step,
                      coef_step
          )
          return(out)
        }
  
  if (bParallel) {
    stopCluster(cl)
  }
  
  train_auc <- numeric()
  train_auc_step <- numeric()
  pred_test <- rep(0, nrow(data))
  pred_test_step <- rep(0, nrow(data))
  coef <- numeric()
  coef_step <- numeric()
  
  for (j in 1:numberOfpart){
    train_auc <- cbind(train_auc, steprun[[j]][[1]])
    pred_test[parId[[j]]] <- steprun[[j]][[2]]
    coef <- rbind(coef, steprun[[j]][[3]])
    
    train_auc_step <- cbind(train_auc_step, steprun[[j]][[4]])
    pred_test_step[parId[[j]]] <- steprun[[j]][[5]]
    coef_step <- rbind(coef_step, steprun[[j]][[6]])
  }
  
  ##calculate the AUC on test data
  pred_ts <- prediction(pred_test, data$y)
  test_AUC <- performance(pred_ts,'auc')@y.values[[1]] 
  perf_test <- performance(pred_ts,"tpr","fpr")
  
  
  pred_ts_step <- prediction(pred_test_step, data$y)
  test_AUC_step <- performance(pred_ts_step,'auc')@y.values[[1]] 
  perf_test_step <- performance(pred_ts_step,"tpr","fpr")
  
  
  ##Combine all coefficients
  coef1 <- group_by(coef, variables) %>%
           summarise(., coefficients = mean(coefficients), count = n()) %>%
           as.data.frame()
  coef1_step <- group_by(coef_step, variables) %>%
                summarise(., coefficients = mean(coefficients), count = n()) %>%
                as.data.frame()
  
  output <- list(train_AUC = mean(train_auc),
                 test_AUC = test_AUC,
                 coefficient = coef1[with(coef1, order(-count)),],
                 test_perf= perf_test,
                 train_AUC_step = mean(train_auc_step),
                 test_AUC_step = test_AUC_step,
                 coefficient_step = coef1_step[with(coef1_step, order(-count)),],
                 test_perf_step= perf_test_step)
  
  return(output)
}




