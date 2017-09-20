
LassoModel <- function(numberOfpart, #Number of part
                       folds,        #folds in CV
					   var_gp,       #Variable group for group lasso model
                       parallel,     #parallel or not
                       cores,        #parallel Cores
                       ..) {
  
  install_load("doParallel", "Metrics", "caret", "dplyr", "grpreg", 
               "pROC", "glmnet")
  
  data <- modeldata
  y <- data$y
  
  set.seed(1234)
  parId <- createFolds(y, k = numberOfpart, 
                       list = TRUE, 
                       returnTrain = FALSE)
  
  bParallel <- parallel #Whether or not to do parallel Run
  
  if (bParallel) {
    cl <- makeCluster(cores)
    registerDoParallel(cl, cores = cores)
  }
  
  lassorun <- foreach (i = 1:numberOfpart,
         .packages = c("caret","dplyr", "Metrics", "grpreg",
                       "glmnet","ROCR")) %dopar% {
                         
           train <- data[-parId[[i]], ]
           test <- data[parId[[i]], ]
           
           x_train <- as.matrix(train[, setdiff(names(train), "y")])
           y_train <- train[, "y"]
           x_test <- as.matrix(test[, setdiff(names(test), "y")])
           
           ##flag the fold id
           set.seed(234)
           foldsid <- createFolds(y_train, k = folds, 
                                  list = TRUE, 
                                  returnTrain = FALSE)
           foldid <- rep(0, length(y_train))
           
           for (id in 1:folds) {
             foldid[foldsid[[id]]] <- id
           }
           
           ##orginla lasso model
           lasso <- glmnet(x = x_train,
                           y = as.factor(y_train),
                           family=c("binomial"))
           lambda_s <- c(lasso$lambda[1:(length(lasso$lambda)-1)], 
                         seq(0, min(lasso$lambda), length = 100))
           
           ##Lasso with cross validation
           cv_lasso <- cv.glmnet(x = x_train,
                                 y = as.factor(y_train),
                                 family=c("binomial"),
                                 lambda = lambda_s,
                                 foldid = foldid)
           coef_s = data.frame(partid = i, 
                               lambda = cv_lasso$lambda.min,
                               variables = rownames(coef(cv_lasso)),
                               coefficients = coef(cv_lasso)[,1])
           coef_s <- coef_s[!(coef_s$coefficients == 0), ]
           rownames(coef_s) <- NULL
           
           train_pred_s <- predict(cv_lasso, x_train, type = "response")
           test_pred_s <- predict(cv_lasso, x_test, type = "response")
           
           ##Calculate the AUC on train data
           pred_train_s <- prediction(train_pred_s, y_train)
           train_auc_s <- performance(pred_train_s,'auc')@y.values[[1]] 
           
           ##Original group Lasso model
           gp_lasso <- grpreg(X = x_train,
                              y = y_train,
                              group = var_gp,
                              family=c("binomial"),
                              penalty=c("grLasso"))
           lambda_seq <- c(gp_lasso$lambda[1:(length(gp_lasso$lambda)-1)], 
                           seq(0, min(gp_lasso$lambda), length = 100))
           
           ##Group Lasso with cross validation
           cv_gp_lasso <- cv.grpreg(X = x_train,
                                    y = y_train,
                                    lambda = lambda_seq,
                                    group = var_gp,
                                    cv.ind = foldid,
                                    family=c("binomial"),
                                    penalty=c("grLasso"))
           
           coef = data.frame(partid = i, 
                             lambda = cv_gp_lasso$lambda.min,
                             variables = names(coef(cv_gp_lasso)),
                             coefficients = coef(cv_gp_lasso))
           coef <- coef[!(coef$coefficients == 0), ]
           
           train_pred <- predict(cv_gp_lasso, x_train, type = "response")
           test_pred <- predict(cv_gp_lasso, x_test, type = "response")
           
           ##Calculate the AUC on train data
           pred_train <- prediction(train_pred, y_train)
           train_auc <- performance(pred_train,'auc')@y.values[[1]] 
           
           output <- list(train_auc_s,
                          test_pred_s,
                          coef_s,
                          train_auc, 
                          test_pred,
                          coef)
           return(output)
         }
  
  if (bParallel) {
    stopCluster(cl)
  }
  
  train_auc_lasso <- numeric()
  pred_test_lasso <- rep(0, nrow(data))
  coef_lasso <- numeric()
  train_auc_gplasso <- numeric()
  pred_test_gplasso <- rep(0, nrow(data))
  coef_gplasso <- numeric()
  
  for (j in 1:numberOfpart){
    train_auc_lasso <- cbind(train_auc_lasso, lassorun[[j]][[1]])
    pred_test_lasso[parId[[j]]] <- lassorun[[j]][[2]]
    coef_lasso <- rbind(coef_lasso, lassorun[[j]][[3]])
    train_auc_gplasso <- cbind(train_auc_gplasso, lassorun[[j]][[4]])
    pred_test_gplasso[parId[[j]]] <- lassorun[[j]][[5]]
    coef_gplasso <- rbind(coef_gplasso, lassorun[[j]][[6]])
  }
  
  ##calculate the AUC on test data
  pred_ts_lasso <- prediction(pred_test_lasso, data$y)
  test_auc_lasso <- performance(pred_ts_lasso,'auc')@y.values[[1]]
  perf_test_lasso <- performance(pred_ts_lasso,"tpr","fpr")
  
  pred_ts_gplasso <- prediction(pred_test_gplasso, data$y)
  test_auc_gplasso <- performance(pred_ts_gplasso,'auc')@y.values[[1]]
  perf_test_gplasso <- performance(pred_ts_gplasso,"tpr","fpr")
  
  ##Combine all coefficients
  coef1 <- group_by(coef_lasso, variables) %>%
    summarise(., coefficients = mean(coefficients), count = n()) %>%
    as.data.frame()
  coef2 <- group_by(coef_gplasso, variables) %>%
    summarise(., coefficients = mean(coefficients), count = n()) %>%
    as.data.frame()
  
  
  output <- list(train_AUC_lasso = mean(train_auc_lasso),
                 test_AUC_lasso = test_auc_lasso,
                 coefficient_lasso = coef1[with(coef1, order(-count)),],
                 test_perf_lasso = perf_test_lasso,
                 
                 train_AUC_gplasso = mean(train_auc_gplasso),
                 test_AUC_gplasso = test_auc_gplasso,
                 coefficient_gplasso = coef2[with(coef2, order(-count)),],
                 test_perf_gplasso = perf_test_gplasso
  )
  return(output)
}



