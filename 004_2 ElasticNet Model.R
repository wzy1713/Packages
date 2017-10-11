
ElasticNetModel <- function(train_test_split,  #train,test split groups
                            alphalist,         #alpha value beween 0 and 1
                            numberOfpart,      #Number of part
                            folds,             #folds in CV
                            parallel,          #parallel or not
                            cores,             #parallel Cores
                            ..) {
  
  install_load("doParallel", "Metrics", "caret", "dplyr", "grpreg", 
               "pROC", "glmnet")
  
  set.seed(789)
  splitid <- createFolds(rawdata$y, k = train_test_split, 
                         list = TRUE, 
                         returnTrain = FALSE)
  
  alpha_f <- NULL
  lambda_f <- NULL
  train_auc_f <- NULL
  val_auc_f <- NULL
  test_auc_f <- NULL
  coef_f2 <- NULL
  for (gp in 1:train_test_split) {
    data <- rawdata[-splitid[[gp]],]
    y <- data$y
    test <- rawdata[splitid[[gp]],]
    y_test <- test$y
    
    set.seed(1234)
    parId <- createFolds(y, k = numberOfpart, 
                         list = TRUE, 
                         returnTrain = FALSE)
    
    bParallel <- parallel #Whether or not to do parallel Run
    
    if (bParallel) {
      cl <- makeCluster(cores)
      registerDoParallel(cl, cores = cores)
    }
    
    result <- NULL
    for (j in 1:length(alphalist)) {
      alpha <- alphalist[j]
      elasticnetrun <- foreach (i = 1:numberOfpart,
                                .packages = c("caret","dplyr", "Metrics", "grpreg",
                                              "glmnet","ROCR")) %dopar% {
                                                
                                                train <- data[-parId[[i]], ]
                                                val <- data[parId[[i]], ]
                                                
                                                x_train <- as.matrix(train[, setdiff(names(train), "y")])
                                                y_train <- train[, "y"]
                                                x_val <- as.matrix(val[, setdiff(names(val), "y")])
                                                y_val <- val[, "y"]
                                                
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
                                                                      alpha = alpha,
                                                                      foldid = foldid)
                                                
                                                train_pred_s <- predict(cv_lasso, x_train, type = "response")
                                                val_pred_s <- predict(cv_lasso, x_val, type = "response")
                                                
                                                ##Calculate the AUC on train data
                                                pred_train_s <- prediction(train_pred_s, y_train)
                                                train_auc_s <- performance(pred_train_s,'auc')@y.values[[1]] 
                                                
                                                ##Calculate the AUC on validation data
                                                pred_val_s <- prediction(val_pred_s, y_val)
                                                val_auc_s <- performance(pred_val_s,'auc')@y.values[[1]] 
                                                
                                                
                                                output <- data.frame(alpha,
                                                                     lambda = cv_lasso$lambda.min,
                                                                     train_auc = train_auc_s, 
                                                                     val_auc = val_auc_s)
                                                
                                                return(output)
                                              }
      train_auc <- 0
      val_auc <- 0
      lambda <- 0
      for (l in 1:length(elasticnetrun)){
        lambda <- lambda + elasticnetrun[[l]]$lambda
        train_auc <- train_auc + elasticnetrun[[l]]$train_auc
        val_auc <- val_auc + elasticnetrun[[l]]$val_auc
      }
      
      re <- data.frame(alpha = alpha, 
                       lambda = lambda/numberOfpart, 
                       train_auc = train_auc/numberOfpart, 
                       val_auc = val_auc/numberOfpart)
      result <- rbind(result, re)
    }
    
    if (bParallel) {
      stopCluster(cl)
    }
    
    id <- which(result$val_auc == max(result$val_auc))
    best_para <- result[id,]
    f_model <- glmnet(x = as.matrix(data[, setdiff(names(data), "y")]),
                      y = as.factor(data$y),
                      alpha = best_para$alpha,
                      lambda = result$lambda,
                      family=c("binomial"))
    coef_f = data.frame(variables = rownames(coef(f_model)),
                        coefficients = coef(f_model)[,id])
    coef_f <- coef_f[!(coef_f$coefficients == 0), ]
    rownames(coef_f) <- NULL
    
    test_pred <- predict(f_model, 
                         as.matrix(test[, setdiff(names(test), "y")]), 
                         type = "response")
    test_pred <- test_pred[, id]
    pred_test <- prediction(test_pred, test$y)
    test_auc_s <- performance(pred_test,'auc')@y.values[[1]] 
    
    alpha_f <- c(alpha_f, best_para$alpha)
    lambda_f <- c(lambda_f, best_para$lambda)
    train_auc_f <- c(train_auc_f, best_para$train_auc)
    val_auc_f <- c(val_auc_f, best_para$val_auc)
    test_auc_f <- c(test_auc_f, test_auc_s)
    coef_f2 <- rbind(coef_f2, coef_f)
  }
  
  coeff_final <- coef_f2 %>% 
                 group_by(variables) %>%
                 summarize(coef = mean(coefficients), std = sd(coefficients)) %>% 
                 as.data.frame()
  coeff_final <- coeff_final[!(coeff_final$coef == 0), ]
  
  final_output <- list(alpha = c(mean(alpha_f), sd(alpha_f)),
                       lambda = c(mean(lambda_f), sd(lambda_f)),
                       train_auc = c(mean(train_auc_f), sd(train_auc_f)),
                       val_auc = c(mean(val_auc_f), sd(val_auc_f)),
                       test_auc = c(mean(test_auc_f), sd(test_auc_f)),
                       coefficients = coeff_final)
  return(final_output)
}

