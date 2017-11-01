################ Split ################ 
library(plyr)
library(tidyverse)
library(modelr)
library(glmnet)
library(gbm)
library(hrbrthemes)


split_train_validation_test <- function(base_datos) {
  splits <- resample_partition(base_datos, c(train = 0.6, validation = 0.3, test = 0.1))
  datasets <- map(splits, as_data_frame)
  datasets
}

################ Ridge ################ 

ridge_regression <- function(datasets) {
  set.seed(2)
  
  train <- datasets[[1]] 
  x <- data.matrix(train %>% select(-c(index, valor)))
  y <- data.matrix(train %>% select(valor))
  cv_ridge <- cv.glmnet(x,y,alpha=0, lambda = c(0, 0.01, 0.05,  0.1, 1, 5, 7, 9, 10, 15, 20))
  best_lambda <- cv_ridge$lambda.min
  mse_ridge <- min(cv_ridge$cvm)
  
  lambda_mse <- tibble(cv_ridge$lambda, cv_ridge$cvm) %>% rename(lambda = `cv_ridge$lambda`, 
                                                                 cvm = `cv_ridge$cvm`)

  
  val <- datasets[[2]]
  x_val <- data.matrix(val %>% select(-c(index, valor)))
  y_val <- data.matrix(val %>% select(valor))
  
  ridge.mod=glmnet(x,y,alpha=0)
  
  train_pred <- predict(ridge.mod, s = best_lambda, newx = x)
  train_error <- mean((train_pred-y)^2)
  
  validation_pred <- predict(ridge.mod, s = best_lambda, newx = x_val)
  
  validation_error <- mean((validation_pred-y_val)^2)
  
  resultados <- tibble::tibble(name = "Ridge", hyperparameter = best_lambda, mse = validation_error,
                               train = train_error)
  
  devolver <- list(plot = lambda_mse, resultados = resultados)
}

lasso_regression <- function(datasets) {
  set.seed(2)
  
  train <- datasets[[1]] 
  x <- data.matrix(train %>% select(-c(index, valor)))
  y <- data.matrix(train %>% select(valor))
  cv_ridge <- cv.glmnet(x,y,alpha=1, lambda = c(0.01, 0.05,  0.1, 0.5, 0.65, 1, 5, 7, 9, 10, 15, 20))
  best_lambda <- cv_ridge$lambda.min
  mse_ridge <- min(cv_ridge$cvm)
  
  lambda_mse <- tibble(cv_ridge$lambda, cv_ridge$cvm) %>% rename(lambda = `cv_ridge$lambda`, 
                                                                 cvm = `cv_ridge$cvm`)
  
  
  val <- datasets[[2]]
  x_val <- data.matrix(val %>% select(-c(index, valor)))
  y_val <- data.matrix(val %>% select(valor))
  
  ridge.mod=glmnet(x,y,alpha=1)
  
  train_pred <- predict(ridge.mod, s = best_lambda, newx = x)
  train_error <- mean((train_pred-y)^2)
  
  validation_pred <- predict(ridge.mod, s = best_lambda, newx = x_val)
  
  validation_error <- mean((validation_pred-y_val)^2)
  
  resultados <- tibble::tibble(name = "Lasso", hyperparameter = best_lambda, mse = validation_error,
                               train = train_error)
  
  devolver <- list(plot = lambda_mse, resultados = resultados)
}
  
regresion_simple <- function(datasets) {
  
  train <- datasets[[1]]
  train <- train %>% select(-c(index))
  mod <- lm(valor ~., data = train)
  
  val <- datasets[[2]]
  val <- val %>% select(-c(index))
  y_val <- data.matrix(val %>% select(valor))
  
  y <- unlist(train %>% select(valor))
  
  train_pred <- predict(mod)
  train_error <- mean((train_pred-y)^2)
  
  
  validation_pred <- predict(mod, newdata = val %>% select(-valor))
  
  validation_error <- mean((validation_pred-y_val)^2)
  
  resultados <- tibble::tibble(name = "Regresion", hyperparameter = NA, mse = validation_error,
                               train = train_error)
  
  devolver <- list(plot = NA, resultados = resultados)
}




################ GBM ################ 

gbm_over <- function(datasets, base_datos) {
  
  train <- datasets[[1]] %>% select(-index)
  y <- train %>% select(valor)
  
  
  caretGrid <- expand.grid(interaction.depth=c(1, 3, 5), n.trees = (10:50)*50,
                           shrinkage=c(0.01, 0.001, 0.1, 0.2),
                           n.minobsinnode=c(1, 10, 20))
  metric <- "RMSE"
  trainControl <- trainControl(method="cv", number=10)
  
  gbm.caret <- train(valor ~., data=train, distribution="gaussian", method="gbm",
                     trControl=trainControl, verbose=FALSE, 
                     tuneGrid=caretGrid, metric=metric, bag.fraction=0.75)  
  #fit <- gbm(formula = valor ~., data = train, n.trees = 1000, interaction.depth =4, shrinkage = 0.02, cv.folds = 5)
  
  train_pred <- predict(gbm.caret)
  
  train_error <- mean((train_pred-y)^2)
  
  val <- datasets[[2]]
  x_val <- val %>% select(-c(valor, index))
  y_val <- val %>% select(valor)
  validation_pred <- predict(gbm.caret, newdata = x_val)
  validation_error <- mean((validation_pred-y_val)^2)
  
  
  
  resultados <- tibble::tibble(name = "Gradient Boosting Trees", hyperparameter = list(gbm.caret$bestTune), validation_error = validation_error,
                               train = train_error)
  
  test <- datasets[[3]]
  
  y_test <- test %>% select(valor)
  x_test <- test %>% select(-c(valor, index))
  test_pred <- predict(gbm.caret, newdata = x_test)
  test_error <- mean((test_pred-y_test)^2)
  
  x_total <- base_datos %>% select(-c(valor, index))
  y_total <- unlist(base_datos %>% select(valor))
  y_todos <- predict(gbm.caret, newdata = x_total)
  
  total <- tibble::tibble(y_total, y_todos)
  
  devolver <- list(plot = NA, resultados = resultados, test_error = test_error, pred = total)
}


