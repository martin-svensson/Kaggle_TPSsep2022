# ====================================================================================================== #
# Description
#
#   Building model wrappers  
#
# Change log:
#   Ver   Date        Comment
#   1.0   06/09/22    Initial version
#
# ====================================================================================================== #

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

# ---- XGboost -----------------------------------------------------------------

xgboost_fit <- function(data_x, data_y, data_val_x,
                        nround = 1000, max_depth = 2, gamma, lambda, eta, obj) {
  
  # grad <- function(x, y) {
  #   (2 * y * (x - y)) / ((x + y) * (x + y) * abs(x - y))
  # }
  # 
  # hess <- function(x, y) {
  #   (-4 * y * (x - y)) / ((x + y) * (x + y) * (x + y) * abs(x - y))
  # }
  # 
  # smape_obj <- function(preds, dtrain) {
  #   labels <- xgboost::getinfo(dtrain, "label")
  #   grad <- grad(labels, preds)
  #   hess <- hess(labels, preds)
  #   return(list(grad = grad, hess = hess))
  # } # loss function
  # 
  # smape_loss <- function(preds, dtrain) {
  #   labels <- getinfo(dtrain, "label")
  #   err <- mean((abs(preds - labels)) / ((abs(preds) + abs(labels)) / 2))
  #   return(list(metric = "smape", value = err))
  # } # for evaluation of validation data
  
  dtrain <- 
    xgb.DMatrix(
      data = as.matrix(data_x),
      label = as.matrix(data_y)
    )
  
  dval <- 
    xgb.DMatrix(
      data = as.matrix(data_val_x)
    )  
  
  model <- 
    xgboost(
      data = dtrain, 
      nround = nround,
      max.depth = max_depth,
      lambda = lambda,
      eta = eta,
      gamma = gamma,
      objective = obj # SMAPE unavailable
      #objective = smape_obj
    )
  
  # -- predict on validation data and get fitted values
  
  fitted_values <- 
    model %>%
    predict(
      dtrain
    )
  
  predict_val <- 
    model %>% 
    predict(
      dval
    )
  
  # -- Feature importance
  
  feature_importance <-
    xgb.importance(
      model, 
      feature_names = colnames(dtrain)
    )
  
  # -- output
  
  out <- 
    list(
      "feature_importance" = feature_importance,
      "fitted_values" = ceiling(fitted_values), 
      "predictions" = ceiling(predict_val)
    )
  
  return(out)
  
}

# ---- LightGBM ----------------------------------------------------------------

# lightgbm does not cnverge with mape loss

# data_x <- 
#   train_data[
#     year(date) != 2019,
#     names(train_data)[c(9, 11:13, 14:22)], 
#     with = F
#   ]
# 
# data_val_x <- 
#   train_data[
#     year(date) == 2019,
#     names(train_data)[c(9, 11:13, 14:22)], 
#     with = F
#   ]
# 
# data_y <- 
#   train_data[
#     year(date) != 2019, 
#     "num_sold"
#   ]
# 
# data_val_y <- 
#   train_data[
#     year(date) == 2019, 
#     "num_sold"
#   ]
# 
# lightGBM_fit <- function(data_x, data_y, data_val_x,
#                          nrounds = 1000, num_leaves, lr) {
#   
#   dtrain <- 
#     lgb.Dataset(
#       data = as.matrix(data_x), 
#       label = as.matrix(data_y)
#     )
#   
#   dval <- 
#     lgb.Dataset(
#       data = as.matrix(data_val_x)
#     )
#   
#   model <- 
#     lightgbm(
#       data = dtrain, 
#       nrounds = nrounds,
#       params = list(
#         # num_leaves = num_leaves,
#         # learning_rate = lr,
#         objective = "mape"
#       )
#     )
#   
# }




# ---- Neural Network ----------------------------------------------------------

NN_fit <- function(data_x, data_y, data_val_x, data_val_y,
                   dropout_fac, L2_fac, epochs_no, loss,
                   verbose, view_metrics = T) {
  
  # Input:
  #   - data_x, data_y, data_val_x, data_val_y: data.tables/frames.
  
  data_x <- data_x %>% as.matrix
  data_y <- data_y %>% as.matrix
  data_val_x <- data_val_x %>% as.matrix
  data_val_y <- data_val_y %>% as.matrix
  
  # -- define, compile and train model
  
  model <- keras_model_sequential()
  
  model %>% 
    layer_dense(
      units = 40, activation = "relu", input_shape = c(ncol(data_x))
    ) %>% 
    layer_dropout(
      rate = dropout_fac
    ) %>%
    layer_activity_regularization(
      l2 = L2_fac
    ) %>%
    layer_dense(
      units = 15, activation = "relu"
    ) %>% 
    layer_dropout(
      rate = dropout_fac
    ) %>%
    layer_activity_regularization(
      l2 = L2_fac
    ) %>%
    layer_dense(
      units = 1, 
      activation = "relu" # we want to ensure positive values
    )
  
  model %>% 
    compile(
      loss = loss , # competition evaluation metric (custom function doesn't work)
      optimizer = optimizer_adam(),
      metrics = metric_mean_squared_error()
    )
  
  model_history <- 
    model %>% 
    fit(
      data_x, 
      data_y[ , 1], 
      epochs = epochs_no, 
      batch_size = 128, 
      validation_data = list(data_val_x, data_val_y[ , 1]),
      view_metrics = view_metrics,
      verbose = verbose
    )
  
  # -- predict on validation data and get fitted values
  
  fitted_values <- 
    model %>%
    predict(
      data_x, 
      batch_size = 128
    )
    
  predict_val <- 
    model %>% 
    predict(
      data_val_x, 
      batch_size = 128
    )
  
  # -- output
  
  out <- 
    list(
      "model_history" = model_history, # for plotting etc
      "fitted_values" = ceiling(fitted_values), 
      "predictions" = ceiling(predict_val)
    )
  
  return(out)
  
}

# ---- Prophet -----------------------------------------------------------------

prophet_fit <- function(data_ts, f_period = 365) {
  
  model <- 
    data_ts %>%
    rename(
      ds = date, 
      y = 2
    ) %>%
    prophet
  
  forecast_df <- 
    make_future_dataframe(
      model, periods = f_period
    )
  
  forecast <- 
    predict(
      model,
      forecast_df
    ) %>%
    as.data.table()
  
  # -- Output
  
  out <- 
    list(
      model,
      forecast
    )
  
  names(out) <- c("model", "forecast")
  
  return(out)
  
}


