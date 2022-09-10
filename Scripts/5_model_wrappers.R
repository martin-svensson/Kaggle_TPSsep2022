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


# ---- Read in data for tests ---------------------------------------------------

library(data.table)
library(tidyverse)
library(magrittr)
library(glmnet)
library(splines)
# -- For pipeline functions
library(lubridate)
library(caret)

load("./Output/1_split_row_id.RData")
load("./Output/1_data.RData")

# -- Source pipeline

pipeline <- new.env()

source(
  "./Scripts/2_EDA_implementation.R",
  local = pipeline
)

source(
  "./Scripts/4_encoding.R",
  local = pipeline
)

rm(
  list = na.omit(str_extract(ls(envir = pipeline), "^(?!fun_).*")),
  envir = pipeline
)

train_data <- data[!(row_id %in% split_row_id[["test"]])]

train_data %<>%
  pipeline$fun_add_vars() %>%
  pipeline$fun_encoding(
    label = "num_sold",
    cat_vars = c("country", "store", "product")
  )


# ---- GLM ---------------------------------------------------------------------

data_x <- 
  train_data[ , names(train_data)[c(11, 12, 14:19)], with = F]

product_doy <- 
  model.matrix(
    as.formula(num_sold ~ product*bs(day_of_year, df = 9)),
    data = train_data
  ) %>% as.data.table()

product_doy[ , `(Intercept)` := NULL] # added by the GLM

data_x <- cbind(data_x, product_doy)

data_val_x <- data_x[1:10000]

data_x <- data_x[10001:nrow(data_x)]

data_y <- train_data[10001:nrow(train_data), "num_sold"]

family <- Gamma

GLM_fit <- function(data_x, data_y, data_val_x, family, nlambda = 100, alpha = 1) {
  
  # Input:
  #   - data_x, data_y, data_val_x: data.tables/frames. Note: Intercept is added by the model
  #   - family
  #   - lambda_grid: a sequence of lambda values
  #   - alpha: a single numeric value
  # Requires: glmnet
  
  model <- 
    glmnet(
      x = as.matrix(data_x), 
      y = as.matrix(data_y)[, 1], 
      family = family, 
      intercept = T,
      alpha = alpha, 
      nlambda = nlambda
    )
  
  model_fitted <- 
    predict(
      model, 
      newx = as.matrix(data_x), 
      s = model$lambda, 
      type = "response"
    )
  
  model_preds <- 
    predict(
      model, 
      newx = as.matrix(data_val_x), 
      s = model$lambda, 
      type = "response"
    )
  
  out <- 
    list(
      "fitted_val" = model_fitted, 
      "predictions"  = model_preds
    )
  
  return(out)
}




# ---- XGboost -----------------------------------------------------------------





# ---- Neural Network ----------------------------------------------------------







# ==== EXPORT ------------------------------------------------------------------------------------------ 