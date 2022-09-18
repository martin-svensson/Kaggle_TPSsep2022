# ====================================================================================================== #
# Description
#
#   HP tuning and validation. Since 2020 is very different from the other years, we will use
#   2019 as validation set and train on 2017 + 2018. Note that we do not really have enough data
#   to do cross validation, so tuning will only be done with 2019 as validation set. 
#   We wil validate individual models as well as ensembles (specifically passing prophet forecast as feature in other models).
#
# Change log:
#   Ver   Date        Comment
#   1.0   12/09/22    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

# -- Data wrangling

library(data.table)
library(tidyverse)
library(magrittr)
theme_set(theme_bw())

# -- Modeling

library(splines)
library(keras)
library(xgboost)
library(Ckmeans.1d.dp) # used for xgb.importance ggplot
library(lightgbm)
library(forecast)
library(prophet)

# -- For pipeline functions

library(lubridate)
library(caret)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/1_split_row_id.RData")
load("./Output/1_data.RData")
load("./Output/holidays_df.RData")

# -- Source pipeline

pipeline <- new.env()

source(
  "./Scripts/2_EDA_implementation.R",
  local = pipeline
)

source(
  "./Scripts/3_encoding.R",
  local = pipeline
)

source(
  "./Scripts/4_model_wrappers.R"
)

rm(
  list = na.omit(str_extract(ls(envir = pipeline), "^(?!fun_).*")),
  envir = pipeline
)

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

# -- Apply pipeline

train_data <- data[!(row_id %in% c(split_row_id[["2019"]], split_row_id[["2020"]]))]

val_data <- data[row_id %in% split_row_id[["2019"]]]

train_data %<>%
  pipeline$fun_add_vars(
    product_x_doy = TRUE, 
    holidays = holidays_df
  ) %>%
  pipeline$fun_encoding(
    label = "num_sold",
    cat_vars = c("country", "store", "product", "weekday")
  )

val_data %<>%
  pipeline$fun_add_vars(
    product_x_doy = TRUE, 
    holidays = holidays_df
  ) %>%
  pipeline$fun_encoding(
    label = "num_sold",
    cat_vars = c("country", "store", "product", "weekday")
  )

# -- Evaluation metric

SMAPE <- function(y_true, y_pred) {
  
  100 * 2 * mean(abs(y_pred - y_true) / (abs(y_true) + abs(y_pred)))
  
}

# -- INDIVIDUAL MODELS -------------------------------------------------------


# ---- Prophet -----------------------------------------------------------------

# No tuning, only validation

prophet_df <- 
  train_data[ , 2:6] %>%
  pivot_wider(
    values_from = num_sold,
    names_from = c("country", "store", "product")
  ) 

all_forecasts <- 
  map(
    names(prophet_df)[-1],
    ~ prophet_fit(
        data_ts = prophet_df[ , c("date", .x)],
        f_period = length(unique(val_data$date))
      )
  ) %>%
  setNames(names(prophet_df)[-1])

all_yhat <- 
  map(
    names(all_forecasts), 
    ~ all_forecasts[[.x]][[2]][ , c("yhat", "ds")] %>%
      mutate("ts" = .x)
  ) %>% rbindlist()

all_yhat[
  , 
  `:=`(ds = as.IDate(ds), yhat = ceiling(yhat))
]

prophet_val <- 
  val_data[ , 2:6] %>%
  mutate(
    ts = paste0(country, "_", store, "_", product)
  ) %>%
  left_join(
    y = all_yhat[year(ds) == 2019],
    by = c("ts" = "ts", "date" = "ds")
  )

SMAPE(y_true = prophet_val$num_sold, y_pred = prophet_val$yhat)

# fun baseline


# ---- XGBoost -----------------------------------------------------------------

nround <- c(100)
max_depth <- c(6)
gamma <- c(0)
lambda <- c(2)
eta <- c(0.2)
obj <- c("reg:gamma")

hp_grid_xgb <- 
  expand.grid(
    nround, 
    max_depth,
    gamma, 
    lambda,
    eta,
    obj
  )

colnames(hp_grid_xgb) <- 
  c("nround", 
    "max_depth",
    "gamma",
    "lambda",
    "eta",
    "obj")

hp_grid_xgb$val_error <- rep(NA, nrow(hp_grid_xgb))

feature_names <- names(train_data)[c(10, 13:14, 15, 16:35)] #[c(7, 10, 13:16, 17:35)]

for (i in (1:nrow(hp_grid_xgb))) {
  
  model_fit <- 
    xgboost_fit(
      data_x = train_data[ , feature_names, with = F],
      data_y = train_data[ , "num_sold"], 
      data_val_x = val_data[ , feature_names, with = F], 
      nround = hp_grid_xgb[i, "nround"], 
      max_depth = hp_grid_xgb[i, "max_depth"], 
      gamma = hp_grid_xgb[i, "gamma"], 
      lambda = hp_grid_xgb[i, "lambda"], 
      eta = hp_grid_xgb[i, "eta"],
      obj = hp_grid_xgb[i, "obj"]
    )
  
  hp_grid_xgb[i, "val_error"] <- 
    SMAPE(
      y_true = val_data$num_sold, 
      y_pred = model_fit[["predictions"]]
    )
  
}


# ---- NNet --------------------------------------------------------------------

epochs_no_seq <- c(80)
dropout_fac_seq <- c(0)
L2_fac_seq <- c(0)
loss <- c("mean_absolute_percentage_error")

hp_grid_nnet <- 
  expand.grid(
    epochs_no_seq, 
    dropout_fac_seq,
    L2_fac_seq,
    loss
  )

colnames(hp_grid_nnet) <- 
  c("epochs_no_seq", 
    "dropout_fac_seq",
    "L2_fac_seq",
    "loss")

hp_grid_nnet$val_error <- rep(NA, nrow(hp_grid_nnet))

feature_names <- names(train_data)[c(9, 12:14, 15:29)]

for (i in (1:nrow(hp_grid_nnet))) {
  
  model_fit <- 
    NN_fit(
      data_x = train_data[ , feature_names, with = F],
      data_y = train_data[ , "num_sold"], 
      data_val_x = val_data[ , feature_names, with = F], 
      data_val_y = val_data[ , "num_sold"],
      epochs_no = hp_grid_nnet[i, "epochs_no_seq"], 
      dropout_fac = hp_grid_nnet[i, "dropout_fac_seq"], 
      L2_fac = hp_grid_nnet[i, "L2_fac_seq"],
      loss = hp_grid_nnet[i, "loss"], 
      verbose = 0
    )
  
  hp_grid_nnet[i, "val_error"] <- 
    SMAPE(
      y_true = val_data$num_sold, 
      y_pred = model_fit[["predictions"]]
    )
  
}


# -- ENSEMBLE ----------------------------------------------------------------

# Using Prophets predictions as feature in XGBoost. Should make up for XGBoosts stortfall of not incorporating autocorrelation / lagged values

data_x <- 
  train_data %>%
  mutate(
    ts = paste0(country, "_", store, "_", product)
  ) %>%
  left_join(
    y = all_yhat[year(ds) != 2019],
    by = c("ts" = "ts", "date" = "ds")
  )

data_val_x <- 
  val_data %>%
  mutate(
    ts = paste0(country, "_", store, "_", product)
  ) %>%
  left_join(
    y = all_yhat[year(ds) == 2019],
    by = c("ts" = "ts", "date" = "ds")
  )
  
feature_names <- names(data_x)[c(9, 12:14, 15:29)]  

# -- XGBoost

xgb_ensemble_fit <- 
  xgboost_fit(
    data_x = data_x[ , feature_names, with = F],
    data_y = data_x[ , "num_sold"], 
    data_val_x = data_val_x[ , feature_names, with = F], 
    nround = hp_grid_xgb[1, "nround"], 
    max_depth = hp_grid_xgb[1, "max_depth"], 
    gamma = hp_grid_xgb[1, "gamma"], 
    lambda = hp_grid_xgb[1, "lambda"], 
    eta = hp_grid_xgb[1, "eta"],
    obj = hp_grid_xgb[1, "obj"]
  )

SMAPE(
  y_true = data_val_x$num_sold, 
  y_pred = xgb_ensemble_fit[["predictions"]]
) # including yhat from prophet is not working well

# -- NNET

nn_ensemble_fit <- 
  NN_fit(
    data_x = data_x[ , feature_names, with = F],
    data_y = data_x[ , "num_sold"], 
    data_val_x = data_val_x[ , feature_names, with = F], 
    data_val_y = data_val_x[ , "num_sold"],
    epochs_no = hp_grid_nnet[1, "epochs_no_seq"], 
    dropout_fac = hp_grid_nnet[1, "dropout_fac_seq"], 
    L2_fac = hp_grid_nnet[1, "L2_fac_seq"],
    loss = hp_grid_nnet[1, "loss"], 
    verbose = 0
  )

SMAPE(
  y_true = data_val_x$num_sold, 
  y_pred = nn_ensemble_fit[["predictions"]]
) # including yhat from prophet is not working well


# ==== EXPORT ------------------------------------------------------------------------------------------ 

save(
  hp_grid_xgb,
  file = "./Output/5_hp_grid_xgb.RData"
)
