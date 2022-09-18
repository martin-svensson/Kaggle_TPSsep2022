# ====================================================================================================== #
# Description
#
#   Final models and submission creation
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
load("./Output/5_hp_grid_xgb.RData")
load("./Output/1_competition_test_data.RData")

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

train_data <- 
  data %>%
  pipeline$fun_add_vars(
    product_x_doy = TRUE, 
    country_x_year = TRUE,
    holidays = holidays_df
  ) %>%
  pipeline$fun_encoding(
    label = "num_sold",
    cat_vars = c("country", "store", "product", "weekday")
  )

competition_test_data %<>%
  pipeline$fun_add_vars(
    product_x_doy = TRUE, 
    country_x_year = TRUE,
    holidays = holidays_df
  ) %>%
  pipeline$fun_encoding(
    label = "day_of_year", # used to get the model matrix, should be done in a better way
    cat_vars = c("country", "store", "product", "weekday")
  )


# -- XGBOOST -----------------------------------------------------------------



# ---- 2020 + 2021 indicator ---------------------------------------------------------

train_data_tmp <- copy(train_data)
test_data_tmp <- copy(competition_test_data)

train_data_tmp[
  ,
  covid_indic := ifelse(year(date) == 2020, 1, 0)
]

test_data_tmp[
  ,
  covid_indic := ifelse(year(date) == 2021, 1, 0)
]

feature_names <- names(train_data_tmp)[c(9, 12:30)]

model_fit <- 
  xgboost_fit(
    data_x = train_data_tmp[ , feature_names, with = F],
    data_y = train_data_tmp[ , "num_sold"], 
    data_val_x = test_data_tmp[ , feature_names, with = F], 
    nround = hp_grid_xgb[1, "nround"], 
    max_depth = hp_grid_xgb[1, "max_depth"], 
    gamma = hp_grid_xgb[1, "gamma"], 
    lambda = hp_grid_xgb[1, "lambda"], 
    eta = hp_grid_xgb[1, "eta"],
    obj = hp_grid_xgb[1, "obj"]
  )


forecast_plot <- 
  competition_test_data[ , num_sold := model_fit$predictions] %>%
  group_by(
    date, 
    product
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  )

data %>%
  group_by(
    date, 
    product
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  ) %>%
  rows_append(
    forecast_plot
  ) %>%
  ggplot(aes(x = date, y = num_sold, color = product)) +
  geom_line()

submission_1 <- 
  data.table(
    "row_id" = competition_test_data$row_id, 
    "num_sold" = model_fit$predictions
  )
# public leaderboard; 6.89. Significantly worse than testing on 2019, so it is safe to say that 2021 is different. 

fwrite(
  submission_1,
  file = "./Output/Submissions/submission_1.csv"
)

# ---- Only 2020 ---------------------------------------------------------------

feature_names <- names(train_data)[c(9, 12:29)]

model_fit <- 
  xgboost_fit(
    data_x = train_data[year(date) == 2020, feature_names, with = F],
    data_y = train_data[year(date) == 2020, "num_sold"], 
    data_val_x = competition_test_data[ , feature_names, with = F], 
    nround = hp_grid_xgb[1, "nround"], 
    max_depth = hp_grid_xgb[1, "max_depth"], 
    gamma = hp_grid_xgb[1, "gamma"], 
    lambda = hp_grid_xgb[1, "lambda"], 
    eta = hp_grid_xgb[1, "eta"],
    obj = hp_grid_xgb[1, "obj"]
  )

forecast_plot <- 
  competition_test_data[ , num_sold := model_fit$predictions] %>%
  group_by(
    date, 
    product
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  )

data %>%
  group_by(
    date, 
    product
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  ) %>%
  rows_append(
    forecast_plot
  ) %>%
  ggplot(aes(x = date, y = num_sold, color = product)) +
  geom_line()

submission_2 <- 
  data.table(
    "row_id" = competition_test_data$row_id, 
    "num_sold" = model_fit$predictions
  )
# public leaderboard; 6.80. Slightly better than submission 1, but it might as well be a fluke

fwrite(
  submission_2,
  file = "./Output/Submissions/submission_2.csv"
)

xgb.ggplot.importance(model_fit[["feature_importance"]])


# ---- Only 2017 to 2019 -------------------------------------------------------

# learn patterns form 2017 to 2019 and scale according to bump in volume in 2020

feature_names <- names(train_data)[c(9, 12:29)]

model_fit <- 
  xgboost_fit(
    data_x = train_data[year(date) != 2020, feature_names, with = F],
    data_y = train_data[year(date) != 2020, "num_sold"], 
    data_val_x = competition_test_data[ , feature_names, with = F], 
    nround = hp_grid_xgb[1, "nround"], 
    max_depth = hp_grid_xgb[1, "max_depth"], 
    gamma = hp_grid_xgb[1, "gamma"], 
    lambda = hp_grid_xgb[1, "lambda"], 
    eta = hp_grid_xgb[1, "eta"],
    obj = hp_grid_xgb[1, "obj"]
  )

scaling_factor_tmp <- 
  train_data %>%
  group_by(
    year(date)
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  )

scaling_factor <- 
  scaling_factor_tmp[4, ]$num_sold / mean(scaling_factor_tmp[1:3, ]$num_sold)

predictions_scaled <- 
  ceiling(model_fit$predictions * scaling_factor)
  
forecast_plot <- 
  competition_test_data[ , num_sold := predictions_scaled] %>%
  group_by(
    date, 
    product
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  )

data %>%
  group_by(
    date, 
    product
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  ) %>%
  rows_append(
    forecast_plot
  ) %>%
  ggplot(aes(x = date, y = num_sold, color = product)) +
  geom_line()

submission_3 <- 
  data.table(
    "row_id" = competition_test_data$row_id, 
    "num_sold" = model_fit$predictions
  )
# public leaderboard; 30, so the pattern in 2021 follows that of 2020 much more closely that that of 2017 to 2019

fwrite(
  submission_3,
  file = "./Output/Submissions/submission_3.csv"
)



# ---- Submission 6 ------------------------------------------------------------

feature_names <- names(train_data)[c(10, 13:43)]

model_fit <- 
  xgboost_fit(
    data_x = train_data[ , feature_names, with = F],
    data_y = train_data[ , "num_sold"], 
    data_val_x = competition_test_data[ , feature_names, with = F], 
    nround = hp_grid_xgb[1, "nround"], 
    max_depth = hp_grid_xgb[1, "max_depth"], 
    gamma = hp_grid_xgb[1, "gamma"], 
    lambda = hp_grid_xgb[1, "lambda"], 
    eta = hp_grid_xgb[1, "eta"],
    obj = hp_grid_xgb[1, "obj"]
  )

forecast_plot <- 
  competition_test_data[ , num_sold := model_fit$predictions] %>%
  group_by(
    date, 
    product
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  )

data %>%
  group_by(
    date, 
    product
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  ) %>%
  rows_append(
    forecast_plot
  ) %>%
  ggplot(aes(x = date, y = num_sold, color = product)) +
  geom_line()

submission_6 <- 
  data.table(
    "row_id" = competition_test_data$row_id, 
    "num_sold" = model_fit$predictions
  )
# public leaderboard; 6.80. Slightly better than submission 1, but it might as well be a fluke

fwrite(
  submission_6,
  file = "./Output/Submissions/submission_6.csv"
)

xgb.ggplot.importance(model_fit[["feature_importance"]])


# -- PROPHET -----------------------------------------------------------------

prophet_df <- 
  train_data %>%
  pivot_wider(
    id_cols = "date",
    values_from = num_sold,
    names_from = c("country", "store", "product")
  ) %>% as.data.table()

regressors <- names(train_data)[c(10, 12, 13)]

prophet_regressors <- 
  train_data[ 
    , 
    .SD[1], 
    by = date
  ][
    ,
    c("date", regressors),
    with = F
  ]

# -- forecast df complete with regressors. This is the same no matter which ts we are looking at

prophet_test_reg <- 
  competition_test_data[ 
    , 
    .SD[1], 
    by = date
  ][
    ,
    c("date", regressors),
    with = F
  ]

prophet_forecast_df <-
  prophet_regressors %>%
  rows_append(
    prophet_test_reg
  ) %>%
  rename(
    ds = date
  )

# -- model fit function

prophet_fit_regressors <- function(data_ts, training_years) {
  
  model <- prophet()
  model <- add_regressor(model, "weekday")
  model <- add_regressor(model, "christmas")
  model <- add_regressor(model, "new_year")
  
  model_df <- 
    data_ts[year(date) %in% training_years] %>%
    rename(
      ds = date, 
      y = 2
    ) %>% 
    left_join(
      y = prophet_regressors,
      by = c("ds" = "date")
    )
  
  model <- 
    fit.prophet(
      model, 
      model_df
    )
  
  # -- forecast
  
  forecast <- 
    predict(
      model,
      prophet_forecast_df[year(ds) %in% c(training_years, 2021)]
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

# -- Fit the model to each ts

all_forecasts <- 
  map(
    names(prophet_df)[-1],
    ~ prophet_fit_regressors(
        data_ts = prophet_df[ , c("date", .x), with = F],
        training_years = c(2017:2020) # training only on 2020 does need yields reasonable results
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

prophet_test_df <- 
  competition_test_data %>%
  mutate(
    ts = paste0(country, "_", store, "_", product)
  ) %>%
  left_join(
    y = all_yhat,
    by = c("ts" = "ts", "date" = "ds")
  )

# -- Plotting

prophet::dyplot.prophet(
  all_forecasts[[8]][["model"]],
  all_forecasts[[8]][["forecast"]]
)

forecast_plot <-
  prophet_test_df %>%
  group_by(
    date,
    product
  ) %>%
  summarise(
    num_sold = sum(yhat)
  )

data %>%
  group_by(
    date,
    product
  ) %>%
  summarise(
    num_sold = sum(num_sold)
  ) %>%
  rows_append(
    forecast_plot
  ) %>%
  ggplot(aes(x = date, y = num_sold, color = product)) +
  geom_line()

# -- Submission

submission_4 <- 
  data.table(
    "row_id" = prophet_test_df$row_id, 
    "num_sold" = prophet_test_df$yhat
  )
# public leaderboard; 11.3, so better than no regressors on 2019

fwrite(
  submission_4,
  file = "./Output/Submissions/submission_4.csv"
)







# -- ENSEMBLE ----------------------------------------------------------------

# Run xgboost and prophet sections first

# -- Plotting

forecast_plot_xgb <- 
  competition_test_data[ , num_sold := model_fit$predictions] %>%
  group_by(
    date, 
    product
  ) %>%
  summarise(
    num_sold_xgb = sum(num_sold)
  )

forecast_plot_prophet <-
  prophet_test_df %>%
  group_by(
    date,
    product
  ) %>%
  summarise(
    num_sold_prophet = sum(yhat)
  )

forecast_plot <- 
  forecast_plot_xgb 

forecast_plot$num_sold_prophet <- forecast_plot_prophet$num_sold_prophet

forecast_plot %>%
  ggplot() +
  geom_line(aes(x = date, y = num_sold_xgb), color = "blue") + 
  geom_line(aes(x = date, y = num_sold_prophet), color = "red") + 
  facet_wrap(~ product)

# -- simple averaging

submission_5 <- 
  data.table(
    "row_id" = competition_test_data$row_id,
    "num_sold" = (competition_test_data$num_sold +  prophet_test_df$yhat) / 2
  )
# public leaderboard; 7.9, so prophet makes it worse

fwrite(
  submission_5,
  file = "./Output/Submissions/submission_5.csv"
)
