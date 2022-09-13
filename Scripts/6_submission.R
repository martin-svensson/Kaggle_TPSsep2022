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
  pipeline$fun_add_vars() %>%
  pipeline$fun_encoding(
    label = "num_sold",
    cat_vars = c("country", "store", "product", "weekday")
  )

competition_test_data %<>%
  pipeline$fun_add_vars() %>%
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



# -- PROPHET -----------------------------------------------------------------





