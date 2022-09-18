# ====================================================================================================== #
# Description
#
#   Functional implementation of findings from EDA to be sourced in later scripts (nothing is exported in this file).
#   The idea is akin to create model pipelines to ensure consistent transformations of both train and test data
#   Note that objects and libraries are loaded/sourced only to develop and test the functions (and are not removed for convenience if changes to the functions are needed)
#
# Change log:
#   Ver   Date        Comment
#   1.0   05/09/22    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/1_split_row_id.RData")
load("./Output/1_data.RData")
load("./Output/holidays_df.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

train_data <- data[!(row_id %in% split_row_id[["2020"]])]


# -- Time Variables ----------------------------------------------------------

fun_add_vars <- function(data, product_x_doy = TRUE, country_x_year = TRUE, holidays = NULL) {
  
  data_tmp <- copy(data)
  
  # -- Time vars
  
  data_tmp[
    ,
    `:=`(year = year(date), 
         month = as.factor(month(date)), 
         day_of_month = day(date), 
         day_of_year = yday(date), 
         weekday = as.factor(weekdays(date)))
  ]  
  
  # -- Weekend indicator
  
  data_tmp[
    ,
    weekend := ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0) 
  ]
  
  # -- COVID indicator
  
  if (max(year(data_tmp$date)) >= 2020) {
    
    data_tmp[
      ,
      covid := ifelse(date > as.IDate("2020-03-01") & date < as.IDate("2020-06-01"), 1 ,0)
    ]
    
  }
  
  # -- Important dates (manual)
  
  data_tmp[
    ,
    `:=`(christmas = ifelse(month == 12 & day_of_month > 26, 1, 0), 
         new_year = ifelse(month == 1 & day_of_month < 8, 1, 0))
  ]
  
  # -- Other holidays
  
  if (!is.null(holidays)) {
    
    data_tmp %<>%
      left_join(
        y = holidays_df[ , -"name"],
        by = c("date" = "date", "country" = "Name")
      ) %>%
      mutate(holiday_indic = coalesce(holiday_indic, 0L))
    
    data_tmp[ , country := as.factor(country)]
    
  }
  
  # -- Interaction feature between product and day of year (for yearly  seasonality)
  
  if (product_x_doy) {
    
    interact_1 <- 
      model.matrix(
        as.formula(row_id ~ day_of_year:product - 1),
        data = data_tmp
      ) %>% as.data.table()  
    
    data_tmp %<>%
      cbind(interact_1)
    
  }
  
  # -- Interaction between 2020+2021 and country (countries behave radically different in 2020 which we assume continues in 2021)
  
  if (country_x_year == TRUE & max(year(data_tmp$date)) == 2020) {
    
    data_tmp[ , odd_year := ifelse(year == 2020, 1, 0)]
    
    interact_2 <- 
      model.matrix(
        as.formula(row_id ~ odd_year:country - 1),
        data = data_tmp
      ) %>% as.data.table()  
    
    data_tmp %<>%
      cbind(interact_2)
    
  }
  
  if (country_x_year == TRUE & max(year(data_tmp$date)) == 2021) { # test data
    
    data_tmp[ , odd_year := ifelse(year == 2021, 1, 0)]
    
    interact_2_a <- 
      model.matrix(
        as.formula(row_id ~ odd_year:country - 1),
        data = data_tmp
      ) %>% as.data.table()  
    
    data_tmp %<>%
      cbind(interact_2_a)
    
  }
  
  return(data_tmp)
  
}


# test <- fun_add_vars(train_data, product_x_doy = TRUE, holidays = holidays_df)
