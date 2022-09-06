# ====================================================================================================== #
# Description
#
#   Functional implementation of findings from EDA to be sourced in later scripts (nothing is exported in this file).
#   The idea is akin to create model pipelines to ensure consistent transformations of both train and test data
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
library(magrittr)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/1_split_row_id.RData")
load("./Output/1_data.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

train_data <- data[!(row_id %in% split_row_id[["test"]])]


# -- Time Variables ----------------------------------------------------------

fun_add_vars <- function(data) {
  
  data_tmp <- copy(data)
  
  # -- Time vars
  
  data_tmp[
    ,
    `:=`(month = as.factor(month(date)), day_of_month = day(date), day_of_year = yday(date), weekday = as.factor(weekdays(date)))
  ]  
  
  # -- Weekend indicator
  
  data_tmp[
    ,
    weekend := ifelse(weekday %in% c("Saturday", "Sunday"), 1, 0) 
  ]
  
  # -- Christmas
  
  data_tmp[
    ,
    holiday := ifelse((month == 12 & day_of_month > 26) | (month == 1 & day_of_month < 8), 1, 0)
  ]
  
  return(data_tmp)
}

# test <- fun_add_vars(train_data)
