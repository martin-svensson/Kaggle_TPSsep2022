# ====================================================================================================== #
# Description
#
#   Normalization and one-hot encoding (not necessary fo tree based methods, but it is necessary to neural networks).
#   Transformations are done functionally, so they can enter the model pipeline. 
#
# Change log:
#   Ver   Date        Comment
#   1.0   06/09/22    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(magrittr)
library(caret)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/1_split_row_id.RData")
load("./Output/1_data.RData")

# -- Source pipeline

pipeline <- new.env()

source(
  "./Scripts/2_EDA_implementation.R",
  local = pipeline
)

rm(
  list = na.omit(str_extract(ls(envir = pipeline), "^(?!fun_).*")),
  envir = pipeline
)

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

train_data <- data[!(row_id %in% split_row_id[["2020"]])]

train_data %<>%
  pipeline$fun_add_vars()

fun_encoding <- function(data, label, cat_vars) {
  
  # normalization of cont vars and one-hot encoding for cat var
  # Note: There are no cont_vars transformations in this specific case, since 
  # there is no reason to transform day_of_year
  
  data_tmp <- copy(data)
  
  one_hot <- 
    dummyVars(
      paste0(label, " ~ ."),
      data = data_tmp[ , c(label, cat_vars), with = F]
    )
  
  cat_vars_one_hot <- 
    predict(
      one_hot,
      data_tmp[ , c(label, cat_vars), with = F]
    ) %>% as.data.table
  
  # first level of each predictor is included in the intercept
  # relevel beforehand if another level should be the intercept (typically good practice to use the most common for non-ordinal vars)
  
  first_level <- 
    map_chr(
      cat_vars,
      ~ levels(data_tmp[[.x]])[1]
    )
  
  levels_remove <- paste0(cat_vars, ".", first_level)
  
  cat_vars_one_hot[ , (levels_remove) := NULL]
  
  # add intercept and one hot feats
  
  data_tmp[ , intercept := 1]
  
  data_tmp[ , (names(cat_vars_one_hot)) := cat_vars_one_hot]
  
  return(data_tmp)
  
}
