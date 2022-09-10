# ====================================================================================================== #
# Description
#
#   Data pre-processing         
#
# Change log:
#   Ver   Date        Comment
#   1.0   04/09/22    Initial version
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

data <- 
  list.files("./Data") %>%
  map(~ fread(file = paste0("./Data/", .x))) %>% 
  set_names(
    gsub("\\.csv", "", list.files("./Data"))
  )

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

# -- Variable Names ----------------------------------------------------------

# All names are fine


# -- Variable Types ----------------------------------------------------------

data %>% 
  map(str)

# ---- Character to factor

char_cols <- 
  data[["train"]][ 
    , 
    .SD, 
    .SDcols = is.character
  ] %>% names

data[["train"]][
  ,
  (char_cols) := map(.SD, as.factor),
  .SDcols = char_cols
]

data[["test"]][
  ,
  (char_cols) := map(.SD, as.factor),
  .SDcols = char_cols
]


# -- Data Split --------------------------------------------------------------

# We are forecasting and thus need to preserve the temporal dependency (ie. we can only use the present to predict the future)

split_row_id <- list()

split_row_id[["2020"]] <- 
  data[["train"]][year(date) == 2020, row_id]

split_row_id[["2019"]] <- 
  data[["train"]][year(date) == 2019, row_id]

split_row_id[["2018"]] <- 
  data[["train"]][year(date) == 2018, row_id]

split_row_id[["2017"]] <- 
  data[["train"]][year(date) == 2017, row_id]

# CV splits are created by year to allow  the model to learn seasonal effects. 
# Use 2017 to predict 2018 and 2017+2018 to predict 2019 in the CV. 


# ==== EXPORT ------------------------------------------------------------------------------------------ 

competition_test_data <- data[["test"]]
data <- data[["train"]]

save(
  competition_test_data, 
  file = "./Output/1_competition_test_data.RData"
)

save(
  data, 
  file = "./Output/1_data.RData"
)

save(
  split_row_id,
  file = "./Output/1_split_row_id.RData"
)
