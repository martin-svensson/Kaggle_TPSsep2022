# ====================================================================================================== #
# Description
#
#   Correlation analysis
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
library(car)

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

train_data <- data[!(row_id %in% split_row_id[["test"]])]

train_data %<>%
  pipeline$fun_add_vars()


# ---- VIF Analysis ------------------------------------------------------------

predictors_vif_1 <- 
  c("country",
    "store",
    "product",
    "weekend",
    "holiday")  

vif_model_1 <- 
  glm(
    as.formula(paste0("num_sold", "~", paste0(predictors_vif_1, collapse = "+"))),
    family = Gamma(link = "inverse"),
    data = train_data
  )

vif(vif_model_1)

# all good

