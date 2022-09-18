# ====================================================================================================== #
# Description
#
#   Create holiday database. Source https://date.nager.at/Api
#
# Change log:
#   Ver   Date        Comment
#   1.0   17/09/22    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(magrittr)
library(jsonlite)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/1_split_row_id.RData")
load("./Output/1_data.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

country_codes <-
  data$country %>%
  levels %>%
  as.data.table %>%
  setnames(old = ".", new = "Name")

country_codes$code <- 
  c("BE",
    "FR",
    "DE",
    "IT",
    "PL",
    "ES")

year <- 
  data$date %>%
  year %>%
  unique %>%
  c(2021) # for test data

holiday_api <- 
  "https://date.nager.at/api/v2/publicholidays/"

holidays_df <- 
  pmap_dfr(
    .l = expand.grid("year" = year, "code" = country_codes$code),
    .f = function(year, code) { 
      fromJSON(paste0(holiday_api, year, "/", code))[ , c("date", "name", "countryCode", "global")] 
    }
  ) %>% as.data.table

holidays_df %<>%
  .[global == TRUE]

holidays_df %<>%
  .[!(month(date) == 12 | month(date) == 1)]
  # we take care of those manually

holidays_df %<>%
  left_join(
    y = country_codes, 
    by = c("countryCode" = "code")
  )

holidays_df[ , `:=`(global = NULL, countryCode = NULL)]

holidays_df$holiday_indic <- 1L

holidays_df[ , date := as.IDate(date)]

# ==== EXPORT ------------------------------------------------------------------------------------------ 

save(
  holidays_df,
  file = "./Output/holidays_df.RData"
)
