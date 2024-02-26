rm(list = ls())

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(svDialogs, stringr)

load("output/data_log/final_outliers.rda")

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  filename.dataset = choose.files(caption = "Please select the raw data for the Enumerator Performance", multi = F),
  pwd = svDialogs::dlgInput("Please provide a password for your zipped file.")$res,
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)    # this one is appended to the end of filenames
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/final_part_cleaning.R')
# -----------------------------------------------------------------------------`