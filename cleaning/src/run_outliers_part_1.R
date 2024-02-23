rm(list = ls())

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(svDialogs, stringr)

load("output/data_log/final_logical.rda")

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  n_sd = svDialogs::dlg_list(c(2,3,4), title = "Please Select the Number of SD", rstudio = getOption("svDialogs.rstudio", TRUE))$res,
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)    # this one is appended to the end of filenames
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/first_part_outliers_checks.R')
# -----------------------------------------------------------------------------`