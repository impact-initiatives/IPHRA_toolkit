rm(list = ls())

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(svDialogs, stringr)

load("output/data_log/final_deletion.RData")

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  language_other = svDialogs::dlg_list(choices = c("French","Arabic", "Spanish"), title="Please select others language", rstudio = getOption("svDialogs.rstudio", TRUE))$res,
  api = svDialogs::dlg_list(choices = c("Microsoft","DeepL","No Api"), title="Please select Translator", rstudio = getOption("svDialogs.rstudio", TRUE))$res,
  api_key = svDialogs::dlg_input(message="Please input your api:")$res,
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)      # this one is appended to the end of filenames
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/first_part_other_cleaning.R')
# -----------------------------------------------------------------------------`