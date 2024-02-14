rm(list = ls())

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(svDialogs, stringr)

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name.short = str_split(list.files("output/data_log/data/","_data_translation_part_done"),"_data_translation_part_done")[[1]][1],
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  filename.data = paste0("output/data_log/data/",list.files("output/data_log/data/",pattern = "_data_translation_part_done")), 
  filename.tool = "output/data_log/tool/tool.xlsx"  # the filename of your data for 
)
params  <- c(
  fix_sheet_names_to_match = "data",     # this should be one of "tool", "data", or "none"
  combine_folder = "temp/combine/"
)
# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/first_part_logical_checks.R')
# -----------------------------------------------------------------------------`