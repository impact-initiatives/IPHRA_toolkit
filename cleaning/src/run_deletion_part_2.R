rm(list = ls())

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(svDialogs)

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name.short = str_split(list.files("output/checking/requests/"),"_deletion_requests")[[1]][1],
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  filename.data = "output/data_log/deletion/data/data_deletion_part_1.xlsx", 
  filename.tool = "output/data_log/tool/tool.xlsx"  # the filename of your data for 
)
params  <- c(
  fix_sheet_names_to_match = "data",     # this should be one of "tool", "data", or "none"
  combine_folder = "temp/combine/"
)
# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/second_part_fixing_deletion.R')
# -----------------------------------------------------------------------------`