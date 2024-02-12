rm(list = ls())

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(svDialogs)

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name.short = gsub(" ", "_",dlgInput("Please provide name of assessment (please fill country and dont use special characters)", "IPHRA_COUNTRY_CODE")$res),   # provide a short name for filenames of output documents (e.g. "POL_PDM")      # this string is only used for creating titles for output documents
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  filename.data = choose.files(caption = "Please select the raw data", multi = F), 
  filename.tool = choose.files(caption = "Please select the kobo tool", multi = F)  # the filename of your data for 
)
params  <- c(
  fix_sheet_names_to_match = "data",     # this should be one of "tool", "data", or "none"
  combine_folder = "temp/combine/"
)
# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/first_part_deletion_log.R')
# -----------------------------------------------------------------------------`