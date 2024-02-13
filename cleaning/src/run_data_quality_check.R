setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())


## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name.short = "data",  # provide a short name for filenames of output documents (e.g. "POL_PDM")      # this string is only used for creating titles for output documents
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  filename.data = "data/inputs/kobo_export/raw_data_msna_car.xlsx", 
  filename.tool = "resources/tool_car_msna.xlsx"  # the filename of your data for 
)

params  <- c(
  fix_sheet_names_to_match = "data",     # this should be one of "tool", "data", or "none"
  combine_folder = "temp/combine/"
)
# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
rmarkdown::render('data_quality_plausibility_check.Rmd',
output_file = paste0("output/", strings['dataset.name.short'], "_Quality_Check_and_Plausibility_", strings['out_date'],".html"))
cat("\n> Quality Check completed! You can check your output folder.")
# -----------------------------------------------------------------------------`