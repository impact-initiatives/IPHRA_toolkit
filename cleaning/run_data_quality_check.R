setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())


## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name.short = gsub(" ", "_",dlgInput("Please provide name of assessmend (please dont use special characters)", "IPHRA_COUNTRY_DATE")$res),   # provide a short name for filenames of output documents (e.g. "POL_PDM")      # this string is only used for creating titles for output documents
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  raw_data = choose.files(caption = "Please select the raw_data (Raw Data from the IPHRA tool)", multi = F), 
  tool = choose.files(caption = "Please select the Kobo tool XLS form", multi = F)  # the filename of your data for 
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
rmarkdown::render('src/data_quality_plausibility_check.Rmd',
output_file = paste0("output/", strings['dataset.name.short'], "_Quality_Check_and_Plausibility_", strings['out_date'],".html"))
cat("\n> Quality Check completed! You can check your output folder.")
# -----------------------------------------------------------------------------`