
  rm(list = ls())
  
  # loading all packages, functions and the Kobo tool
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(svDialogs)
  
  
  ## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
  strings <- c(
    dataset.name.short = "TEST",
      # gsub(" ", "_",dlgInput("Please provide name of assessment (please fill country and dont use special characters)", "IPHRA_COUNTRY_CODE")$res),  # provide a short name for filenames of output documents (e.g. "POL_PDM")      # this string is only used for creating titles for output documents
    out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
    filename.data = "resources/dummy_raw_data.xlsx",
      # choose.files(caption = "Please select the raw data", multi = F), 
    filename.tool = "resources/IPHRA_tool_v1.xlsx"
      # choose.files(caption = "Please select the kobo tool", multi = F) 
  )
  
  params  <- c(
    fix_sheet_names_to_match = "data",     # this should be one of "tool", "data", or "none"
    combine_folder = "temp/combine/"
  )
  
  Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")

  rmarkdown::render('data_quality_plausibility_check.Rmd',
  output_file = paste0("output/quality_report/", strings['dataset.name.short'], "_Quality_Check_and_Plausibility_", strings['out_date'],".html"))
  cat("\n> Quality Check completed! You can check your output folder.")
  