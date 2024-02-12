rm(list = ls())
# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, openxlsx, randomcoloR, sf, anytime, DT,
               cluster, survey, srvyr, knitr, webshot, docstring, tcltk, scales,svDialogs)
source("src/functions_create_dummy.R")
num <- as.numeric(svDialogs::dlg_input(message = "Please enter the number of submission to create the dummy data (only real number)")$res)
tool_path <- choose.files(caption ="Please select the tool to create the dummy data.", multi = F)
data <- xlsform_fill_loop(tool.path = tool_path,n = num)

# Sensitive data removed write excel
sheetsbinded <- list("main" = data$main,
                     "hh_roster" = data$hh_roster,
                     "ind_health" = data$ind_health,
                     "water_count_loop" = data$water_count_loop,
                     "child_nutrition" = data$child_nutrition,
                     "women" = data$women,
                     "died_member" = data$died_member)
write.xlsx(sheetsbinded, paste0("data/inputs/kobo_export/dummy_raw_data.xlsx"), overwrite = T)
## TO ADD CONSTRAINT TO THE LOOPS, AND TO CHECK CONSTRAINTS ON SELECT ONE AND SELECT_MULTIPLE
## TO CHECK CALCULATIONS
