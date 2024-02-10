rm(list = ls())
source("src/functions_create_dummy.R")
data <- xlsform_fill_loop(tool.path = "resources/tool.xlsx",n = 20)

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
