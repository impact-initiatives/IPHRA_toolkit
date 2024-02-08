rm(list = ls())
source("src/utils/create_dummy.R")
data <- xlsform_fill_loop(tool.path = "resources/tool.xlsx",n = 20)

## TO ADD CONSTRAINT TO THE LOOPS, AND TO CHECK CONSTRAINTS ON SELECT ONE AND SELECT_MULTIPLE
## TO CHECK CALCULATIONS
