rm(list = ls())
source("src/utils/create_dummy.R")
data <- xlsform_fill_loop(tool.path = "resources/tool.xlsx",n = 20)

sum(as.numeric(data$main$num_hh))
