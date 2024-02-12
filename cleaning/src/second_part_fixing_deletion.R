source("src/init.R")


## read raw.data

raw.main <- read_excel(strings['filename.data'], sheet = "main", col_types = "text")
raw.hh_roster <- read_excel(strings['filename.data'], sheet = "hh_roster", col_types = "text")
raw.ind_health <- read_excel(strings['filename.data'], sheet = "ind_health", col_types = "text")
raw.water_count_loop <- read_excel(strings['filename.data'], sheet = "water_count_loop", col_types = "text")
raw.child_nutrition <- read_excel(strings['filename.data'], sheet = "child_nutrition", col_types = "text")
raw.women <- read_excel(strings['filename.data'], sheet = "women", col_types = "text")
raw.died_member <- read_excel(strings['filename.data'], sheet = "died_member", col_types = "text")

tool.survey <- read_excel(strings['filename.tool'], sheet = "survey", col_types = "text")
tool.choices <- read_excel(strings['filename.tool'], sheet = "choices", col_types = "text")
label_colname <- load.label_colname(strings['filename.tool'])

##-----------------------------------------------------------------------------
# Check previous deletion.log.fast

files_deletion <- list.files("output/data_log/deletion")

if("first_deletion_batch.xlsx" %in% files_deletion){
  deletion.log.fast <- readxl::read_excel("./output/data_log/deletion/first_deletion_batch.xlsx")
}

# ------------------------------------------------------------------------------
# AFTER RECEIVING FILLED-OUT Deletion requests:

cleaning.log.deletion <- data.frame() 

or.request <- read_excel(paste0("output/checking/requests/",list.files("output/checking/requests/","deletion_requests")),sheet = "Sheet2", col_types = "text")


print(or.request)