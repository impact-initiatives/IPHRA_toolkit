# init for tabular analysis
# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, openxlsx, randomcoloR, sf, anytime, DT,
               cluster, survey, srvyr, knitr, webshot, docstring, tcltk, scales, deeplr)

devtools::install_github("impact-initiatives/addindicators")
devtools::install_github("REACH-WoU-Regional/translateR")

source("src/utils/misc_utils.R")
source("src/utils/check_kobo.R")
source("src/utils/kobo_utils.R")
source("src/utils/regional_detect_data_falsification.R")
source("src/utils/utils_audit.R")
source("src/utils/utils_cleaning.R")
source("src/utils/utils_cleaning_loops.R")
source("src/utils/utils_translate.R")


options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

dir.audits <- "data/inputs/audits/reach/"
dir.requests <- "output/checking/requests/"
dir.responses <- "output/checking/responses/"


# ------------------------------------------------------------------------------
enum_colname <- "enumerator"

# ------------------------------------------------------------------------------
# small utility function 
make.short.name <- function(name, no_date = F) return(gsub("__","_", paste0(strings['dataset.name.short'],"_", name, ifelse(no_date, "", paste0("_", strings['out_date'])))))
make.filename.xlsx <- function(dir = ".", name, no_date = F) return(gsub("//","/", paste0(dir, "/", make.short.name(name, no_date), ".xlsx")))

