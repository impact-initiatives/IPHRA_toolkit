source("src/init.R")

filename.dataset <- "data/inputs/kobo_export/ROM_PP2_rawdata.xlsx"

kobo.raw <- read_excel(filename.dataset, col_types = "text") %>%
  rename(uuid ="_uuid", index = "_index")

deletion.log <- read_excel(paste0("output/deletion_log/ROM_PP2_deletion_log.xlsx"), col_types = "text")
cleaning.log <- read_excel(paste0("output/cleaning_log/ROM_PP2_cleaning_log.xlsx"), col_types = "text")

create.count_collected_enu(kobo.raw,     "enumerator_num")
create.count_deleted_enu(deletion.log, "enumerator_num")
create.count_enu_cleaning(cleaning.log, "enumerator_num")

cat("\n> Done. Created 3 files in output/enum_performance.")
