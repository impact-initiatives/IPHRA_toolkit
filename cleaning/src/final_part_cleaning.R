source("src/init.R")
options(warn=-1)
#-------------------------------------------------------------------------------
# 6) Remove PII columns, apply any last changes, then save cleaned dataset
################################################################################
if(!is.null(raw.died_member)){
  cleaning.log <- bind_rows(cleaning.log.deletion,
                            cleaning.log.other,
                            cleaning.log.trans,
                            cleaning.log.checks.direct,
                            cleaning.log.followups,
                            cleaning.log.dependency,
                            cleaning.log.outliers)
} else {
  cleaning.log <- bind_rows(cleaning.log.deletion,
                            cleaning.log.other,
                            cleaning.log.checks.direct,
                            cleaning.log.followups,
                            cleaning.log.dependency,
                            cleaning.log.outliers)
}
# finalize cleaning log:
cleaning.log <- cleaning.log %>% distinct() %>% 
  filter(old.value %!=na% new.value) %>% left_join(raw.main %>% select(uuid, any_of(enum_colname)))

# Output Cleaning Log
write.xlsx(cleaning.log, make.filename.xlsx("output/cleaning_log", "cleaning_log", no_date = T), overwrite = T)

pii.to.remove_main <- c(
  "deviceid",
  "audit",
  "audit_URL",
  "gps",
  "_gps_latitude",
  "_gps_longitude",
  "_gps_altitude",
  "_gps_precision")
raw.main.removed  <- raw.main %>% select(-any_of(pii.to.remove_main))

# All data write excel
datasheets <- list("main" = raw.main,
                   "hh_roster" = raw.hh_roster,
                   "ind_health" = raw.ind_health,
                   "water_count_loop" = raw.water_count_loop,
                   "child_nutrition" = raw.child_nutrition,
                   "women" = raw.women,
                   "died_member" = raw.died_member
)
write.xlsx(datasheets, paste0("output/data_log/data/",dataset.name.short, "_full_data",strings["out_date"],".xlsx"), overwrite = T,
           zoom = 90, firstRow = T)

datasheets_anon <- list("main" = raw.main.removed,
                        "hh_roster" = raw.hh_roster,
                        "ind_health" = raw.ind_health,
                        "water_count_loop" = raw.water_count_loop,
                        "child_nutrition" = raw.child_nutrition,
                        "women" = raw.women,
                        "died_member" = raw.died_member   #,  "incidents_what" = new.loop2
)
write.xlsx(datasheets_anon, paste0("output/final/",dataset.name.short, "_final_anonymized_data",strings["out_date"],".xlsx"), overwrite = T,
           zoom = 90, firstRow = T)

source("src/count_enum_performance.R")
source("src/package4validation.R")

save.image("output/data_log/final_Data.rda")

cat("\n\n#############################################################################################\n")
cat("Cleaning Process is Done. You can now proceed to Analysis.\n")
cat("#############################################################################################\n")
cat("\nD O N E\n")
options(warn=0)