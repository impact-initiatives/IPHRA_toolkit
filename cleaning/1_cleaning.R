setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

####################
##  LOAD Tool     ##
####################
source("src/init.R")

####################
##  LOAD Data     ##
####################

source("src/load_Data.R")

cat(paste0("Always make sure to change the name of your output files in the function make.short.name in place of the ???"))
# small utility functions
dctime_short <- "2023_01_01"
make.short.name <- function(name, no_date = F) return(gsub("__","_", paste0("MDA_RAC_", name, ifelse(no_date, "", paste0("_", dctime_short)))))
make.filename.xlsx <- function(dir = ".", name, no_date = F) return(gsub("//","/", paste0(dir, "/", make.short.name(name, no_date), ".xlsx")))


## Section below only for research cycles that requires cleaning on regular basis and use one kobo server. 
cat(paste0("Section below only for research cycles that requires cleaning on regular basis and use one kobo server."))
#-------------------------------------------------------------------------------

# uuids_to_remove <- c()

# update this to the latest data log for this country or leave as-is
# filename_dataset_previous <- "output/data_log/..."
# 
# if(filename_dataset_previous != "output/data_log/..."){
#   main.data.previous <- read_xlsx(filename_dataset_previous, col_types = "text", sheet = 1)
#   loop1.previous <- read_xlsx(filename_dataset_previous, col_types = "text", sheet = 2)
#   loop2.previous <- read_xlsx(filename_dataset_previous, col_types = "text", sheet = 3)
#   uuids_to_remove <- main.data.previous$uuid
# }
# 
# deletion.log.previous <- load.requests("output/deletion_log", paste0(country_short, ".*_deletion_log"))
# uuids_to_remove <- c(uuids_to_remove, deletion.log.previous$uuid)
  
# ########################################
# ##  load & remove previous data        ##
# raw.main <- kobo.raw.main %>%          ##
#   filter(!(uuid %in% uuids_to_remove))
# raw.loop1 <- kobo.raw.loop1 %>%        ##
#   filter(!(uuid %in% uuids_to_remove))
# raw.loop2 <- kobo.raw.loop2 %>%        ##
#   filter(!(uuid %in% uuids_to_remove))
#                                        ##
########################################
raw.main <- data.list$main
## Add loops if needed. 

#-------------------------------------------------------------------------------
# 0) ANY CHANGES TO THE TOOL WHILE DATA COLLECTION :$
################################################################################



#-------------------------------------------------------------------------------
# 1) NO CONSENT + DUPLICATES --> deletion log
################################################################################

# check for duplicates 
ids <- raw.main$uuid[duplicated(raw.main$uuid)]
if (length(ids)>0) warning("Duplicate uuids detected: ", length(ids))
# add to deletion log
deletion.log.new <- create.deletion.log(raw.main %>% filter(uuid %in% ids),enum_colname, "Duplicate") # a brand new deletion log
rm(ids)

# check for duplicates center_id
ids <- raw.main$uuid[duplicated(raw.main$centre_id)]
if (length(ids)>0) warning("Duplicate center ids detected: ", length(ids))
# add to deletion log
deletion.log.new <- create.deletion.log(raw.main %>% filter(uuid %in% ids),enum_colname, "Duplicate") # a brand new deletion log
rm(ids)

# check for no consent
no_consents <- raw.main %>% filter(consent == "no")
if (nrow(no_consents) > 0) warning("No-consent detected: ", nrow(no_consents))
# add to deletion log
if("no_consent_why" %in% colnames(raw.main)){
  deletion.log.no_consents <- no_consents %>% 
    mutate(reason = paste0("no consent", ifelse(is.na(no_consent_why), "", paste0(": ", no_consent_why)))) %>% select(uuid, !!sym(enum_colname), reason)
  # translate the no-consents reasons :)
  deletion.log.no_consents <- deletion.log.no_consents %>% translate.responses("reason") %>% 
    mutate(reason = str_to_lower(response.en.from.uk)) %>% select(-response.en.from.uk)
}else{
  deletion.log.no_consents <- no_consents %>% create.deletion.log(enum_colname, "no consent")
}
deletion.log.new <- rbind(deletion.log.new, deletion.log.no_consents)

####################################################
## run this to remove duplicates and no-consents  ##
raw.main  <- raw.main[!(raw.main$uuid %in% deletion.log.new$uuid),]
# raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% deletion.log.new$uuid),]
# raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% deletion.log.new$uuid),]
####################################################

rm(no_consents, deletion.log.no_consents)


#-------------------------------------------------------------------------------
# 2) AUDIT CHECKS
################################################################################
## Survey durations 

# FOR POL: WE WERE NOT GIVEN ACCESS TO AUDIT FILES, so no audit logs will be found

audits <- load.audit.files(dir.audits, uuids = raw.main$uuid, track.changes = F) 

# save.image("environment.RData")
# load("environment.RData")

if(nrow(audits) == 0) {audits.summary <- tibble(uuid = raw.main$uuid, tot.rt = NA)
}else{
  audits.summary <- audits %>% 
    group_by(uuid) %>% 
    group_modify(~process.uuid(.x))
}

data.audit <- raw.main %>% 
  mutate(duration_mins = difftime(as.POSIXct(end), as.POSIXct(start), units = 'mins'),
         num_NA_cols = rowSums(is.na(raw.main)),
         num_dk_cols = rowSums(raw.main == "dk_undec", na.rm = T),
         num_other_cols = rowSums(!is.na(raw.main[str_ends(colnames(raw.main), "_other")]), na.rm = T)) # %>%
  # select(uuid, !!sym(enum_colname), start, end, duration_mins, num_NA_cols, num_dk_cols, num_other_cols)

audits.summary <- data.audit %>% 
  left_join(audits.summary, by="uuid") %>% select(-contains("/")) %>% 
  relocate(uuid, duration_mins, num_NA_cols, num_dk_cols, num_other_cols, tot.rt) %>% 
  arrange(duration_mins)

write.xlsx(audits.summary, make.filename.xlsx("output/checking/audit/", "audits_summary"))


# follow up with FPs if there are surveys under 10 minutes or above 1 hour
survey_durations_check <- audits.summary %>% filter(tot.rt < 10 | tot.rt > 60)
if(nrow(survey_durations_check) > 0){
  write.xlsx(survey_durations_check, make.filename.xlsx("output/checking/audit/", "survey_durations"),
             zoom = 90, firstRow = T)
}else cat("\nThere are no survey durations to check :)")


## Soft duplicates (less than 12 different columns?)

res.soft_duplicates <- find.similar.surveys(raw.main, tool.survey, uuid = "uuid") %>% 
  filter(number_different_columns <= 12)

if(nrow(res.soft_duplicates) > 0){
  write.xlsx(res.soft_duplicates, make.filename.xlsx("output/checking/audit/", "soft_duplicates"))
}else cat("\nThere are no soft duplicates to check :)")

rm(audits, data.audit)

#-------------------------------------------------------------------------------

# DECISIONs:
# interviews that were too fast and decided to be removed based on these uuids:
ids <- c(

)
deletion.log.too.fast <- create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                             enum_colname, "Survey duration deemed too fast.")
# soft duplicates to remove:
ids <- c(
  
)
deletion.log.softduplicates <- create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                                   enum_colname, "Soft duplicate")
# incomplete submissions to remove:
ids <- c(
  
)
deletion.log.incomplete <- create.deletion.log(raw.main %>% filter(uuid %in% ids), enum_colname, "Incomplete submission")

deletion.log.audits <- bind_rows(deletion.log.too.fast, deletion.log.softduplicates, deletion.log.incomplete)
deletion.log.new <- bind_rows(deletion.log.new, deletion.log.audits)

#################################################
##   removing fast submissions and duplicates  ##
raw.main  <- raw.main[! (raw.main$uuid  %in% deletion.log.audits$uuid),]
# raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% deletion.log.audits$uuid),]
# raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% deletion.log.audits$uuid),]
#################################################

rm(ids, deletion.log.too.fast, deletion.log.softduplicates)

# #-------------------------------------------------------------------------------
# # 3) LOOP INCONSITENCIES + SPATIAL CHECKS
# ################################################################################
# cat(paste0("This section is only for assessments that includes loops and need to be checked with their respective calculations."))
# cat(paste0("Make sure to adjust variable names accordingly."))
# ## check for inconsistency in loops:
# 
# counts_loop1 <- raw.loop1 %>% 
#   group_by(uuid) %>% 
#   summarize(loop1_count = n())
# loop_counts_main <- raw.main %>% select(uuid, !!sym(enum_colname), date_interview, hh_size) %>% left_join(counts_loop1) %>% 
#   mutate(hh_size = ifelse(hh_size == "999", NA, as.numeric(hh_size))) %>% 
#   filter(hh_size > 1 & loop1_count %!=na% (hh_size - 1))
# 
# if(nrow(loop_counts_main) > 0){
#   # look at the loop_counts (perhaps just send a screenshot to AO)
#   loop_counts_main %>% view(title = "Inconsistencies in loop1")
#   # find loops for inconsistent uuids:
#   inconsistent_loop1 <- loop_counts_main %>% left_join(raw.loop1)
# }else{ cat("No inconsistencies with loops! :)") }
# 
# 
# # DECISION: what to do with these inconsistencies?
# 
# ids_to_clean <- c(
#   # put here the uuids for which *variable* should be adjusted
# )
# loop_indexes_to_delete <- c(
#   # put here the loop indexes which should be removed
#   
# )
# ids_to_delete <- c(
#   # uuids of submissions that will be totally removed
# 
# )
# 
# cleaning.log.loop_inconsitency <- loop_counts_main %>% 
#   filter(uuid %in% ids_to_clean) %>% 
#   mutate(variable = "hh_size", loop_index = NA,
#          old.value = as.character(hh_size), new.value = ifelse(is.na(loop1_count),"1",as.character(loop1_count + 1)), issue = "Inconsistency in number of entries in hh loop") %>% 
#   select(any_of(CL_COLS))
# 
# deletion.log.loop_inconsistency <- tibble()
# dl_inconsistency1 <- create.deletion.log(pull.raw(loop_indexes_to_delete), 
#                                                        enum_colname, "Inconsistency in number of entries in hh loop")
# dl_inconsistency2 <- create.deletion.log(pull.raw(ids_to_delete), 
#                                          enum_colname, "Inconsistency in number of entries in hh loop") %>% 
#   mutate(loop_index = NA)
# deletion.log.loop_inconsistency <- rbind(dl_inconsistency1, dl_inconsistency2)
# 
# ####################################################
# ## run this to delete/clean entries               ##
# raw.loop1 <- raw.loop1[!raw.loop1$loop_index %in% dl_inconsistency1$uuid,]
# ##                                                ##
# raw.main  <- raw.main[! (raw.main$uuid  %in% dl_inconsistency2$uuid),]
# raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% dl_inconsistency2$uuid),]
# raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% dl_inconsistency2$uuid),]
# ##                                                ##
# raw.main <- raw.main %>% apply.changes(cleaning.log.loop_inconsitency)
# ####################################################
# 
# deletion.log.new <- bind_rows(deletion.log.new, deletion.log.loop_inconsistency) %>% 
#   relocate(loop_index, .after = uuid)
# cleaning.log <- cleaning.log.loop_inconsitency   # a brand new cleaning log
# 
# rm(ids_to_clean, loop_indexes_to_delete, counts_loop1)
# 
# #-------------------------------------------------------------------------------
# 
# ## GPS checks
# warning("No need to run if GPS checks is not needed.")
# 
# ## In case any needed 
# # run this section always (if POL) ------------------------  -------------------
# 
# if(country == "Poland"){
#   if(!"admin2" %in% colnames(raw.main)){ stop("admin2 column is missing from data")
#   }else{
#     # find if there are any "geopoint" variables in this data:
#     gps_vars <- tool.survey %>% filter(type == "geopoint") %>% pull(name)
#     if(length(gps_vars) == 0){ stop("there are no geopoint variables in tool.survey.")
#     } else if(length(gps_vars) > 1){
#       warning("Found more than one geopoint variable in tool.survey: ", paste(gps_vars, collapse = ", "),"\nOnly the first one will be used to match admin2!")
#       gps_vars <- gps_vars[1]
#     } else{
#       gps_cols <- raw.main %>% select(contains(gps_vars)) %>% names   # there should be 4 of these
#       if(length(gps_cols) < 2) {stop("columns with geopoint data were not found in data")}
#     }
#   }
#   sf_use_s2(TRUE)
#   admin2_boundary <- st_read(dsn = "resources/pol_admbnda_adm2.json") %>% 
#     mutate(ADM2_PCODE = ifelse(is.na(ADM2_PCODE), NA, paste0("20", str_replace(ADM2_PCODE, "PL", "POL"))))  # this is to standardize geo data to our pcode format
#   
#   # TODO additional check for low precision??
#   
#   collected_pts <- raw.main %>% filter(!is.na(!!sym(gps_cols[1]))) %>% 
#     select(uuid, !!sym(enum_colname), admin2, contains(gps_vars)) %>% 
#     left_join(admin2.list) %>%
#     rename(selected_admin2_label = label, selected_admin2 = admin2) %>% 
#     mutate(selected_admin2_label = str_to_title(selected_admin2_label, "pl")) 
#   
#   collected_sf <- collected_pts %>% st_as_sf(coords = paste0(gps_vars, c("_longitude", "_latitude")), crs = "+proj=longlat +datum=WGS84")
#   
#   sf_use_s2(FALSE)
#   
#   spatial_join <- st_join(collected_sf, admin2_boundary, join = st_within) %>% 
#     select(-geometry) %>% select(-contains(gps_vars)) %>% 
#     mutate(GPS_MATCH = ifelse(is.na(ADM2_PCODE), "outside POL", ifelse(ADM2_PCODE == selected_admin2, "match", "WRONG")))
#   
#   if(any(spatial_join$GPS_MATCH != "match")){
#     
#     check_spatial <- tibble(spatial_join) %>% 
#       rename(within_admin2 = ADM2_PCODE, within_admin2_label = ADM2_PL) %>% 
#       left_join(collected_pts %>% select(uuid, contains(gps_vars))) %>% 
#       select(uuid, !!sym(enum_colname), GPS_MATCH, contains(gps_vars), contains("admin2")) %>% 
#       filter(GPS_MATCH != "match")
#     # %>% view
#     
#     write.xlsx(check_spatial, make.filename.xlsx("output/checking/audit/", "gps_checks"), overwrite = T)
#     rm(collected_sf, spatial_join, admin2_boundary)
#     
#   }else cat("All GPS points are matching their selected poviat :)")
# }
# 
# #-------------------------------------------------------------------------------
# 
# # run this section only if there is need to recode spatial data 
# 
# cleaning.log.spatial <- tibble()
# 
# if(country == "Poland"){
#   # for gps points outside Poland, set them to NA immediately
#   check_outside_POL <- check_spatial %>% filter(GPS_MATCH == "outside POL")
#   
#   cleaning.log.spatial <- rbind(cleaning.log.spatial, check_outside_POL %>% 
#     recode.set.NA.regex(gps_cols, ".*", "GPS point is falling outside of Poland"))
#   
#   # how about the other points?
#   check_wrong_admin2 <- check_spatial %>% filter(GPS_MATCH == "WRONG")
# }
# 
# # DECISION: what to do with these inconsistencies:
# 
# # for these uuids, admin2 will be recoded to match the geolocation:
# ids <- c(
#   ## POL
#   "1f43f45b-2dd8-4553-9d1d-a50dc79b841e",
#   "88ff086f-32dc-48c2-b791-b65e85246fa9",    
#   "e1cb77b8-abfe-485f-b3cd-709baa88c419"
#   ##
# )
# cl.spatial_recode <- check_wrong_admin2 %>% filter(uuid %in% ids) %>%  
#   mutate(old.value = selected_admin2, new.value = within_admin2, variable = "admin2", issue = "Enumerator selected wrong poviat by mistake") %>% 
#   select(any_of(CL_COLS))
# 
# # for these uuids, remove geolocation data
# ids <- c(
#   
# )
# cl.spatial_remove_geo <- recode.set.NA.regex(pull.raw(ids), gps_cols, ".*", "Mismatch between selected admin2 and GPS location")
# 
# cleaning.log.spatial <- rbind(cleaning.log.spatial, cl.spatial_recode, cl.spatial_remove_geo)
# 
# # do we remove any suspicious surveys because of GPS mismatch?
# ids_remove <- c(
#   
# )
# deletion.log.new <- rbind(deletion.log.new,
#                           create.deletion.log(pull.raw(ids_remove), enum_colname, "Mismatch between selected admin2 and GPS location"))
# 
# # ------------------------------------
# raw.main <- raw.main %>% apply.changes(cleaning.log.spatial)
# cleaning.log <- bind_rows(cleaning.log, cleaning.log.spatial)

#################################################
raw.main  <- raw.main[! (raw.main$uuid  %in% deletion.log.new$uuid),]
# raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% deletion.log.new$uuid),]
# raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% deletion.log.new$uuid),]
#################################################

# deletion log should be now finalized

# Save deletion.log file
deletion.log.whole <- rbind(deletion.log.previous, deletion.log.new)
write.xlsx(deletion.log.whole, make.filename.xlsx("output/deletion_log/", "deletion_log", no_date = T), overwrite=T)


#-------------------------------------------------------------------------------
# 3) OTHERS AND TRANSLATIONS
################################################################################

other.db <- get.other.db()

other.db.main  <- other.db[other.db$name %in% colnames(raw.main),]
# there are no other question in loop1
other.db.loop2 <- other.db[other.db$name %in% colnames(raw.loop2),]


other.responses <- find.responses(raw.main, other.db.main, values_to = "response.ro")


other.responses.j <- other.responses %>% translate.responses(values_from = "response.ro", language_codes = "ro")

save.other.requests(create.translate.requests(other.db, other.responses.j, is.loop = T),
                    make.short.name("other_requests"), use_template = T)

# ------------------------------------------------------------------------------

# THERE IS NOTHING TO TRANSLATE UNTIL WE GET SOME DATA FROM LOOP2

# translate all text questions, but skip these columns:
trans_cols_to_skip <- c(
  # add columns to skip
  )
trans.db <- get.trans.db() %>% filter(!name %in% trans_cols_to_skip)

# trans.db.main <- trans.db[trans.db$name %in% colnames(raw.main),]
# trans.responses.main <- rbind(find.responses(raw.main, trans.db.main))

# trans.db.loop1 <- trans.db[trans.db$name %in% colnames(raw.loop1),]
# trans.responses.loop1 <- find.responses(raw.loop1, trans.db.loop1, is.loop = T)

trans.responses <- find.responses(raw.main, trans.db, values_to = "response.ro")
trans.responses.j <- trans.responses %>% translate.responses(values_from = "response.ro", language_codes = "ro")

save.trans.requests(create.translate.requests(trans.db, trans.responses.j, is.loop = T), make.short.name("translate_requests"))


# ------------------------------------------------------------------------------
# AFTER RECEIVING FILLED-OUT OTHER requests:
warning("!!!!Please remove cleaning.log line below if you have loops!!!!")
cleaning.log <- data.frame() 

or.request <- load.requests(dir.requests,  make.short.name("other_requests", no_date = T), sheet = "Sheet2") 
or.edited  <- load.requests(dir.responses, make.short.name("other_requests", no_date = T),
                            sheet = "Sheet2", validate = T) # specify Sheet2 because the first one is a readme


cleaning.log.other <- data.frame()
or.true.and.recode <- filter(or.edited, check == 1)
if (nrow(or.true.and.recode) > 0){
  cat(paste0("Multiple columns selected in ", nrow(or.true.and.recode)," or.edited entries:\n",
             paste0(or.true.and.recode %>% pull(uuid), collapse = "\n")), sep = "\n")
  if(any(or.true.and.recode$ref.type != "select_multiple")) stop("One of those is not a select_multiple!!!")
  # if(any(!is.na(or.true.and.recode$loop_index))) stop("Deal with loop code INSTEAD of UUID-LOOP-INDEX")
  issue <- "Recoding other response"
  for(r in 1:nrow(or.true.and.recode)){
    x <- or.true.and.recode[r,]
    # get list of choices from other response
    if (str_detect(x$existing.v, ";")) {
      choices <- str_trim(str_split(x$existing.v, ";")[[1]])
    } else {
      choices <- str_trim(str_split(x$existing.v, "\r\n")[[1]])
    }
    choices <- choices[choices!=""]
    if(is.na(x$loop_index)){
      old.value <- as.character(raw.main[raw.main$uuid==x$uuid[1], x$ref.name[1]])
    } else {
      old.value <- as.character(raw.loop1[raw.loop1$loop_index==x$loop_index[1], x$ref.name[1]])
    }
    
    l <- str_split(old.value, " ")[[1]]
    # add to the cleaning log each choice in the other response
    for (choice in choices){
      # set corresponding variable to "1" if not already "1"
      list.name <- get.choice.list.from.name(x$ref.name)
      new.code <- filter(tool.choices, list_name==list.name & !!sym(label_colname)==choice)
      if (nrow(new.code)!=1){
        warning(paste0("Choice is not in the list. UUID: ", x$uuid,"; recode.into: ", choice))
        return("err")
      }
      variable.name <- paste0(x$ref.name, "/", new.code$name)
      if(is.na(x$loop_index)){
        if (variable.name %in% colnames(raw.main)){
          old.boolean <- raw.main[[variable.name]][raw.main$uuid==x$uuid[1]]
        } else warning("Column not found")
      } else {
        if (variable.name %in% colnames(raw.loop1)){
          old.boolean <- raw.loop1[[variable.name]][raw.loop1$loop_index==x$loop_index[1]]
        } else warning("Column not found")
      }
      df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=variable.name, issue=issue,
                       old.value=old.boolean, new.value="1")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
      l <- unique(c(l, new.code$name))
    }
    # update cumulative variable
    new.value <- paste(sort(l), collapse=" ")
    df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$ref.name, issue=issue,
                     old.value=old.value, new.value=new.value)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
  or.edited <- or.edited %>% filter(check == 2)
}

or.true <- filter(or.edited, !is.na(true.v))
or.recode <- filter(or.edited, !is.na(existing.v))
or.remove <- filter(or.edited, !is.na(invalid.v))


# 1) handle invalid
print(paste("Number of responses to be deleted:", nrow(or.remove)))
if (nrow(or.remove)>0){
  for (r in 1:nrow(or.remove)) {
    if(is.na(or.remove$loop_index[r])){
      add.to.cleaning.log.other.remove(raw.main, or.remove[r,])
    } else{
      add.to.cleaning.log.other.remove(raw.loop1, or.remove[r,])
    } 
  }
} 


# 2) handle recoding
print(paste("Number of responses to be recoded:", nrow(or.recode)))
if (nrow(or.recode)>0){
  for (r in 1:nrow(or.recode)) {
    if(is.na(or.recode$loop_index[r])){
      add.to.cleaning.log.other.recode(raw.main, or.recode[r,])
    } else {
      add.to.cleaning.log.other.recode(raw.loop1, or.recode[r,])
    }
  }
}

# 3) handle true\
or.true <- rbind(or.true, or.true.and.recode)
print(paste("Number of responses to be translated:", nrow(or.true)))
t <- or.true %>%
  mutate(issue = "Translating other responses") %>%
  rename(variable=name, old.value=response.ro, new.value=true.v) %>%
  select(uuid, variable,issue, old.value, new.value)
cleaning.log.other <- rbind(cleaning.log.other, t)


## if you have more sheets use the following format

# cleaning.log.other <- bind_rows(recode.others(raw.main,or.edited,"response.uk",is.loop = F),
#                                 recode.others(raw.loop1,or.edited,"response.uk",is.loop = T),
#                                 etc.)

raw.main <- raw.main %>% 
  apply.changes(cleaning.log.other)
## add loops if needed. 

cleaning.log <- bind_rows(cleaning.log, cleaning.log.other #, cleaning.log.other_loop2
                      ) 

### ADD translation cleaning if needed. 

trans <- load.edited(dir.responses, "trans")
colnames(trans)[str_starts(colnames(trans), "TRUE")] <- "true.trans"
colnames(trans)[str_starts(colnames(trans), "EXISTING")] <- "existing.trans"
colnames(trans)[str_starts(colnames(trans), "INVALID")] <- "invalid.trans"
trans$check <- rowSums(is.na(select(trans, true.trans, invalid.trans)))
t.trans <- filter(trans, check!=2)
# if (nrow(t.trans)>0) stop("Missing entries or multiple columns selected")
trans.true <- filter(trans, !is.na(true.trans))
# trans.recode <- filter(trans, !is.na(existing.trans))
trans.remove <- filter(trans, !is.na(invalid.trans))
# if (nrow(bind_rows(trans.true, trans.recode, trans.remove))!=nrow(trans)) stop()

cleaning.log.trans <- data.frame()

# 1) handle invalid
print(paste("Number of responses to be deleted:", nrow(trans.remove)))
if (nrow(trans.remove)>0) for (r in 1:nrow(trans.remove)) add.to.cleaning.log.trans.remove.LOOP(raw.main, trans.remove[r,])

# # 2) handle recoding
# print(paste("Number of responses to be recoded:", nrow(trans.recode)))
# if (nrow(trans.recode)>0) for (r in 1:nrow(trans.recode)) add.to.cleaning.log.other.recode.LOOP(raw.main, trans.recode[r,])

# 3) handle true
print(paste("Number of responses to be translated:", nrow(trans.true)))
t.trans <- trans.true %>%
  mutate(issue = "Translating other responses") %>%
  rename(variable=name, old.value=response.ro, new.value=true.trans) %>%
  select(uuid, loop_index, variable,issue, old.value, new.value)
cleaning.log.trans <- rbind(cleaning.log.trans, t.trans)

raw.main <- raw.main %>% 
  apply.changes(cleaning.log.trans)


cleaning.log <- bind_rows(cleaning.log, cleaning.log.trans)
#-------------------------------------------------------------------------------
# 4) LOGIC CHECKS
################################################################################

# 4A) direct cleaning

## Change to match your assessment

cleaning.log.checks.direct <- tibble()

## replace all '999's with NA:
int_cols_main  <- tool.survey %>% filter(type == "integer" & datasheet == "main") %>% pull(name)
int_cols_loop1 <- tool.survey %>% filter(type == "integer" & datasheet != "main") %>% pull(name)

check_hh_size_999 <- raw.main %>% 
  mutate(flag = ifelse(hh_size == "999",T,F),
         variable = "hh_size",
         loop_index = NA,
         issue = "Change 999 in HH size to 1",
         old.value = hh_size,
         new.value = "1") %>% 
  filter(flag) %>% select(uuid,loop_index,variable,old.value,new.value,issue,enumerator_num)
raw.main <- raw.main %>% apply.changes(check_hh_size_999)
# cl_999s <- bind_rows(recode.set.NA.if(raw.main, int_cols_main, "999", "replacing 999 with NA"),
#                      recode.set.NA.if(raw.loop1, int_cols_loop1, "999", "replacing 999 with NA"))
# cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_999s)

check_children_missing_doc <- raw.main %>% filter(children_sum == "0" & str_detect(missing_documentation, "birth_certificate"))
cl_children_missing_doc <- check_children_missing_doc %>% 
  recode.multiple.remove.choices("missing_documentation", "birth_certificate", "Invalid response because there are no children reported in this HH")
cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_children_missing_doc)

check_school_age_enrol <- raw.main %>% 
  mutate(flag = ifelse(school_enrol == "yes_some" & school_age_sum == "1",T,F),
         variable = "school_enrol",
         loop_index = NA,
         issue = "Change yes_some to yes_all",
         old.value = school_enrol,
         new.value = "yes_all") %>% filter(flag) %>% select(uuid,loop_index,variable,old.value,new.value,issue,enumerator_num)
cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, check_school_age_enrol)

check_diff_work <- raw.main %>% 
  mutate(flag = ifelse(diff_work %in% c("lack_lang lack_info discrim none",
                                        "lack_lang none",
                                        "not_staying none"), T,F),
         variable = "diff_work",
         loop_index = NA,
         issue = "Remove none",
         old.value = diff_work,
         new.value = stringr::str_remove(diff_work, " none")) %>% filter(flag) %>% select(uuid,loop_index,variable,old.value,new.value,issue,enumerator_num)

cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, check_diff_work)

check_diff_work_none <- raw.main %>% 
  mutate(flag = ifelse(diff_work %in% c("lack_lang lack_info discrim none",
                                        "lack_lang none",
                                        "not_staying none"), T,F),
         variable = "diff_work/none",
         loop_index = NA,
         issue = "Remove none",
         old.value = `diff_work/none`,
         new.value = "0") %>% filter(flag) %>% select(uuid,loop_index,variable,old.value,new.value,issue,enumerator_num)

cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, check_diff_work_none)

check_how_receive_info <- raw.main %>% 
  mutate(flag = ifelse(how_receive_info %in% c("dont_know none",
                                               "social_media by_phone email none"), T,F),
         variable = "how_receive_info",
         loop_index = NA,
         issue = "Remove none",
         old.value = how_receive_info,
         new.value = stringr::str_remove(how_receive_info, " none")) %>% filter(flag) %>% select(uuid,loop_index,variable,old.value,new.value,issue,enumerator_num)

cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, check_how_receive_info)

check_how_receive_info_none <- raw.main %>% 
  mutate(flag = ifelse(diff_work %in% c("dont_know none",
                                        "social_media by_phone email none"), T,F),
         variable = "how_receive_info/none",
         loop_index = NA,
         issue = "Remove none",
         old.value = `how_receive_info/none`,
         new.value = "0") %>% filter(flag) %>% select(uuid,loop_index,variable,old.value,new.value,issue,enumerator_num)

cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, check_how_receive_info_none)

#####
raw.main  <- raw.main  %>% apply.changes(cleaning.log.checks.direct)
raw.loop1 <- raw.loop1 %>% apply.changes(cleaning.log.checks.direct, is.loop = T)
raw.loop2 <- raw.loop2 %>% apply.changes(cleaning.log.checks.direct, is.loop = T)

cleaning.log.checks.direct <- rbind(cleaning.log.checks.direct,check_hh_size_999)
cleaning.log <- bind_rows(cleaning.log, cleaning.log.checks.direct)


# ------------------------------------------------------------------------------

# 4B) checks for followups

checks_followups <- tibble()

# if hh member is a child/grandchild, then their age should be lower than respondent's
# and vice versa if they are parent/grandparent
check_hh_relations <- raw.loop1 %>% select(uuid, loop_index, age_hh_member, relation_hh_member) %>% 
  left_join(raw.main %>% select(uuid, resp_age, !!sym(enum_colname), today, place_of_interview)) %>% mutate(resp_age = as.numeric(resp_age), age_hh_member = as.numeric(age_hh_member)) %>% 
  filter(resp_age < age_hh_member & relation_hh_member %in% c("child", "grandchild") | resp_age > age_hh_member & relation_hh_member %in% c("parent", "grandparent"))

checks_followups <- rbind(checks_followups,
                          make.logical.check.entry(check_hh_relations, 1,  c("age_hh_member", "relation_hh_member"), 
                                             "HH member is younger/older than respondent, but is their parent/child",
                                             cols_to_keep = c("today", enum_colname, "place_of_interview", "resp_age"), T))

# if currently or previously retired, check if age is below 40
check_retired <- raw.main %>% filter(as.numeric(resp_age) < 40 & (work_coa == "retired" | resp_activity == "retired"))

checks_followups <- rbind(checks_followups,
                          make.logical.check.entry(check_retired, 2,  c("work_coa", "resp_activity"), 
                                                   "Respondent is under 40 years old and reported being retired",
                                                   cols_to_keep = c("today", enum_colname, "place_of_interview", "resp_age"), T))

# if age is > 60, check if they are a student or intern
check_students_interns <- raw.main %>% filter(resp_age >= 60 & (work_coa %in% c("student", "intern") | resp_activity == "student"))

checks_followups <- rbind(checks_followups,
                          make.logical.check.entry(check_students_interns, 3,  c("work_coa", "resp_activity"), 
                                                   "Respondent is over 60 years old and reported being a student or intern",
                                                   cols_to_keep = c("today", enum_colname, "place_of_interview", "resp_age"), T))

# if asked to return to Ukraine by employer, check if unemployed/retired
check_returned_employer <- raw.main %>% filter(str_detect(temp_return_reason, "asked_empl") & 
                                                 work_coa %in% c("retired", "unemployed" ))

checks_followups <- rbind(checks_followups,
                          make.logical.check.entry(check_returned_employer, 4,  c("work_coa", "temp_return_reason"), 
                                                   "Respondent stated they returned to Ukraine after employer's request, bus is currently unemployed/retired",
                                                   cols_to_keep = c("today", enum_colname, "place_of_interview", "resp_age"), T))

####
create.follow.up.requests(checks_followups, make.short.name(paste0("follow-up_requests_",dctime_short,".xlsx"), no_date = T))


# ------------------------------------------------------------------------------
# AFTER RECEIVING filled followup requests:

fu.edited <- load.requests(dir.responses, make.short.name("follow-up", no_date = T)) %>%
  mutate(modified = !is.na(invalid) | (!is.na(new.value) & new.value != old.value),
         check = !is.na(invalid) & !is.na(new.value)) 

fu.check <- fu.edited %>% filter(check)
if(nrow(fu.check)>0) warning("Two entries in invalid and new.value columns:\n", paste0(fu.check$uuid, collapse = "\n"))

# go ahead only if no warnings found above...
fu.edited <- fu.edited %>% filter(modified) %>%
  mutate(new.value = ifelse(!is.na(invalid) & invalid == "yes", NA, new.value),
         issue = ifelse(!is.na(explanation), paste0("Information followed up with field team: ", explanation), issue))

if (nrow(fu.edited)> 0) {
  # check that new.value for select_one and select_multiple is one of the existing option
  list.options <- tool.choices %>% filter(list_name %in% get.choice.list.from.name(fu.edited$variable)) %>% 
    group_by(list_name) %>% summarise(options=paste0(paste0(name, collapse=";"), ";"))
  fu.check.all <- fu.edited %>%
    left_join(select(tool.survey, name, q.type, list_name), by=c("variable"="name")) %>%
    left_join(list.options, by="list_name")
  if (nrow(fu.check.all)!=nrow(fu.edited)) stop("Something went wrong with left_join")
}

# recode select_multiples
fu.multiple <- fu.edited %>% filter(get.type(variable) == "select_multiple")

if(any(is.na(fu.multiple$invalid))) stop("Select multiples cannot be invalid. Set them to 0 instead!")
cleaning.log.followups_multiple <- tibble()

if(nrow(fu.multiple)>0){
  main_row <- pull.raw(ifelse(is.na(fu.multiple[1,]$loop_index), fu.multiple[1,]$uuid, fu.multiple[1,]$loop_index))
  for(r in 1:nrow(fu.multiple)){
    x <- fu.multiple[r,]
    if(is.na(x$loop_index)){
      if(main_row$uuid %!=na% x$uuid) main_row <- pull.raw(x$uuid)
    } else{
      if (main_row$loop_index %!=na% x$loop_index) {
        main_row <- pull.raw(x$loop_index)
      }
    } 
    cummulative_variable  <-  str_split(x$variable, "/", simplify = T)[1]
    cchoice  <-  str_split(x$variable, "/", simplify = T)[2]
    # TODO: improve recoding here (use group_by and add/remove.choices) to not have to apply changes in every iteration
    if(x$new.value == "0"){
      cl <- recode.multiple.remove.choices(main_row, cummulative_variable, cchoice, issue = x$issue)
      main_row <- main_row %>% apply.changes(cl)
      cleaning.log.followups_multiple <- rbind(cleaning.log.followups_multiple, cl)
    }else{
      cl <- recode.multiple.add.choices(main_row, cummulative_variable, cchoice, issue = x$issue)
      main_row <- main_row %>% apply.changes(cl)
      cleaning.log.followups_multiple <- rbind(cleaning.log.followups_multiple, cl)
    }
  }
}
cleaning.log.followups <- fu.edited %>% filter(get.type(variable) != "select_multiple") %>% select(any_of(CL_COLS))
cleaning.log.followups <- rbind(cleaning.log.followups, cleaning.log.followups_multiple)

# apply changes
raw.main  <- raw.main  %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.main)))
# raw.loop1 <- raw.loop1 %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.loop1)))
# raw.loop2 <- raw.loop2 %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.loop2)))

cleaning.log <- bind_rows(cleaning.log, cleaning.log.followups)

# ------------------------------------------------------------------------------

#############################################################################################################
# 5) Outliers
#############################################################################################################
# save.image(file = "Environment.RData")
# load("Environment.RData")

cleaning.log.outliers <- data.frame()
# define columns to check for outliers

cols.integer_main <- filter(tool.survey, type == "integer")
cols.integer_raw.main <- cols.integer_main[cols.integer_main$name %in% colnames(raw.main),] %>% pull(name)
# cols.integer_raw.loop1 <- cols.integer_main[cols.integer_main$name %in% colnames(raw.loop1),] %>% pull(name)

# cols <- filter(tool.survey, str_starts(name, "G_3")) %>% pull(name)

n.sd <- 2

res.outliers_main <- data.frame()
# res.outliers_loop1 <- data.frame()
df.all <- data.frame()
#------------------------------------------------------------------------------------------------------------
# [MAIN SHEET] -> detect outliers 

raw.main.outliers <- raw.main %>%
  select("uuid", cols.integer_raw.main) %>%
  mutate_at(cols.integer_raw.main, as.numeric)

# Outliers per country

for (col in cols.integer_raw.main) {
  values <- raw.main.outliers %>% 
    filter(!!sym(col) %_>_% 0) %>% 
    rename(value=col) %>%  select(uuid, value) %>% 
    mutate(value.log=log10(value)) %>%  mutate(variable=col) %>% 
    mutate(is.outlier.lin = (value > mean(value) + n.sd*sd(value)) |
             (value < mean(value) - n.sd*sd(value)),
           is.outlier.log = (value.log > mean(value.log) + n.sd*sd(value.log)) |
             (value.log < mean(value.log) - n.sd*sd(value.log)))
  values <- filter(values, is.outlier.log) %>%  select(uuid, variable, value)
  if (nrow(values)>0) print(paste0(col, ": ", nrow(values), " outliers detected"))
  res.outliers_main <- rbind(res.outliers_main, values)
}

f.alpha <- function(x) return(ifelse(x, 1, 0))

# Outliers Boxplot generator per country

df <- raw.main.outliers %>% 
  select(uuid, all_of(cols.integer_raw.main)) %>% 
  pivot_longer(-uuid, names_to = "variable", values_to = "value") %>% 
  mutate(value.log = log10(value)) %>% 
  left_join(select(res.outliers_main, -value) %>% mutate(is.outlier=T), by = c("uuid","variable")) %>% 
  mutate(is.outlier = ifelse(is.na(is.outlier), F, is.outlier)) %>% 
  filter(!is.na(value) & value>0)
df <- gather(df, key = "measure", value = "value", variable)
df.all <- rbind(df.all, df)


write.xlsx(df.all, paste0("output/checking/outliers/main_outlier_prices_analysis_", n.sd, "sd.xlsx"), overwrite=T)

# generating prices boxplots for same locations
g.outliers_main <- ggplot(df.all) +
  geom_boxplot(aes(x= measure, y=value.log), width = 0.2) + ylab("Values (log10)") +
  geom_point(aes(x=measure, y=value.log, group = measure), alpha=f.alpha(df.all$is.outlier), colour="red") +
  facet_wrap(~value, ncol = 4, scales = "free_y")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# Save
ggsave(paste0("output/checking/outliers/main_outlier_prices_analysis_", n.sd, "sd.pdf"), g.outliers_main, 
       width = 40, height = 80, units = "cm", device="pdf")


# Output requests to check
res.outliers_main <- res.outliers_main %>% 
  mutate(issue = "Outliers",
         loop_index = NA,
         new.value = NA,
         explanation=NA) %>% 
  rename("old.value"=value) %>% 
  select(uuid,loop_index,variable,issue,old.value,new.value,explanation)

cleaning.log.outliers <- rbind(cleaning.log.outliers,res.outliers_main)

save.outlier.responses_msna(cleaning.log.outliers)  

#------------------------------------------------------------------------------------------------------------
# --> edit the file
# --> Manually check outliers and change to NA (Decision made with country FPS)
# --> save new file as outliers_responses_edited.xlsx in output/checking/responses/
#------------------------------------------------------------------------------------------------------------

# RUN ONLY IF Anything need to be changed

outlier.recode <- load.edited(dir.responses, "outliers")
outlier.check <- load.edited(dir.requests, "outliers")

if (nrow(outlier.check) != nrow(outlier.recode)) warning("Number of rows are not matching")

cleaning.log.outliers <- outlier.recode %>%
  select(uuid,loop_index,variable,issue,old.value,new.value) %>%
  filter(is.na(new.value))

raw.main <- raw.main %>% 
  apply.changes(cleaning.log.outliers)

cleaning.log <- rbind(cleaning.log,cleaning.log.outliers)


#-------------------------------------------------------------------------------
# 6) Remove PII columns, apply any last changes, then save cleaned dataset
################################################################################

# finalize cleaning log:
cleaning.log <- cleaning.log %>% distinct() %>% 
  filter(old.value %!=na% new.value) %>% left_join(raw.main %>% select(uuid, any_of(enum_colname)))

if (length(list.files(make.filename.xlsx("output/cleaning_log", "cleaning_log", no_date = T))) > 0) {
  cleaning.log.previous <- read_xlsx(make.filename.xlsx("output/cleaning_log", "cleaning_log"))
  cleaning.log.whole <- rbind(cleaning.log.previous, cleaning.log)
} else cleaning.log.whole <- cleaning.log

# Output Cleaning Log
write.xlsx(cleaning.log, make.filename.xlsx("output/cleaning_log", "cleaning_log", no_date = T), overwrite = T)


# combine new and previous data:
# ------------------------------------------------------------------------------

if(!"main.data.previous" %in% ls()) {
  warning("main.data.previous was not found! Are you sure you don't have a previous data_log that you can load?")
  new.main <- raw.main
  # new.loop1 <- raw.loop1
  # new.loop2 <- raw.loop2
}else{
  # check if there are any columns somehow added during this cleaning process
  for(cc in colnames(raw.main)){
    if(!(cc %in% colnames(main.data.previous))){
      warning(paste("column",cc,"found in raw.main but not in main.data.previous!"))
    }}
  # and the other way around:
  for(cc in colnames(main.data.previous)){
    if(!(cc %in% colnames(raw.main))){
      warning(paste("column",cc,"found in main.data.previous but not in raw.main!"))
    }}
  
  new.main  <- bind_rows(main.data.previous, raw.main) %>% filter(!uuid %in% deletion.log.whole$uuid)
  # new.loop1 <- bind_rows(loop1.previous, raw.loop1) %>% filter(!uuid %in% deletion.log.whole$uuid)
  # new.loop2 <- bind_rows(loop2.previous, raw.loop2) %>% filter(!uuid %in% deletion.log.whole$uuid)
}
pii.to.remove_main <- c(
  "deviceid",
  "staff_other",
  "audit",
  "audit_URL",
  "username")
new.main.removed  <- new.main %>% select(-any_of(pii.to.remove_main))

# All data write excel
datasheets <- list("main" = new.main,
               "hh_compos" = new.loop1  #, "incidents_what" = new.loop2
               )
write.xlsx(datasheets, make.filename.xlsx("output/data_log", "full_data"), overwrite = T,
           zoom = 90, firstRow = T)

# final (pii removed)
datasheets_anon <- list("main" = new.main.removed,
                        "hh_compos" = new.loop1   #,  "incidents_what" = new.loop2
                        )
write.xlsx(datasheets_anon, make.filename.xlsx("output/final", "final_anonymized_data"), overwrite = T,
           zoom = 90, firstRow = T)

source("src/count_enum_performance.R")
source("package4validation.R")

cat("\nD O N E\n")
