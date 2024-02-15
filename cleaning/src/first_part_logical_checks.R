source("src/init.R")

## read raw.data

raw.main <- read_excel(strings['filename.data'], sheet = "main", col_types = "text")
raw.hh_roster <- read_excel(strings['filename.data'], sheet = "hh_roster", col_types = "text")
raw.ind_health <- read_excel(strings['filename.data'], sheet = "ind_health", col_types = "text")
raw.water_count_loop <- read_excel(strings['filename.data'], sheet = "water_count_loop", col_types = "text")
raw.child_nutrition <- read_excel(strings['filename.data'], sheet = "child_nutrition", col_types = "text")
raw.women <- read_excel(strings['filename.data'], sheet = "women", col_types = "text")
raw.died_member <- read_excel(strings['filename.data'], sheet = "died_member", col_types = "text")

## read tool
tool.survey <- read_excel(strings['filename.tool'], sheet = "survey", col_types = "text")
tool.choices <- read_excel(strings['filename.tool'], sheet = "choices", col_types = "text")
label_colname <- load.label_colname(strings['filename.tool'])


###-------------------------------------------------------------------------------
# 4) LOGIC CHECKS
################################################################################

# 4A) direct cleaning

## Change to match your assessment

cleaning.log.checks.direct <- tibble()

int_cols_main  <- tool.survey %>% filter(type == "integer" & datasheet == "main") %>% pull(name)
int_cols_hh_roster  <- tool.survey %>% filter(type == "integer" & datasheet == "hh_roster") %>% pull(name)
int_cols_water_count_loop  <- tool.survey %>% filter(type == "integer" & datasheet == "water_count_loop") %>% pull(name)
int_cols_died_member  <- tool.survey %>% filter(type == "integer" & datasheet == "died_member") %>% pull(name)


### Cleaning of 999s to NAs
cl_999s <- bind_rows(recode.set.NA.if(raw.main, int_cols_main, "999", "replacing 999 with NA"),
                     recode.set.NA.if(raw.hh_roster, int_cols_hh_roster, "999", "replacing 999 with NA"),
                     recode.set.NA.if(raw.water_count_loop, int_cols_water_count_loop, "999", "replacing 999 with NA"),
                     recode.set.NA.if(raw.died_member, int_cols_died_member, "999", "replacing 999 with NA"))

cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_999s)

### Food Security Direct changes

raw.main <- raw.main %>% 
  add_fcs_new(cutoffs = "normal") %>% 
  add_rcsi_new() %>%   
  add_hhs_new() %>% 
  add_lcsi_new() %>% 
  add_hdds_new() %>% 
  add_fcm_phase_new()

raw.flag <- raw.main %>% 
  check_fs_flags(date_dc_date = "start") ## CHANGE by removing date_dc_date

### FCS score is 0. 

fcs_columns <- names(raw.flag)[which(str_starts(names(raw.flag),"fcs_"))]
check <- raw.flag %>% filter(fcs_score == 0)
cl_fcs_all_0 <- data.frame()
for (i in 1:nrow(check)) {
  cl <-  recode.set.NA.if(check[i,], fcs_columns, check[i,fcs_columns], "replacing fcs columns with NA because all fcs are 0", ignore_case = F) %>% 
    filter(!is.na(old.value)) %>% 
    mutate(old.value = as.character(old.value))
  cl_fcs_all_0 <- bind_rows(cl_fcs_all_0,cl)
}

cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_fcs_all_0)


### FCS score is 7. 

fcs_columns <- names(raw.flag)[which(str_starts(names(raw.flag),"fcs_"))]

check <- raw.flag %>% filter(fcs_score == 112)
cl_fcs_all_7 <- data.frame()
for(i in 1:nrow(check)){
  cl<- recode.set.NA.if(check[i,], fcs_columns, check[i,fcs_columns], "replacing fcs columns with NA because all fcs are 7", ignore_case = F) %>% 
    filter(!is.na(old.value)) %>% 
    mutate(old.value = as.character(old.value))
  cl_fcs_all_7 <- bind_rows(cl_fcs_all_7,cl) 
}

cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_fcs_all_7)

### All LCSI NA
lcsi_columns <- names(raw.flag)[which(str_starts(names(raw.flag),"lcsi_"))]
check <-  raw.flag %>% filter(flag_lcsi_na == 1)
cl_lcsi_all_na <- data.frame()
for (i in 1:nrow(check)) {
  cl <- recode.set.NA.if(check[i,], lcsi_columns, check[i,lcsi_columns], "replacing lcsi columns with NA because all lcsi are na", ignore_case = F) %>% 
    filter(!is.na(old.value)) %>% 
    mutate(old.value = as.character(old.value))
  cl_lcsi_all_na <- bind_rows(cl_lcsi_all_na,cl)
}

cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_lcsi_all_na)

### LCSI Displaced but HH not displaced
if("flag_lcsi_displ" %in% names(raw.flag)){
  displ <- lcs_variables[which(grepl("displaced|migration|migrated",get.label(lcs_variables)))]
  check <-  raw.flag %>% filter(flag_lcsi_displ == 1)
  cl_lcsi_displ <- data.frame()
  for (i in 1:nrow(check)) {
    cl <- recode.set.NA.if(check[i,], displ, check[i,displ], "replacing lcsi displacement strategy columns with NA because HH not IDP", ignore_case = F) %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value))
    cl_lcsi_displ <- bind_rows(cl_lcsi_displ,cl)
  }
  
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_lcsi_displ)
}

### LCSI Agriculture but HH not displaced
if("flag_lcsi_liv_agriculture" %in% names(raw.flag)){
  agric <- lcs_variables[which(grepl("agriculture|crop|crops|farm",get.label(lcs_variables)))]
  check <-  raw.flag %>% filter(flag_lcsi_liv_agriculture == 1)
  cl_lcsi_agric <- data.frame()
  for (i in 1:nrow(check)) {
    cl <- recode.set.NA.if(check[i,], agric, check[i,agric], "replacing lcsi agricultural strategy columns with NA because HH do not have income from agriculture", ignore_case = F) %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value))
    cl_lcsi_agric <- bind_rows(cl_lcsi_agric,cl)
  }
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_lcsi_agric)
}

### LCSI Livestock but HH not displaced
if("flag_lcsi_liv_livestock" %in% names(raw.flag)){
  livest <- lcs_variables[which(grepl("livestock|livestocks|animal",get.label(lcs_variables)))]
  check <-  raw.flag %>% filter(flag_lcsi_liv_livestock == 1)
  cl_lcsi_livest <- data.frame()
  for (i in 1:nrow(check)) {
    cl <- recode.set.NA.if(check[i,], livest, check[i,livest], "replacing lcsi livestock strategy columns with NA because HH do not have income from livestock", ignore_case = F) %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value))
    cl_lcsi_livest <- bind_rows(cl_lcsi_livest,cl)
  }
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_lcsi_livest)
}




# 4B) FLAG Logical Checks

checks_followups <- tibble()

## FSL
# Check number 1
check_protein_rcsi <- raw.flag %>% 
  select(uuid,enum_colname, flag_protein_rcsi)%>% 
  filter(flag_protein_rcsi == 1) %>% 
  left_join(raw.main %>% select(uuid, fcs_score, fcs_meat, fcs_dairy))

checks_followups <- rbind(checks_followups,
                          make.logical.check.entry(check_protein_rcsi, 1,  c("fcs_score", "fcs_meat","fcs_dairy"), 
                                                   cols_to_keep = c(enum_colname),"rCSI Score is high while protein consumption is also reported as frequent", F))

# Check number 2
check_lcsi_coherence <- raw.flag %>% 
  select(uuid,enum_colname, flag_lcsi_coherence)%>% 
  filter(flag_lcsi_coherence == 1)%>% 
  left_join(raw.main %>% select(uuid, lcsi_emergency, lcsi_stress, lcsi_crisis))

checks_followups <- rbind(checks_followups,
                          make.logical.check.entry(check_lcsi_coherence, 2,  c("lcsi_emergency", "lcsi_stress","lcsi_crisis"), 
                                                   cols_to_keep = c(enum_colname),"HHs report using crisi or emergency strategies but not stress strategies or Emergency and no crisis.", F))


#Check number 3
fcs_flag_columns <- c("fcs_cereal","fcs_legumes","fcs_dairy","fcs_meat","fcs_veg",
                      "fcs_fruit","fcs_oil","fcs_sugar","fcs_score")
rcsi_flag_columns <- c("rcsi_lessquality","rcsi_borrow",
                       "rcsi_mealsize","rcsi_mealadult","rcsi_mealnb","rcsi_score")
check_fcsrcsi_box <- raw.flag %>% 
  select(uuid,enum_colname, flag_fcsrcsi_box)%>% 
  filter(flag_fcsrcsi_box == 1)%>% 
  left_join(raw.main %>% select(uuid, fcs_flag_columns, rcsi_flag_columns))

checks_followups <- rbind(checks_followups,
                          make.logical.check.entry(check_fcsrcsi_box, 3,  c(fcs_flag_columns, rcsi_flag_columns), 
                                                   cols_to_keep = c(enum_colname),"HH that would have an acceptable FCS score and a high rCSI score", F))


####
create.follow.up.requests(checks_followups, paste0(make.short.name("follow-up_requests"),".xlsx"))
svDialogs::dlg_message("Translation for both others and translations are done and a file is created in the folder output/checking/requests/ with other_requests in the title. Please check the READ_ME file for information on filling the file.", type = "ok")
