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

raw.flag <- raw.main %>% 
  dplyr::mutate_at(vars(fcs_cereal,fcs_legumes,fcs_dairy,fcs_meat,fcs_veg,fcs_fruit,fcs_oil,fcs_sugar),as.numeric) %>%
  dplyr::mutate(fcs_weight_cereal1 = ifelse(is.na(fcs_cereal), NA, fcs_cereal * 2),
                fcs_weight_legume2 = ifelse(is.na(fcs_legumes), NA, fcs_legumes * 3),
                fcs_weight_dairy3 = ifelse(is.na(fcs_dairy), NA, fcs_dairy * 4),
                fcs_weight_meat4 = ifelse(is.na(fcs_meat), NA, fcs_meat * 4),
                fcs_weight_veg5 = ifelse(is.na(fcs_veg), NA, fcs_veg * 1),
                fcs_weight_fruit6 = ifelse(is.na(fcs_fruit), NA, fcs_fruit * 1),
                fcs_weight_oil7 = ifelse(is.na(fcs_oil), NA, fcs_oil * 0.5),
                fcs_weight_sugar8 = ifelse(is.na(fcs_sugar), NA, fcs_sugar * 0.5)) %>% 
  dplyr::mutate(fcs_score = fcs_weight_cereal1 + fcs_weight_legume2 + fcs_weight_dairy3 + 
                  fcs_weight_meat4 + fcs_weight_veg5 + fcs_weight_fruit6 + fcs_weight_oil7 + fcs_weight_sugar8,
                fcs_cat = dplyr::case_when(is.na(fcs_score) ~ NA_character_,
                                           fcs_score < 21.5 ~ "Poor",
                                           fcs_score <= 35 ~ "Borderline",
                                           fcs_score > 35 & fcs_score < 200 ~ "Acceptable",
                                           TRUE ~ NA_character_)) %>%
  dplyr::mutate()
  dplyr::mutate_at(vars(rcsi_lessquality,rcsi_borrow,rcsi_mealsize,rcsi_mealadult,rcsi_mealnb),as.numeric) %>% 
  addindicators::add_rcsi(rCSILessQlty = "rcsi_lessquality",
                          rCSIBorrow = "rcsi_borrow",
                          rCSIMealSize = "rcsi_mealsize",
                          rCSIMealAdult = "rcsi_mealadult",
                          rCSIMealNb = "rcsi_mealnb") %>% 
  mutate_at(vars(hhs_lack_food,hhs_sleep_nofood,hhs_wholedaynight_nofood),~ifelse(. %in% c('dk','dwta'),'no',.)) %>% 
  addindicators::add_hhs(hhs_nofoodhh_1 = "hhs_lack_food",
                         hhs_nofoodhh_1a = "hhs_lack_food_freq",
                         hhs_sleephungry_2 = "hhs_sleep_nofood",
                         hhs_sleephungry_2a = "hhs_sleep_nofood_freq",
                         hhs_alldaynight_3 = "hhs_wholedaynight_nofood",
                         hhs_alldaynight_3a = "hhs_wholedaynight_nofood_freq",
                         yes_answer = "yes", no_answer = "no", rarely_answer = "rarely", 
                         sometimes_answer = "sometimes", often_answer = "often") %>%
  addindicators::add_lcsi(lcsi_stress_vars =  c("lcsi_stress1","lcsi_stress2","lcsi_stress3","lcsi_stress4"),
                          lcsi_crisis_vars = c("lcsi_crisis1","lcsi_crisis2","lcsi_crisis3"),
                          lcsi_emergency_vars = c("lcsi_emergency1","lcsi_emergency2","lcsi_emergency3"), 
                          yes_val = "yes",
                          no_val = "no_had_no_need",
                          exhausted_val = "no_exhausted",
                          not_applicable_val = "not_applicable") %>%
  addindicators::add_fcm_phase(hhs_categories_little = "No or Little")

  healthyr::create_fsl_quality_report()













svDialogs::dlg_message("Translation for both others and translations are done and a file is created in the folder output/checking/requests/ with other_requests in the title. Please check the READ_ME file for information on filling the file.", type = "ok")
