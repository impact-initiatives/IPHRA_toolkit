## Function to check FS Flags

check_fs_flags <- function(.dataset,
                           date_dc_date = "today",
                           fcs_cereal = "fcs_cereal",
                           fcs_legumes = "fcs_legumes",
                           fcs_dairy = "fcs_dairy",
                           fcs_meat = "fcs_meat",
                           fcs_veg = "fcs_veg",
                           fcs_fruit = "fcs_fruit",
                           fcs_oil = "fcs_oil",
                           fcs_sugar = "fcs_sugar",
                           rcsi_lessquality = "rcsi_lessquality",
                           rcsi_borrow = "rcsi_borrow",
                           rcsi_mealsize = "rcsi_mealsize",
                           rcsi_mealadult = "rcsi_mealadult",
                           rcsi_mealnb = "rcsi_mealnb",
                           hhs_lack_food = "hhs_lack_food",
                           hhs_lack_food_freq = "hhs_lack_food_freq",
                           hhs_sleep_nofood = "hhs_sleep_nofood",
                           hhs_sleep_nofood_freq = "hhs_sleep_nofood_freq",
                           hhs_wholedaynight_nofood = "hhs_wholedaynight_nofood",
                           hhs_wholedaynight_nofood_freq = "hhs_wholedaynight_nofood_freq",
                           hdds_cereals = "hdds_cereals",
                           hdds_tubers = "hdds_tubers",
                           hdds_legumes = "hdds_legumes",
                           hdds_veg = "hdds_veg",
                           hdds_fruit = "hdds_fruit",
                           hdds_meat = "hdds_meat",
                           hdds_fish = "hdds_fish",
                           hdds_dairy = "hdds_dairy",
                           hdds_eggs = "hdds_eggs",
                           hdds_sugar = "hdds_sugar",
                           hdds_oil = "hdds_oil",
                           hdds_condiments = "hdds_condiments",
                           lcsi_stress1 = "lcsi_stress1",
                           lcsi_stress2 = "lcsi_stress2",
                           lcsi_stress3 = "lcsi_stress3",
                           lcsi_stress4 = "lcsi_stress4",
                           lcsi_crisis1 = "lcsi_crisis1",
                           lcsi_crisis2 = "lcsi_crisis2",
                           lcsi_crisis3 = "lcsi_crisis3",
                           lcsi_emergency1 = "lcsi_emergency1",
                           lcsi_emergency2 = "lcsi_emergency2",
                           lcsi_emergency3 = "lcsi_emergency3",
                           lcsi_stress = "lcsi_stress",
                           lcsi_crisis = "lcsi_crisis",
                           lcsi_emergency = "lcsi_emergency",
                           lcsi_cat_yes = "lcsi_cat_yes",
                           lcsi_cat_exhaust = "lcsi_cat_exhaust",
                           lcsi_cat = "lcsi_cat",
                           fcs_cat ="fcs_cat",
                           fcs_score = "fcs_score",
                           rcsi_cat = "rcsi_cat",
                           rcsi_score = "rcsi_score",
                           hhs_cat = "hhs_cat",
                           hhs_score = "hhs_score",
                           hdds_cat = "hdds_cat",
                           hdds_score = "hdds_score",
                           fc_cell = "fc_cell",
                           fc_phase = "fc_phase",
                           num_children = "num_children",
                           enumerator = "enumerator",
                           uuid = "uuid",
                           cluster = "cluster") {
  # change df into dataframe
  .dataset <- as.data.frame(.dataset)
  
  options(warn = -1)
  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  if(cluster %in% names(.dataset)){
    results <- .dataset %>% 
      dplyr::select(uuid, enumerator,date_dc_date, cluster) %>% 
      rename(date_dc_date = date_dc_date) %>% 
      mutate(date_dc_date = lubridate::as_date(as.numeric(date_dc_date),origin = "1899-12-30"))
  } else{
    results <- .dataset %>% 
      dplyr::select(uuid, enumerator,date_dc_date)%>% 
      rename(date_dc_date = date_dc_date) %>% 
      mutate(date_dc_date = lubridate::as_date(as.numeric(date_dc_date),origin = "1899-12-30"))
  }
  # combine all fcs_columns together
  fcs_flag_columns <- c(fcs_cereal,fcs_legumes,fcs_dairy,fcs_meat,fcs_veg,fcs_fruit,fcs_oil,fcs_sugar,fcs_score)
  
  if(all(fcs_flag_columns %in% colnames(.dataset))) {
    ## flag issues in data with FCS
    results2 <- .dataset %>%
      dplyr::mutate_at(vars(fcs_flag_columns),as.numeric)%>% 
      dplyr::mutate(flag_meat_cereal_ratio = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_cereal < fcs_meat, 1, 0)),
                    flag_low_cereal = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_cereal < 5, 1, 0)),
                    flag_low_oil = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_oil < 5, 1, 0)),
                    flag_low_fcs = ifelse(is.na(fcs_score), NA, ifelse(fcs_score <= 10, 1, 0)), #remove
                    flag_high_fcs = ifelse(is.na(fcs_score), NA, ifelse(fcs_score >= 56, 1, 0)), #remove
                    flag_protein_fcs = ifelse(is.na(fcs_score), NA, ifelse(fcs_score < 21 & (fcs_meat >= 5 | fcs_dairy >= 5), 1, 0))) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(sd_foods = sd(c(fcs_cereal, fcs_legumes, fcs_dairy, fcs_meat, fcs_veg, fcs_fruit, fcs_oil, fcs_sugar), na.rm = TRUE),
                    flag_sd_foodgroup = dplyr::case_when(sd_foods < 0.8 ~ 1, TRUE ~ 0)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(fcs_flag_columns,
                    fcs_cat,
                    flag_meat_cereal_ratio,
                    flag_low_cereal,
                    flag_low_oil,
                    flag_low_fcs,#remove
                    flag_high_fcs,#remove
                    flag_protein_fcs,
                    flag_sd_foodgroup)
    
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  } 
  ## flag issues in data with rCSI
  
  rcsi_flag_columns <- c(rcsi_lessquality,rcsi_borrow,rcsi_mealsize,rcsi_mealadult,rcsi_mealnb,rcsi_score)
  if(all(rcsi_flag_columns %in% names(.dataset)) & num_children %in% names(.dataset)){
    results2 <- .dataset %>% 
      dplyr::mutate_at(vars(rcsi_flag_columns),as.numeric)%>% 
      dplyr::mutate(flag_protein_rcsi = ifelse(is.na(rcsi_score), NA,
                                               ifelse(is.na(fcs_cereal), NA,
                                                      ifelse(rcsi_score >= 19 & ( fcs_dairy >= 5 | fcs_meat >= 5), 1, 0 ))),
                    flag_fcs_rcsi = ifelse(is.na(rcsi_score), NA,
                                           ifelse(is.na(fcs_score), NA,
                                                  ifelse(fcs_score < 35 & rcsi_score <= 4, 1, 0 ))),
                    flag_high_rcsi = ifelse(is.na(rcsi_score), NA, ifelse(rcsi_score >= 43, 1, 0)),
                    flag_rcsi_children = ifelse(is.na(rcsi_mealadult), NA, ifelse(!is.na(rcsi_mealadult) & as.numeric(num_children) == 0, 1,0)),
                    flag_fcsrcsi_box = dplyr::case_when(rcsi_score > 18 & fcs_score > 56 ~ 1,
                                                        TRUE ~ 0)) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(sd_rcsicoping = sd(c(rcsi_lessquality, rcsi_borrow, rcsi_mealsize, rcsi_mealadult, rcsi_mealnb), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(flag_sd_rcsicoping = dplyr::case_when(sd_rcsicoping < 0.8 & rcsi_score < 4 ~ 1, TRUE ~ 0)) %>% 
      dplyr::select(rcsi_flag_columns,rcsi_cat,flag_protein_rcsi,flag_fcs_rcsi,flag_high_rcsi,flag_rcsi_children,flag_fcsrcsi_box,flag_sd_rcsicoping)
    
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## flag issue in data with HHS
  hhs_flag_columns <- c(hhs_lack_food,hhs_lack_food_freq,hhs_sleep_nofood,hhs_sleep_nofood_freq,
                        hhs_wholedaynight_nofood,hhs_wholedaynight_nofood_freq,hhs_cat,hhs_score)
  if(all(hhs_flag_columns %in% names(.dataset))){
    results2 <- .dataset %>% 
      dplyr::mutate(flag_severe_hhs = ifelse(is.na(hhs_score), NA, ifelse(hhs_score >= 5, 1, 0))) %>% 
      dplyr::select(hhs_flag_columns,flag_severe_hhs)
    
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## flag issues with LCSI
  lcs_flag_columns <- c(lcsi_stress1,lcsi_stress2,lcsi_stress3,lcsi_stress4,lcsi_crisis1,lcsi_crisis2,
                        lcsi_crisis3,lcsi_emergency1,lcsi_emergency2,lcsi_emergency3,lcsi_stress,
                        lcsi_crisis,lcsi_emergency,lcsi_cat_yes,lcsi_cat_exhaust,lcsi_cat)
  
  if(all(lcs_flag_columns %in% names(.dataset))){
    results2 <- .dataset %>% 
      dplyr::mutate(flag_lcsi_coherence = ifelse(is.na(lcsi_emergency), NA,
                                                 ifelse(lcsi_emergency == 1 & lcsi_stress == 0 | 
                                                          lcsi_emergency == 1 & lcsi_crisis == 0 |
                                                          lcsi_crisis == 1 & lcsi_stress == 0, 1, 0)),
                    flag_lcsi_severity = dplyr::case_when(lcsi_emergency == 1 ~ 1,
                                                          TRUE ~ 0))
    
    lcs_variables <- c("lcsi_stress1","lcsi_stress2","lcsi_stress3","lcsi_stress4","lcsi_crisis1",
                       "lcsi_crisis2","lcsi_crisis3","lcsi_emergency1","lcsi_emergency2","lcsi_emergency3")
    results2$lcsi.count.na <-  apply(results2[c(lcs_variables)], 1, function(x) sum(x == "not_applicable"))
    
    results2 <- results2 %>% 
      dplyr::mutate(flag_lcsi_na = dplyr::case_when(lcsi.count.na == 10 ~ 1, TRUE ~ 0)) 
    
    income_types <- c("first_income_types","second_income_types","third_income_types")
    suppressWarnings(
      agric <- lcs_variables[which(grepl("agriculture|crop|crops|farm",get.label(lcs_variables)))]
    )
    
    suppressWarnings(
      livest <- lcs_variables[which(grepl("livestock|livestocks|animal",get.label(lcs_variables)))]
      
    )
    
    suppressWarnings(
      displ <- lcs_variables[which(grepl("displaced|migration|migrated",get.label(lcs_variables)))]
    )
    
    if(length(agric)>0){
      results2$flag_lcsi_liv_agriculture <- dplyr::case_when(rowSums(sapply(results2[agric], function(i) grepl("yes",i))) > 0 & any(results2[income_types] == "sell_agri_prod") > 0  ~ 1, TRUE ~ 0) ## Fix second part to take only select_one from three columns
    }
    
    if(length(livest)>0){
      results2$flag_lcsi_liv_livestock  <- dplyr::case_when(rowSums(sapply(results2[livest], function(i) grepl("yes",i))) > 0 & any(results2[income_types] == "sell_anim_prod") > 0 ~ 1, TRUE ~ 0) ## Fix second part to take only select_one from three columns
    }
    
    if(length(displ)>0){
      results2$flag_lcsi_displ  <- dplyr::case_when(rowSums(sapply(results2[displ], function(i) grepl("yes",i))) > 0 & results2["residency_status"] == "idp" ~ 1, TRUE ~ 0) ## Fix second part to take only select_one from three columns
    }
    
    if(length(livest)>0 & length(agric)>0 & length(displ)>0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_agriculture,flag_lcsi_liv_livestock,flag_lcsi_displ)
    } else if (length(livest)>0 & length(agric)>0 & length(displ) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_livestock,flag_lcsi_liv_agriculture)      
    } else if (length(agric)>0 & length(displ)>0 & length(livest) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_agriculture,flag_lcsi_displ)   
    } else if (length(displ)>0 & length(livest)>0 & length(agric) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_displ,flag_lcsi_liv_livestock)   
    } else if (length(livest)>0 & length(agric) ==0 & length(displ) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_livestock)      
    } else if (length(agric)>0 & length(livest) == 0 & length(displ) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_agriculture)   
    } else if (length(displ)>0 & length(livest) == 0 & length(agric) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_displ)   
    }  else {
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na)   
    }
  
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  fc_phase_col <- c(fc_cell,fc_phase)
  if(all(fc_phase_col %in% names(.dataset))){
    ## flag phase
    results2 <- .dataset %>% 
      dplyr::mutate(flag_fc_cell = ifelse(is.na(fc_cell), NA,
                                          ifelse(fc_cell %in% c(3,4,5,8,9,10), 1, 0))) %>% 
      select(fc_phase_col, flag_fc_cell)
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## flag hhds 
  hdds_flag_columns <- c(hdds_cereals,hdds_tubers,hdds_legumes,hdds_veg,hdds_fruit,
                         hdds_meat,hdds_fish,hdds_dairy,hdds_eggs,hdds_sugar,
                         hdds_oil,hdds_condiments,hdds_cat,hdds_score) 
  if(all(hdds_flag_columns %in% names(.dataset))) {
    results2 <- .dataset %>% 
      dplyr::mutate(flag_low_sugar_cond_hdds = ifelse(is.na(hdds_score), NA,
                                                      ifelse((hdds_score <= 2 & hdds_sugar == "yes" & hdds_condiments == "yes") | 
                                                               (hdds_score <= 1 & hdds_sugar == "yes") |
                                                               (hdds_score <= 1 & hdds_condiments == "yes"), 1, 0))) %>% 
      dplyr::select(hdds_flag_columns,flag_low_sugar_cond_hdds)
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  options(warn = 0)
  return(results)
}

## Function to check WATER CONSUMPTION Flags

check_WASH_flags <- function(.dataset,
                             data_container_loop,
                             date_dc_date = "today",
                             containers = "containers",
                             container_type = "container_type",
                             container_litre_other = "container_litre_other",
                             container_journey_collection = "container_journey_collection",
                             num_containers = "num_containers",
                             water_source = "water_source",
                             water_collect_time = "water_collect_time",
                             num_hh = "num_hh",
                             enumerator = "enumerator",
                             uuid = "uuid") {
  # change df into dataframe
  .dataset <- as.data.frame(.dataset)
  # change df into dataframe
  data_container_loop <- as.data.frame(data_container_loop)
  
  options(warn = -1)
  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  ## Throw an error if the dataset is empty
  if (nrow(data_container_loop) == 0) {
    stop("raw.water_count_loop is empty")
  }
  if(date_dc_date == "start"){
    results <- .dataset %>% 
      dplyr::select(uuid, enumerator,date_dc_date)%>% 
      rename(date_dc_date = date_dc_date) %>% 
      mutate(date_dc_date = lubridate::as_date(date_dc_date))
  } else {
    results <- .dataset %>% 
      dplyr::select(uuid, enumerator,date_dc_date)%>% 
      rename(date_dc_date = date_dc_date) %>% 
      mutate(date_dc_date = lubridate::as_date(as.numeric(date_dc_date),origin = "1899-12-30"))
  }
  
  ## calculate liters per person per day
  calculate_data_container_loop <- data_container_loop %>% 
    dplyr::rowwise() %>% 
    mutate(container_type_litre = str_remove(str_extract(!!rlang::sym(container_type), "([^\\__]+$)"), "l"),
           litre = ifelse(!!rlang::sym(container_type) == "other", as.numeric(!!rlang::sym(container_litre_other)),as.numeric(container_type_litre)),
           litre_per_day = ifelse(is.na(!!rlang::sym(container_journey_collection)), litre, litre * as.numeric(!!rlang::sym(container_journey_collection)))) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(uuid) %>% 
    dplyr::summarise(litre_per_day_per_hh = sum(litre_per_day, na.rm = T))
  
  results2 <- .dataset %>% 
    dplyr::left_join(calculate_data_container_loop) %>% 
    dplyr::mutate(litre_per_day_per_person = litre_per_day_per_hh / as.numeric(!!rlang::sym(num_hh)))
    
  ## FLAGS (Litres per person per day)
  mean_litre_dataset <-  mean(results2$litre_per_day_per_person, na.rm = T)
  sd_litre_dataset <- stats::sd(results2$litre_per_day_per_person, na.rm = T)
  
  results2 <- results2 %>% 
    dplyr::mutate(litre_z_score = (litre_per_day_per_person - mean_litre_dataset) / sd_litre_dataset)
  
  mean_litre_zscore <- mean(results2$litre_z_score, na.rm = T)
  
  results2 <- results2 %>% 
    dplyr::mutate(flag_sd_litre = ifelse(is.na(litre_z_score),NA,
                                         ifelse(litre_z_score < mean_litre_zscore-3 | litre_z_score > mean_litre_zscore+3, 1, 0)),
                  flag_low_litre = ifelse(is.na(litre_per_day_per_person), 0,
                                          ifelse(litre_per_day_per_person <= 1, 1, 0)),
                  flag_high_litre = ifelse(is.na(litre_per_day_per_person),0,
                                           ifelse(litre_per_day_per_person >=50, 1, 0)),
                  flag_high_container = ifelse(is.na(!!rlang::sym(num_containers)), 0 ,
                                                     ifelse(as.numeric(!!rlang::sym(num_containers)) > 20, 1, 0)),
                  flag_no_container = case_when(!!rlang::sym(water_source) %in% c("piped_neighbour","tap","borehole","protected_well",
                                                                                  "unprotected_well","well_spring","unprotected_spring","rainwater_collection",
                                                                                  "tank_truck","cart_tank","kiosk",
                                                                                  "bottled_water","sachet_water","surface_water") & is.na(!!rlang::sym(num_containers)) ~ 1,
                                                TRUE ~0),
                  flag_not_immediate = case_when(!!rlang::sym(water_source) %in% c("piped_dwelling",
                                                                                   "piped_compound",
                                                                                   "rainwater_collection") & !!rlang::sym(water_collect_time) != "inside_compound" ~ 1,
                                                 TRUE ~ 0)) %>% 
    dplyr::select(water_source,num_containers,litre_per_day_per_person,
                  litre_z_score,water_collect_time,flag_sd_litre,flag_low_litre,
                  flag_high_litre,flag_high_container,flag_no_container,flag_not_immediate)
  
  if(!exists("results")){
    results <- results2
  } else {
    results <- cbind(results,results2)
  }
  options(warn = 0)
  return(results)
}

## Function to check Nutrition/Muac
check_nut_flags <- function(.dataset,
                            muac_cm = "muac_cm",
                            edema_confirm = "edema_confirm",
                            child_age_months = "child_age_months",
                            child_sex = "child_sex",
                            uuid = "uuid",
                            loop_index = "loop_index") {
  # change df into dataframe
  .dataset <- as.data.frame(.dataset)
  
  options(warn = -1)
  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  
  results <- .dataset %>% 
    dplyr::select(uuid,loop_index)

  results2 <- .dataset %>% 
    dplyr::mutate(muac_mm = ifelse(is.na(!!rlang::sym(muac_cm)), NA, as.numeric(!!rlang::sym(muac_cm)) * 10),
                  muac_cm = as.numeric(!!rlang::sym(muac_cm)),
                  sex = ifelse(!!rlang::sym(child_sex) == "m",1,2),
                  child_age_months = as.numeric(!!rlang::sym(child_age_months)),
                  age_days = as.numeric(!!rlang::sym(child_age_months))* 30.25)
  
  ## calculate MUAC-for-age z-scores
  results2 <- zscorer::addWGSR(data = results2,
                               sex = "sex",
                               firstPart = "muac_cm",
                               secondPart = "age_days",
                               index = "mfa")
  
  mean_mfaz_dataset <- mean(results2$mfaz, na.rm=T)

  
  results2 <- results2 %>%
    dplyr::mutate(flag_sd_mfaz = ifelse(is.na(mfaz),NA,
                                         ifelse(mfaz < mean_mfaz_dataset - 3 | mfaz > mean_mfaz_dataset + 3, 1, 0)),
                  flag_extreme_muac = ifelse(is.na(muac_cm), NA,
                                             ifelse(muac_cm < 7 | muac_cm > 22, 1, 0))) %>% 
    dplyr::select(mfaz,muac_cm,muac_mm,flag_sd_mfaz,flag_extreme_muac)

  
  if(!exists("results")){
    results <- results2
  } else {
    results <- cbind(results,results2)
  }
  options(warn = 0)
  return(results)
}

create_fsl_quality_report_test <- function (df, grouping = NULL, short_report = NULL, file_path = NULL) {
  options(warn = -1)
  if (is.null(short_report)) {
    short_report <- FALSE
  }
  if (!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }
  if (c("fcs_score") %in% colnames(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(mean_fcs = round(mean(fcs_score, na.rm = TRUE), 2),
                       sd_fcs = round(stats::sd(fcs_score, na.rm = TRUE), 2),
                       mean_days_cereals = round(mean(fcs_cereal, na.rm = TRUE), 2),
                       sd_days_cereals = round(stats::sd(fcs_cereal, na.rm = TRUE), 2),
                       mean_days_legumes = round(mean(fcs_legumes, na.rm = TRUE), 2),
                       sd_days_legumes = round(stats::sd(fcs_legumes, na.rm = TRUE), 2),
                       mean_days_dairy = round(mean(fcs_dairy, na.rm = TRUE), 2),
                       sd_days_dairy = round(stats::sd(fcs_dairy, na.rm = TRUE), 2),
                       mean_days_meat = round(mean(fcs_meat, na.rm = TRUE), 2),
                       sd_days_meat = round(stats::sd(fcs_meat, na.rm = TRUE), 2),
                       mean_days_veg = round(mean(fcs_veg, na.rm = TRUE), 2), 
                       sd_days_veg = round(stats::sd(fcs_veg, na.rm = TRUE), 2), 
                       mean_days_fruit = round(mean(fcs_fruit, na.rm = TRUE), 2),
                       sd_days_fruit = round(stats::sd(fcs_fruit, na.rm = TRUE), 2),
                       mean_days_oils = round(mean(fcs_oil, na.rm = TRUE), 2),
                       sd_days_oils = round(stats::sd(fcs_oil, na.rm = TRUE), 2),
                       mean_days_sugar = round(mean(fcs_sugar, na.rm = TRUE), 2),
                       sd_days_sugar = round(stats::sd(fcs_sugar, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (c("rcsi_score") %in% colnames(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(mean_rcsi = round(mean(rcsi_score, na.rm = TRUE), 2), 
                       sd_rcsi = round(stats::sd(rcsi_score, na.rm = TRUE), 2),
                       mean_rcsi_lessquality = round(mean(rcsi_lessquality, na.rm = TRUE), 2), 
                       sd_rcsi_lessquality = round(stats::sd(rcsi_lessquality, na.rm = TRUE), 2),
                       mean_rcsi_borrow = round(mean(rcsi_borrow, na.rm = TRUE), 2),
                       sd_rcsi_borrow = round(stats::sd(rcsi_borrow, na.rm = TRUE), 2),
                       mean_rcsi_mealsize = round(mean(rcsi_mealsize, na.rm = TRUE), 2),
                       sd_rcsi_mealsize = round(stats::sd(rcsi_mealsize, na.rm = TRUE), 2),
                       mean_rcsi_mealadult = round(mean(rcsi_mealadult, na.rm = TRUE), 2), 
                       sd_rcsi_mealadult = round(stats::sd(rcsi_mealadult, na.rm = TRUE), 2),
                       mean_rcsi_mealnb = round(mean(rcsi_mealnb, na.rm = TRUE), 2),
                       sd_rcsi_mealnb = round(stats::sd(rcsi_mealnb, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (c("hhs_score") %in% colnames(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(mean_hhs = round(mean(hhs_score, na.rm = TRUE), 2),
                       sd_hhs = round(stats::sd(hhs_score, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (c("hdds_score") %in% colnames(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(mean_hdds = round(mean(hdds_score, na.rm = TRUE), 2), 
                       sd_hdds = round(stats::sd(hdds_score, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    } else {
      results <- merge(results, results2)
    }
  }
  if (length(setdiff(c("fcs_score", "rcsi_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_rcsi = round(as.numeric(stats::cor.test(fcs_score, rcsi_score)[4]), 2),
                           corr.fcs_rcsi.pvalue = as.numeric(stats::cor.test(fcs_score, rcsi_score)[3]))
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_rcsi = NA,
                           corr.fcs_rcsi.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fcs_score", "hhs_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_hhs = round(as.numeric(stats::cor.test(fcs_score, hhs_score)[4]), 2), 
                           corr.fcs_hhs.pvalue = round(as.numeric(stats::cor.test(fcs_score, hhs_score)[3]), 6))
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_hhs = NA,
                           corr.fcs_hhs.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fcs_score", "hdds_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_hdds = round(as.numeric(stats::cor.test(fcs_score, hdds_score)[4]), 2),
                           corr.fcs_hdds.pvalue = round(as.numeric(stats::cor.test(fcs_score, hdds_score)[3]), 3))
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_hdds = NA,
                           corr.fcs_hdds.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("hdds_score", "rcsi_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.hdds_rcsi = round(as.numeric(stats::cor.test(hdds_score, rcsi_score)[4]), 2), 
                           corr.hdds_rcsi.pvalue = round(as.numeric(stats::cor.test(hdds_score, rcsi_score)[3]), 3))
        if (!exists("results")) {
          results <- results2
        } else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.hdds_rcsi = NA,
                           corr.hdds_rcsi.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("hhs_score", "rcsi_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.hhs_rcsi = round(as.numeric(stats::cor.test(hhs_score, rcsi_score)[4]), 2),
                           corr.hhs_rcsi.pvalue = round(as.numeric(stats::cor.test(hhs_score, rcsi_score)[3]), 3))
        if (!exists("results")) {
          results <- results2
        } else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.hhs_rcsi = NA,
                           corr.hhs_rcsi.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("hhs_cat", "cluster"), colnames(df))) == 0) {
    df <- df %>% dplyr::mutate(hhs_severe = ifelse(is.na(hhs_cat), NA,
                                                   ifelse(hhs_cat == "Very Severe" | hhs_cat == "Severe", 1, 0)))
    poisson_pvalues <- healthyr::calculate_poisson_pvalues(df, strata = grouping, cluster = "cluster", case = "hhs_severe")
    names(poisson_pvalues)[2] <- "poisson_pvalues.hhs_severe"
    if (!exists("results")) {
      results <- poisson_pvalues
    }else {
      results <- merge(results, poisson_pvalues, by = grouping)
    }
  }
  if (length(setdiff(c("fcs_score", "hhs_score", "hdds_score", 
                       "rcsi_score", "flag_lcsi_coherence"), names(df))) < 5) {
    nms <- df %>% dplyr::select(dplyr::starts_with("flag")) %>% 
      names()
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise_at(.vars = nms, ~round(mean(., na.rm = TRUE), 3) * 100)
    if (!exists("results")) {
      results <- results2
    } else {
      results <- merge(results, results2)
    }
  }
  results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
    dplyr::summarise(n = dplyr::n())
  if (!exists("results")) {
    results <- results2
  }else {
    results <- merge(results, results2)
  }
  
  results <- results %>% dplyr::select(c(1, n, dplyr::everything()))
  if (c("flag_fc_cell") %in% names(df)) {
    results2 <- df %>% dplyr::mutate(p1 = ifelse(is.na(fc_phase), NA,
                                                 ifelse(fc_phase == "Phase 1 FC", 1, 0)), 
                                     p2 = ifelse(is.na(fc_phase), NA, 
                                                 ifelse(fc_phase == "Phase 2 FC", 1, 0)),
                                     p3 = ifelse(is.na(fc_phase), NA,
                                                 ifelse(fc_phase == "Phase 3 FC", 1, 0)),
                                     p4 = ifelse(is.na(fc_phase), NA, 
                                                 ifelse(fc_phase == "Phase 4 FC", 1, 0)), 
                                     p5 = ifelse(is.na(fc_phase), NA, 
                                                 ifelse(fc_phase == "Phase 5 FC", 1, 0))) %>% 
      dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(prop_fc_flags = sum(flag_fc_cell, na.rm = TRUE)/sum(!is.na(fc_cell), na.rm = TRUE), 
                       fews_p1 = round(sum(p1, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p2 = round(sum(p2, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p3 = round(sum(p3, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p4 = round(sum(p4, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p5 = round(sum(p5, na.rm = TRUE)/sum(!is.na(fc_cell)), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
    results <- results %>% 
      dplyr::select(c(1, n, dplyr::everything()))
  }
  if (c("food_exp_share") %in% names(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(prop_fc_flags = sum(flag_fc_cell, na.rm = TRUE)/sum(!is.na(fc_cell), na.rm = TRUE), 
                       fes_1 = round(sum(food_exp_share == "1", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2),
                       fes_2 = round(sum(food_exp_share == "2", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2),
                       fes_3 = round(sum(food_exp_share == "3", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2),
                       fes_4 = round(sum(food_exp_share == "4", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
    results <- results %>% dplyr::select(c(1, n, dplyr::everything()))
  }
  results <- calculate_plausibility_report_test(df = results)
  a <- c("n", "fews_p1", "fews_p2", "fews_p3", "fews_p4", 
         "fews_p5", "flag_severe_hhs", "flag_lcsi_severity", 
         "plaus_fcs", "plaus_rcsi", "plaus_hhs", "plaus_lcsi", 
         "plaus_other_fsl", "plaus_fsl_score", "plaus_fsl_cat")
  b <- intersect(a, colnames(results))
  if (short_report == TRUE & length(setdiff(b, colnames(results))) == 
      0) {
    results <- results %>% dplyr::select(1, b)
  }
  if (!is.null(file_path)) {
    writexl::write_xlsx(results, file_path)
  }
  options(warn = 0)
  return(results)
}
run_fsl_monitoring_dashboard_test <- function (df, grouping_var = NULL, filter_var1 = NULL, filter_var2 = NULL, enum_colname = "enumerator") 
{
  filtering_var1 <- filter_var1
  filtering_var2 <- filter_var2
  grouping_var <- grouping_var
  filter_var1 <- df %>% dplyr::select(filtering_var1) %>% 
    t %>% c %>% unique()
  filter_var2 <- df %>% dplyr::select(filtering_var2) %>% 
    t %>% c %>% unique()
  enum_list <- df %>% dplyr::select(enum_colname) %>% t %>% c %>% 
    unique()
  grouping_list <- df %>% dplyr::select(grouping_var) %>% 
    t %>% c %>% unique()
  min_date <- min(df$date_dc_date)
  max_date <- max(df$date_dc_date)
  shiny::shinyApp(ui = shiny::fluidPage(theme = shinythemes::shinytheme("sandstone"), 
                                        shiny::titlePanel("Monitoring Dashboard"), shiny::fluidRow(shiny::column(2, 
                                                                                                                 "Controls", shinyWidgets::pickerInput("filter_var1", 
                                                                                                                                                       label = paste0("Filter 1: ", filtering_var1), 
                                                                                                                                                       choices = filter_var1, selected = filter_var1[1], 
                                                                                                                                                       multiple = TRUE), shinyWidgets::pickerInput("filter_var2", 
                                                                                                                                                                                                   label = paste0("Filter 2: ", filtering_var2), 
                                                                                                                                                                                                   choices = filter_var2, selected = filter_var2[1], 
                                                                                                                                                                                                   multiple = TRUE), shinyWidgets::pickerInput("grouping_var1", 
                                                                                                                                                                                                                                               label = paste0("Grouping Variable: ", grouping_var), 
                                                                                                                                                                                                                                               choices = grouping_list, selected = grouping_list[1], 
                                                                                                                                                                                                                                               multiple = TRUE), shiny::sliderInput("date_range1", 
                                                                                                                                                                                                                                                                                    label = "Date range to analyze:", min = min_date, 
                                                                                                                                                                                                                                                                                    max = max_date, value = c(min_date, max_date)), 
                                                                                                                 shiny::conditionalPanel(condition = "input.tabSwitch == '3'", 
                                                                                                                                         shinyWidgets::pickerInput("time_plot_var", label = "Select Variable over Time", 
                                                                                                                                                                   choices = c("filler1", "filler2"))), shiny::actionButton("desButton1", 
                                                                                                                                                                                                                            "Apply", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
                                        ), shiny::column(10, "Output", shiny::tabsetPanel(id = "tabSwitch", 
                                                                                          tabPanel("Load Data", value = 1, shiny::fluidRow(column(11, 
                                                                                                                                                  DT::dataTableOutput(outputId = "main_data")))), 
                                                                                          shiny::tabPanel("Short Report", value = 2, shiny::fluidRow(column(11, 
                                                                                                                                                            DT::dataTableOutput("quality_table1")))), shiny::tabPanel("Full Report", 
                                                                                                                                                                                                                      value = 2, shiny::fluidRow(column(11, DT::dataTableOutput("quality_table2")))), 
                                                                                          shiny::tabPanel("Flag Table", value = 2, shiny::fluidRow(column(11, 
                                                                                                                                                          DT::dataTableOutput("flag_table")))), shiny::tabPanel("Correlogram", 
                                                                                                                                                                                                                value = 4, shiny::fluidRow(shiny::column(11, 
                                                                                                                                                                                                                                                         shiny::plotOutput("test_plot5")))), shiny::tabPanel("FCS", 
                                                                                                                                                                                                                                                                                                             value = 5, shiny::fluidRow(shiny::column(11, 
                                                                                                                                                                                                                                                                                                                                                      shiny::plotOutput("test_plot6")))), shiny::tabPanel("rCSI", 
                                                                                                                                                                                                                                                                                                                                                                                                          value = 5, shiny::fluidRow(shiny::column(11, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                   shiny::plotOutput("test_plot7")))), shiny::tabPanel("FCS by Group", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       value = 5, shiny::fluidRow(shiny::column(11, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                shiny::plotOutput("test_plot8")))), shiny::tabPanel("rCSI by Group", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    value = 5, shiny::fluidRow(shiny::column(11, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             shiny::plotOutput("test_plot9")))), ), )), 
  ), server = function(input, output, grp = grouping_var) {
    reactive_db <- shiny::eventReactive(input$desButton1, 
                                        {
                                          df %>% dplyr::filter(!!rlang::sym(grouping_var) %in% 
                                                                 input$grouping_var1) %>% dplyr::filter(date_dc_date >= 
                                                                                                          input$date_range1[1] & date_dc_date <= input$date_range1[2])
                                        })
    output$quality_table1 <- DT::renderDT({
      DT::datatable(healthyr::create_fsl_quality_report(df = reactive_db(), 
                                                        grouping = grouping_var, short_report = TRUE), 
                    filter = "top", extension = "FixedColumns", 
                    options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2, 
                                                                       rightColumns = 0)))
    })
    output$quality_table2 <- DT::renderDT({
      DT::datatable(healthyr::create_fsl_quality_report(df = reactive_db(), 
                                                        grouping = grouping_var, short_report = FALSE), 
                    filter = "top", extension = "FixedColumns", 
                    options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2, 
                                                                       rightColumns = 0)))
    })
    output$main_data <- DT::renderDT({
      DT::datatable(reactive_db(), filter = "top", extension = "FixedColumns", 
                    options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2, 
                                                                       rightColumns = 0)))
    })
    output$flag_table <- DT::renderDT({
      DT::datatable(healthyr::flag_summary_table(df = reactive_db(), 
                                                 grouping = grouping_var), filter = "top", extension = "FixedColumns", 
                    options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2, 
                                                                       rightColumns = 0)))
    })
    output$test_plot5 <- renderPlot({
      cols <- intersect(c("fcs_score", "hhs_score", "rcsi_score", 
                          "hdds_score"), colnames(reactive_db()))
      healthyr::plot_correlogram(df = reactive_db(), numeric_cols = cols)
    })
    output$test_plot6 <- renderPlot({
      cols <- intersect(c("fcs_cereal", "fcs_legumes", 
                          "fcs_dairy", "fcs_meat", "fcs_veg", "fcs_fruit", 
                          "fcs_oil", "fcs_sugar"), colnames(reactive_db()))
      healthyr::plot_ridge_distribution(df = reactive_db(), 
                                        numeric_cols = cols)
    })
    output$test_plot7 <- renderPlot({
      cols <- intersect(c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", 
                          "rcsi_limitportion_3", "rcsi_restrict_4", "rcsi_reducemeals5"), 
                        colnames(reactive_db()))
      healthyr::plot_ridge_distribution(df = reactive_db(), 
                                        numeric_cols = cols)
    })
    output$test_plot8 <- renderPlot({
      cols <- intersect(c("fcs_cereal", "fcs_legumes", 
                          "fcs_dairy", "fcs_meat", "fcs_veg", "fcs_fruit", 
                          "fcs_oil", "fcs_sugar"), colnames(reactive_db()))
      healthyr::plot_ridge_distribution(df = reactive_db(), 
                                        numeric_cols = cols, grouping = grouping_var)
    })
    output$test_plot9 <- renderPlot({
      cols <- intersect(c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", 
                          "rcsi_limitportion_3", "rcsi_restrict_4", "rcsi_reducemeals5"), 
                        colnames(reactive_db()))
      healthyr::plot_ridge_distribution(df = reactive_db(), 
                                        numeric_cols = cols, grouping = grouping_var)
    })
  })
}


calculate_plausibility_report_test <- function (df) {
  print("Now Calculating Plausibility Scoring and Classifications.")
  anthro_plaus_vars <- c("prop_smart_flags", "sd_wfhz_noflag", 
                         "age_ratio.pvalue", "sex_ratio.pvalue", "dps_weight", 
                         "dps_height", "dps_muac", "skewness_wfhz", "kurtosis_wfhz", 
                         "poisson_pvalues.wfhz")
  if (c("prop_smart_flags") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_smart_flags = ifelse(.data$prop_smart_flags <= 2.5, 0,
                                                          ifelse(.data$prop_smart_flags <= 5, 5,
                                                                 ifelse(.data$prop_smart_flags <= 7.5, 10,
                                                                        ifelse(.data$prop_smart_flags > 7.5, 20, NA)))))
  }
  if (c("sd_wfhz_noflag") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_sd_whz = ifelse(.data$sd_wfhz_noflag < 1.1 & .data$sd_wfhz_noflag > 0.9, 0,
                                                     ifelse(.data$sd_wfhz_noflag < 1.15 & .data$sd_wfhz_noflag > 0.85, 5,
                                                            ifelse(.data$sd_wfhz_noflag < 1.2 & .data$sd_wfhz_noflag > 0.8, 10,
                                                                   ifelse(.data$sd_wfhz_noflag >= 1.2 | .data$sd_wfhz_noflag <= 0.8, 20, NA)))))
  }
  if (c("age_ratio.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_ageratio = ifelse(.data$age_ratio.pvalue > 0.1, 0, 
                                                       ifelse(.data$age_ratio.pvalue > 0.05, 2, 
                                                              ifelse(.data$age_ratio.pvalue > 0.001, 4,
                                                                     ifelse(.data$age_ratio.pvalue <= 0.001, 10, NA)))))
  }
  if ((c("sex_ratio.pvalue") %in% names(df)) & !(c("cdr") %in% 
                                                 names(df))) {
    df <- df %>% dplyr::mutate(plaus_sexratio = ifelse(.data$sex_ratio.pvalue > 0.1, 0,
                                                       ifelse(.data$sex_ratio.pvalue > 0.05, 2, 
                                                              ifelse(.data$sex_ratio.pvalue >= 0.001, 4,
                                                                     ifelse(.data$sex_ratio.pvalue <= 0.001, 10, NA)))))
  }
  if (c("dps_weight") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_dps_weight = ifelse(.data$dps_weight >= 0 & .data$dps_weight < 8, 0,
                                                         ifelse(.data$dps_weight >= 8 & .data$dps_weight < 13, 2,
                                                                ifelse(.data$dps_weight >= 13 & .data$dps_weight < 20, 4,
                                                                       ifelse(.data$dps_weight >= 20, 10, NA)))))
  }
  if (c("dps_height") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_dps_height = ifelse(.data$dps_height >= 0 & .data$dps_height < 8, 0,
                                                         ifelse(.data$dps_height >= 8 & .data$dps_height < 13, 2,
                                                                ifelse(.data$dps_height >= 13 & .data$dps_height < 20, 4,
                                                                       ifelse(.data$dps_height >= 20, 10, NA)))))
  }
  if (c("dps_muac") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_dps_muac = ifelse(.data$dps_muac >= 0 & .data$dps_muac < 8, 0,
                                                       ifelse(.data$dps_muac >= 8 & .data$dps_muac < 13, 2,
                                                              ifelse(.data$dps_muac >= 13 & .data$dps_muac < 20, 4,
                                                                     ifelse(.data$dps_muac >= 20, 10, NA)))))
  }
  if (c("skewness_wfhz") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_skewness = ifelse(.data$skewness_wfhz < 0.2, 0,
                                                       ifelse(.data$skewness_wfhz < 0.4, 1,
                                                              ifelse(.data$skewness_wfhz < 0.6, 3,
                                                                     ifelse(.data$skewness_wfhz >= 0.6, 5, NA)))))
  }
  if (c("kurtosis_wfhz") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_kurtosis = ifelse(.data$kurtosis_wfhz < 0.2, 0,
                                                       ifelse(.data$kurtosis_wfhz < 0.4, 1,
                                                              ifelse(.data$kurtosis_wfhz < 0.6, 3,
                                                                     ifelse(.data$kurtosis_wfhz >= 0.6, 5, NA)))))
  }
  if (c("poisson_pvalues.wfhz") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_poisson = ifelse(.data$poisson_pvalues.wfhz > 0.05, 0,
                                                      ifelse(.data$poisson_pvalues.wfhz > 0.01, 1,
                                                             ifelse(.data$poisson_pvalues.wfhz > 0.001, 3, 
                                                                    ifelse(.data$poisson_pvalues.wfhz <= 0.001, 5, NA)))))
  }
  if (length(setdiff(anthro_plaus_vars, names(df))) == 0) {
    print("Generating anthropometric plausibility score and classification.")
    df <- df %>% dplyr::mutate(anthro_plaus_score = .data$plaus_smart_flags + .data$plaus_sd_whz +
                                 .data$plaus_ageratio + .data$plaus_sexratio + .data$plaus_dps_height +
                                 .data$plaus_dps_weight + .data$plaus_dps_muac + .data$plaus_skewness +
                                 .data$plaus_kurtosis + .data$plaus_poisson,
                               anthro_plaus_cat = ifelse(.data$anthro_plaus_score >= 0 & .data$anthro_plaus_score <= 9, "Excellent (0-9)", 
                                                         ifelse(.data$anthro_plaus_score > 9 & .data$anthro_plaus_score <= 14, "Good (10-14)",
                                                                ifelse(.data$anthro_plaus_score > 14 & .data$anthro_plaus_score < 25, "Acceptable (15-24)", 
                                                                       ifelse(.data$anthro_plaus_score >= 25, "Problematic (>=25)", NA)))))
  }
  else {
    print(paste0("Not all necessary variables for anthropometric plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(anthro_plaus_vars, names(df)))
  }
  mort_plaus_vars <- c("plaus_cdr", "plaus_prop_hh_flag_deaths", 
                       "plaus_sex_ratio.pvalue", "plaus_age_ratio_0_5.pvalue", 
                       "plaus_age_ratio_2_5.pvalue", "plaus_age_ratio_5_10.pvalue", 
                       "plaus_mean_hh_size.pvalue", "plaus_prop_joiners", "plaus_prop_leavers", 
                       "plaus_poisson_pvalues.deaths")
  if (c("cdr") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_cdr = ifelse(.data$cdr < 1, 0,
                                                  ifelse(.data$cdr < 2, 5,
                                                         ifelse(.data$cdr < 3.5, 10,
                                                                ifelse(.data$cdr >= 3.5, 20, 0)))))
  }
  if (c("prop_hh_flag_deaths") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_hh_flag_deaths = ifelse(.data$prop_hh_flag_deaths < 0.005, 0,
                                                                  ifelse(.data$prop_hh_flag_deaths < 0.01, 2,
                                                                         ifelse(.data$prop_hh_flag_deaths < 0.015, 5,
                                                                                ifelse(.data$prop_hh_flag_deaths >= 1.5, 10, 0)))))
  }
  if (length(setdiff(c("sex_ratio.pvalue", "cdr"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_sex_ratio.pvalue = ifelse(.data$sex_ratio.pvalue > 0.05, 0,
                                                               ifelse(.data$sex_ratio.pvalue > 0.001, 2,
                                                                      ifelse(.data$sex_ratio.pvalue > 1e-04, 5,
                                                                             ifelse(.data$sex_ratio.pvalue <= 1e-04, 10, 0)))))
  }
  if (c("age_ratio_0_5.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_age_ratio_0_5.pvalue = ifelse(.data$age_ratio_0_5.pvalue > 0.1, 0,
                                                                   ifelse(.data$age_ratio_0_5.pvalue > 0.05, 2,
                                                                          ifelse(.data$age_ratio_0_5.pvalue > 0.001, 5, 
                                                                                 ifelse(.data$age_ratio_0_5.pvalue <= 0.001, 10, 0)))))
  }
  if (c("age_ratio_2_5.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_age_ratio_2_5.pvalue = ifelse(.data$age_ratio_2_5.pvalue > 0.1, 0,
                                                                   ifelse(.data$age_ratio_2_5.pvalue > 0.05, 2,
                                                                          ifelse(.data$age_ratio_2_5.pvalue > 0.001, 5, 
                                                                                 ifelse(.data$age_ratio_2_5.pvalue <= 0.001, 10, 0)))))
  }
  if (c("age_ratio_5_10.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_age_ratio_5_10.pvalue = ifelse(.data$age_ratio_5_10.pvalue > 0.1, 0,
                                                                    ifelse(.data$age_ratio_5_10.pvalue > 0.05, 2,
                                                                           ifelse(.data$age_ratio_5_10.pvalue > 0.001, 5, 
                                                                                  ifelse(.data$age_ratio_5_10.pvalue <= 0.001, 10, 0)))))
  }
  if (c("mean_hh_size.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_mean_hh_size.pvalue = ifelse(.data$mean_hh_size.pvalue > 0.05, 0,
                                                                  ifelse(.data$mean_hh_size.pvalue > 0.001, 2,
                                                                         ifelse(.data$mean_hh_size.pvalue > 1e-04, 5,
                                                                                ifelse(.data$mean_hh_size.pvalue <= 1e-04, 10, 0)))))
  }
  if (c("prop_join_people") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_joiners = ifelse(.data$prop_join_people < 10, 0,
                                                           ifelse(.data$prop_join_people < 20, 2,
                                                                  ifelse(.data$prop_join_people < 30, 5,
                                                                         ifelse(.data$prop_join_people >= 30, 10, 0)))))
  }
  if (c("prop_left_people") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_leavers = ifelse(.data$prop_left_people < 10, 0,
                                                           ifelse(.data$prop_left_people < 20, 2,
                                                                  ifelse(.data$prop_left_people < 30, 5,
                                                                         ifelse(.data$prop_left_people >= 30, 10, 0)))))
  }
  if (length(setdiff(c("poisson_pvalues.deaths"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_poisson_pvalues.deaths = ifelse(.data$poisson_pvalues.deaths > 0.1, 0,
                                                                     ifelse(.data$poisson_pvalues.deaths > 0.05, 2,
                                                                            ifelse(.data$poisson_pvalues.deaths > 0.001, 5,
                                                                                   ifelse(.data$poisson_pvalues.deaths <= 0.001, 10, 0)))))
  }
  if (length(setdiff(mort_plaus_vars, names(df))) == 0) {
    df <- df %>% dplyr::mutate(mort_plaus_score = .data$plaus_cdr + .data$plaus_prop_hh_flag_deaths + .data$plaus_sex_ratio.pvalue + 
                                 .data$plaus_age_ratio_0_5.pvalue + .data$plaus_age_ratio_2_5.pvalue + .data$plaus_age_ratio_5_10.pvalue +
                                 .data$plaus_mean_hh_size.pvalue + .data$plaus_prop_joiners +
                                 .data$plaus_prop_leavers + .data$plaus_poisson_pvalues.deaths,
                               mort_plaus_cat = ifelse(.data$mort_plaus_score >= 0 & .data$mort_plaus_score < 10, "Excellent (0-<10)", 
                                                       ifelse(.data$mort_plaus_score >= 10 & .data$mort_plaus_score < 20, "Good (10-<20)",
                                                              ifelse(.data$mort_plaus_score >= 20 & .data$mort_plaus_score < 25, "Acceptable (20 - <25)", 
                                                                     ifelse(.data$mort_plaus_score >= 25, "Problematic (>=25)", NA)))))
  }
  else {
    print(paste0("Not all necessary variables for mortality plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(mort_plaus_vars, names(df)))
  }
  if (c("sd_fcs") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_sd_fcs = dplyr::case_when(.data$sd_fcs < 8 ~ 3,
                                                               .data$sd_fcs >= 8 & .data$sd_fcs < 9 ~ 2, 
                                                               .data$sd_fcs >= 9 & .data$sd_fcs < 14 ~ 0,
                                                               .data$sd_fcs >= 14 & .data$sd_fcs < 16 ~ 2,
                                                               .data$sd_fcs >= 16 ~ 3,
                                                               TRUE ~ 0))
  }
  if (c("flag_low_fcs") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_low_fcs = dplyr::case_when(.data$flag_low_fcs < 2 ~ 0,
                                                                     .data$flag_low_fcs >= 2 & .data$flag_low_fcs < 4 ~ 1.5,
                                                                     .data$flag_low_fcs >= 4 & .data$flag_low_fcs < 10 ~ 2.5,
                                                                     .data$flag_low_fcs >= 10 ~ 3.5,
                                                                     TRUE ~ 0))
  }
  if (c("flag_high_fcs") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_high_fcs = dplyr::case_when(.data$flag_high_fcs < 2 ~ 0,
                                                                      .data$flag_high_fcs >= 2 & .data$flag_high_fcs < 4 ~ 0.5,
                                                                      .data$flag_high_fcs >= 4 & .data$flag_high_fcs < 10 ~ 1,
                                                                      .data$flag_high_fcs >= 10 ~ 2,
                                                                      TRUE ~ 0))
  }
  if (c("flag_sd_foodgroup") %in% names(df)) { # maybe only 3 cat
    df <- df %>% dplyr::mutate(plaus_flag_sd_foodgroup = dplyr::case_when(.data$flag_sd_foodgroup < 2 ~ 0,
                                                                          .data$flag_sd_foodgroup >= 2 & .data$flag_sd_foodgroup < 4 ~ 2,
                                                                          .data$flag_sd_foodgroup >= 4 & .data$flag_sd_foodgroup < 10 ~ 4,
                                                                          .data$flag_sd_foodgroup >= 10 ~ 6, 
                                                                          TRUE ~ 0))
  }
  if (c("flag_meat_cereal_ratio") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_meat_cereal_ratio = dplyr::case_when(.data$flag_meat_cereal_ratio < 2 ~ 0,
                                                                               .data$flag_meat_cereal_ratio >= 2 & .data$flag_meat_cereal_ratio < 4 ~ 1.5,
                                                                               .data$flag_meat_cereal_ratio >= 4 & .data$flag_meat_cereal_ratio < 10 ~ 2.5,
                                                                               .data$flag_meat_cereal_ratio >= 10 ~ 3.5, 
                                                                               TRUE ~ 0))
  }
  if (c("flag_protein_fcs") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_protein_fcs = dplyr::case_when(.data$flag_protein_fcs < 2 ~ 0,
                                                                         .data$flag_protein_fcs >= 2 & .data$flag_protein_fcs < 4 ~ 0.5,
                                                                         .data$flag_protein_fcs >= 4 & .data$flag_protein_fcs < 10 ~ 1,
                                                                         .data$flag_protein_fcs >= 10 ~ 2,
                                                                         TRUE ~ 0))
  }
  fcs_plaus_vars <- c("plaus_sd_fcs", "plaus_flag_low_fcs", 
                      "plaus_flag_high_fcs", "plaus_flag_sd_foodgroup", "plaus_flag_meat_cereal_ratio", 
                      "plaus_flag_protein_fcs")
  if (length(setdiff(c(fcs_plaus_vars), names(df))) < 6) {
    plaus_nms <- intersect(fcs_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_fcs = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (c("flag_high_rcsi") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_high_rcsi = dplyr::case_when(.data$flag_high_rcsi < 2 ~ 0, 
                                                                       .data$flag_high_rcsi >= 2 & .data$flag_high_rcsi < 4 ~ 2, 
                                                                       .data$flag_high_rcsi >= 4 & .data$flag_high_rcsi < 10 ~ 3,
                                                                       .data$flag_high_rcsi >= 10 ~ 4, 
                                                                       TRUE ~ 0))
  }
  if (c("flag_sd_rcsicoping") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_sd_rcsicoping = dplyr::case_when(.data$flag_sd_rcsicoping < 2 ~ 0, 
                                                                           .data$flag_sd_rcsicoping >= 2 & .data$flag_sd_rcsicoping < 4 ~ 2, 
                                                                           .data$flag_sd_rcsicoping >= 4 & .data$flag_sd_rcsicoping < 10 ~ 4, 
                                                                           .data$flag_sd_rcsicoping >= 10 ~ 6,
                                                                           TRUE ~ 0))
  }
  if (c("sd_rcsi") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_sd_rcsi = dplyr::case_when(.data$sd_rcsi < 8 ~ 3,
                                                                .data$sd_rcsi >= 8 & .data$sd_rcsi < 9 ~ 2,
                                                                .data$sd_rcsi >= 9 & .data$sd_rcsi < 14 ~ 0, 
                                                                .data$sd_rcsi >= 14 & .data$sd_rcsi < 16 ~ 2,
                                                                .data$sd_rcsi >= 16 ~ 3,
                                                                TRUE ~ 0))
  }
  if (c("flag_protein_rcsi") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_protein_rcsi = dplyr::case_when(.data$flag_protein_rcsi < 2 ~ 0, 
                                                                          .data$flag_protein_rcsi >= 2 & .data$flag_protein_rcsi < 4 ~ 1, 
                                                                          .data$flag_protein_rcsi >= 4 & .data$flag_protein_rcsi < 10 ~ 2, 
                                                                          .data$flag_protein_rcsi >= 10 ~ 3, 
                                                                          TRUE ~ 0))
  }
  rcsi_plaus_vars <- c("plaus_flag_protein_rcsi", "plaus_flag_sd_rcsicoping", 
                       "plaus_flag_high_rcsi", "plaus_sd_rcsi")
  if (length(setdiff(c(rcsi_plaus_vars), names(df))) < 4) {
    plaus_nms <- intersect(rcsi_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_rcsi = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (c("flag_severe_hhs") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_severe_hhs = dplyr::case_when(.data$flag_severe_hhs < 1 ~ 0, 
                                                                        .data$flag_severe_hhs >= 1 & .data$flag_severe_hhs < 5 ~ 6, 
                                                                        .data$flag_severe_hhs >= 5 & .data$flag_severe_hhs < 10 ~ 8, 
                                                                        .data$flag_severe_hhs >= 10 ~ 10,
                                                                        TRUE ~ 0))
  }
  if (c("poisson_pvalues.hhs_severe") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_poisson.hhs_severe = ifelse(is.na(.data$poisson_pvalues.hhs_severe), 0, 
                                                                 ifelse(.data$poisson_pvalues.hhs_severe >= 0.05, 0,
                                                                        ifelse(.data$poisson_pvalues.hhs_severe >= 0.01 & .data$poisson_pvalues.hhs_severe < 0.05, 6,
                                                                               ifelse(.data$poisson_pvalues.hhs_severe >= 0.001 & .data$poisson_pvalues.hhs_severe < 0.01, 8,
                                                                                      ifelse(.data$poisson_pvalues.hhs_severe < 0.001, 10, 0))))))
  }
  hhs_plaus_vars <- c("plaus_flag_severe_hhs", "plaus_poisson.hhs_severe")
  if (length(setdiff(c(hhs_plaus_vars), names(df))) < 2) {
    plaus_nms <- intersect(hhs_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_hhs = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (c("flag_lcsi_liv_livestock") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_liv_livestock = dplyr::case_when(.data$flag_lcsi_liv_livestock < 2 ~ 0, 
                                                                                .data$flag_lcsi_liv_livestock >= 2 & .data$flag_lcsi_liv_livestock < 4 ~ 1, 
                                                                                .data$flag_lcsi_liv_livestock >= 4 & .data$flag_lcsi_liv_livestock < 10 ~ 2, 
                                                                                .data$flag_lcsi_liv_livestock >= 10 ~ 3, 
                                                                                TRUE ~ 0))
  }
  if (c("flag_lcsi_liv_agriculture") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_liv_agriculture = dplyr::case_when(.data$flag_lcsi_liv_agriculture < 2 ~ 0, 
                                                                                  .data$flag_lcsi_liv_agriculture >= 2 & .data$flag_lcsi_liv_agriculture < 4 ~ 1, 
                                                                                  .data$flag_lcsi_liv_agriculture >= 4 & .data$flag_lcsi_liv_agriculture < 10 ~ 2, 
                                                                                  .data$flag_lcsi_liv_agriculture >= 10 ~ 3,
                                                                                  TRUE ~ 0))
  }
  if (c("flag_lcsi_liv_agriculture") %in% names(df) | c("flag_lcsi_liv_livestock") %in% names(df)) {
    if (length(setdiff(c("flag_lcsi_liv_agriculture", "flag_lcsi_liv_livestock"), names(df))) == 0) {
      df <- df %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = dplyr::case_when(.data$plaus_flag_lcsi_liv_livestock >= .data$plaus_flag_lcsi_liv_agriculture ~ .data$plaus_flag_lcsi_liv_livestock, 
                                                                                  .data$plaus_flag_lcsi_liv_livestock < .data$plaus_flag_lcsi_liv_agriculture ~ .data$plaus_flag_lcsi_liv_agriculture,
                                                                                  TRUE ~ 0))
    }
    else if (length(setdiff(c("flag_lcsi_liv_agriculture"), names(df))) == 0) {
      df <- df %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = .data$plaus_flag_lcsi_liv_agriculture)
    }
    else if (length(setdiff(c("flag_lcsi_liv_livestock"), names(df))) == 0) {
      df <- df %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = .data$plaus_flag_lcsi_liv_livestock)
    }
  }
  if (c("flag_lcsi_coherence") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_coherence = dplyr::case_when(.data$flag_lcsi_coherence < 2 ~ 0, 
                                                                            .data$flag_lcsi_coherence >= 2 & .data$flag_lcsi_coherence < 4 ~ 3, 
                                                                            .data$flag_lcsi_coherence >= 4 & .data$flag_lcsi_coherence < 10 ~ 5, 
                                                                            .data$flag_lcsi_coherence >= 10 ~ 7, 
                                                                            TRUE ~ 0))
  }
  if (c("flag_lcsi_na") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_na = dplyr::case_when(.data$flag_lcsi_na < 2 ~ 0, 
                                                                     .data$flag_lcsi_na >= 2 & .data$flag_lcsi_na < 4 ~ 1, 
                                                                     .data$flag_lcsi_na >= 4 & .data$flag_lcsi_na < 10 ~ 3, 
                                                                     .data$flag_lcsi_na >= 10 ~ 5,
                                                                     TRUE ~ 0))
  }
  if (c("flag_lcsi_severity") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_severity = dplyr::case_when(.data$flag_lcsi_severity < 2 ~ 0, 
                                                                           .data$flag_lcsi_severity >= 2 & .data$flag_lcsi_severity < 4 ~ 1, 
                                                                           .data$flag_lcsi_severity >= 4 & .data$flag_lcsi_severity < 10 ~ 3, 
                                                                           .data$flag_lcsi_severity >= 10 ~ 5,
                                                                           TRUE ~ 0))
  }
  lcs_plaus_vars <- c("plaus_flag_lcsi_agr_livestock", "plaus_flag_lcsi_coherence", 
                      "plaus_flag_lcsi_na", "plaus_flag_lcsi_severity")
  if (length(setdiff(lcs_plaus_vars, names(df))) < 4) {
    plaus_nms <- intersect(lcs_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_lcsi = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (length(setdiff(c("corr.fcs_rcsi", "corr.fcs_rcsi.pvalue"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_corr.fcs_rcsi = ifelse(.data$corr.fcs_rcsi < -0.2 & .data$corr.fcs_rcsi.pvalue < 0.05, 0,
                                                            ifelse(.data$corr.fcs_rcsi < -0.2 & .data$corr.fcs_rcsi.pvalue >= 0.05, 1,
                                                                   ifelse(.data$corr.fcs_rcsi >= -0.2 & .data$corr.fcs_rcsi < 0.2 & .data$corr.fcs_rcsi.pvalue >= 0.05, 2, 
                                                                          ifelse(.data$corr.fcs_rcsi >= -0.2 & .data$corr.fcs_rcsi < 0.2 & .data$corr.fcs_rcsi.pvalue < 0.05, 3,
                                                                                 ifelse(.data$corr.fcs_rcsi >= 0.2 & .data$corr.fcs_rcsi.pvalue >= 0.05, 4,
                                                                                        ifelse(.data$corr.fcs_rcsi >= 0.2 & .data$corr.fcs_rcsi.pvalue < 0.05, 5, 0)))))))
  }
  if (length(setdiff(c("corr.fcs_hhs", "corr.fcs_hhs.pvalue"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_corr.fcs_hhs = ifelse(.data$corr.fcs_hhs < -0.2 & .data$corr.fcs_hhs.pvalue < 0.05, 0,
                                                           ifelse(.data$corr.fcs_hhs < -0.2 & .data$corr.fcs_hhs.pvalue >= 0.05, 1,
                                                                  ifelse(.data$corr.fcs_hhs >= -0.2 & .data$corr.fcs_hhs < 0.2 & .data$corr.fcs_hhs.pvalue >= 0.05, 1.5,
                                                                         ifelse(.data$corr.fcs_hhs >= -0.2 & .data$corr.fcs_hhs < 0.2 & .data$corr.fcs_hhs.pvalue < 0.05, 2,
                                                                                ifelse(.data$corr.fcs_hhs >= 0.2 & .data$corr.fcs_hhs.pvalue >= 0.05, 2.5, 
                                                                                       ifelse(.data$corr.fcs_hhs >= 0.2 & .data$corr.fcs_hhs.pvalue < 0.05, 3, 0)))))))
  }
  if (length(setdiff(c("corr.hhs_rcsi", "corr.hhs_rcsi.pvalue"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_corr.hhs_rcsi = ifelse(.data$corr.hhs_rcsi > 0.2 & .data$corr.hhs_rcsi.pvalue < 0.05, 0,
                                                            ifelse(.data$corr.hhs_rcsi > 0.2 & .data$corr.hhs_rcsi.pvalue >= 0.05, 1,
                                                                   ifelse(.data$corr.hhs_rcsi > -0.2 & .data$corr.hhs_rcsi <= 0.2 & .data$corr.hhs_rcsi.pvalue < 0.05, 1.5, 
                                                                          ifelse(.data$corr.hhs_rcsi > -0.2 & .data$corr.hhs_rcsi <= 0.2 & .data$corr.hhs_rcsi.pvalue >= 0.05, 2,
                                                                                 ifelse(.data$corr.hhs_rcsi <= -0.2 & .data$corr.hhs_rcsi.pvalue >= 0.05, 2.5, 
                                                                                        ifelse(.data$corr.hhs_rcsi <= -0.2 & .data$corr.hhs_rcsi.pvalue < 0.05, 3, 0)))))))
  }
  if (c("prop_fc_flags") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_fc_flags = ifelse(.data$prop_fc_flags < 0.02, 0,
                                                            ifelse(.data$prop_fc_flags < 0.04, 2,
                                                                   ifelse(.data$prop_fc_flags < 0.1, 4,
                                                                          ifelse(.data$prop_fc_flags >= 0.1, 6, 0)))))
  }
  if (c("flag_fcsrcsi_box") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_fcsrcsi_box = dplyr::case_when(.data$flag_fcsrcsi_box < 2 ~ 0,
                                                                         .data$flag_fcsrcsi_box >= 2 & .data$flag_fcsrcsi_box < 4 ~ 2,
                                                                         .data$flag_fcsrcsi_box >= 4 & .data$flag_fcsrcsi_box < 10 ~ 4,
                                                                         .data$flag_fcsrcsi_box >= 10 ~ 6,
                                                                         TRUE ~ 0))
  }
  if (c("flag_fcs_rcsi") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_fcs_rcsi = dplyr::case_when(.data$flag_fcs_rcsi < 2 ~ 0, 
                                                                      .data$flag_fcs_rcsi >= 2 & .data$flag_fcs_rcsi < 4 ~ 1, 
                                                                      .data$flag_fcs_rcsi >= 4 & .data$flag_fcs_rcsi < 10 ~ 2, 
                                                                      .data$flag_fcs_rcsi >= 10 ~ 3,
                                                                      TRUE ~ 0))
  }
  other_fsl_plaus_vars <- c("plaus_prop_fc_flags", "plaus_corr.hhs_rcsi", 
                            "plaus_corr.fcs_hhs", "plaus_corr.fcs_rcsi", "plaus_flag_fcsrcsi_box", 
                            "plaus_flag_fcs_rcsi")
  if (length(setdiff(other_fsl_plaus_vars, names(df))) < 6) {
    plaus_nms <- intersect(other_fsl_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_other_fsl = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup()
  }
  all_fsl_plaus_vars <- c("plaus_lcsi", "plaus_fcs", "plaus_rcsi", 
                          "plaus_hhs", "plaus_other_fsl")
  if (length(setdiff(all_fsl_plaus_vars, names(df))) < 5) {
    plaus_nms <- intersect(all_fsl_plaus_vars, names(df))
    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plaus_fsl_score = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(plaus_fsl_cat = dplyr::case_when(.data$plaus_fsl_score < 20 ~ "Good",
                                                     .data$plaus_fsl_score >= 20 & .data$plaus_fsl_score < 40 ~ "Moderate",
                                                     .data$plaus_fsl_score >= 40 ~ "Problematic"))
  }
  if (c("mad_ratio.pvalue") %in% colnames(df)) {
    df <- df %>% dplyr::mutate(plaus_mad_ratio.pvalue = ifelse(.data$mad_ratio.pvalue > 0.05, 0,
                                                               ifelse(.data$mad_ratio.pvalue > 0.001, 5,
                                                                      ifelse(.data$mad_ratio.pvalue > 1e-04, 10,
                                                                             ifelse(.data$mad_ratio.pvalue <= 1e-04, 20, 0)))))
  }
  if (c("prop_flag_high_mdd_low_mmf") %in% colnames(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_flag_high_mdd_low_mmf = ifelse(.data$prop_flag_high_mdd_low_mmf < 0.01, 0,
                                                                         ifelse(.data$prop_flag_high_mdd_low_mmf < 0.05, 5,
                                                                                ifelse(.data$prop_flag_high_mdd_low_mmf < 0.1, 10, 
                                                                                       ifelse(.data$prop_flag_high_mdd_low_mmf >= 0.1, 20, 0)))))
  }
  if (c("age_ratio_under6m_6to23m.pvalue") %in% colnames(df)) {
    df <- df %>% dplyr::mutate(plaus_age_ratio_under6m_6to23m.pvalue = ifelse(.data$age_ratio_under6m_6to23m.pvalue > 0.05, 0,
                                                                              ifelse(.data$age_ratio_under6m_6to23m.pvalue > 0.01, 2,
                                                                                     ifelse(.data$age_ratio_under6m_6to23m.pvalue > 0.001, 5,
                                                                                            ifelse(.data$age_ratio_under6m_6to23m.pvalue <= 0.001, 10, 0)))))
  }
  if (c("sd_mdd") %in% colnames(df)) {
    df <- df %>% dplyr::mutate(plaus_sdd_mdd = ifelse(.data$sd_mdd > 1 & .data$sd_mdd < 2, 0,
                                                      ifelse(.data$sd_mdd > 0.8 & .data$sd_mdd < 2.2, 5,
                                                             ifelse(.data$sd_mdd <= 0.8 | .data$sd_mdd >= 2.2, 10, 0))))
  }
  if (c("prop_iycf_caregiver" %in% colnames(df))) {
    df <- df %>% dplyr::mutate(plaus_prop_iycf_caregiver = ifelse(.data$prop_iycf_caregiver >= 0.9, 0,
                                                                  ifelse(.data$prop_iycf_caregiver >= 0.8, 2, 
                                                                         ifelse(.data$prop_iycf_caregiver >= 0.7, 5,
                                                                                ifelse(.data$prop_iycf_caregiver >= 0.5, 10, 10)))))
  }
  iycf_plaus_vars <- c("plaus_sdd_mdd", "plaus_age_ratio_under6m_6to23m.pvalue", 
                       "plaus_sexratio", "plaus_prop_flag_high_mdd_low_mmf", 
                       "plaus_mad_ratio.pvalue")
  if (length(setdiff(iycf_plaus_vars, colnames(df))) == 0) {
    if (!(c("plaus_prop_iycf_caregiver") %in% colnames(df))) {
      df <- df %>% dplyr::mutate(plaus_prop_iycf_caregiver = 10)
      print("No iycf_caregiver variable was available. Plaus penalty of 10 applied assuming this wasn't done during the survey.")
    }
    df <- df %>% dplyr::mutate(iycf_plaus_score = .data$plaus_prop_iycf_caregiver + .data$plaus_sdd_mdd +
                                 .data$plaus_age_ratio_under6m_6to23m.pvalue + .data$plaus_sexratio +
                                 .data$plaus_prop_flag_high_mdd_low_mmf + .data$plaus_mad_ratio.pvalue,
                               iycf_plaus_cat = ifelse(.data$iycf_plaus_score >= 0 & .data$iycf_plaus_score < 10, "Excellent (0-<10)", 
                                                       ifelse(.data$iycf_plaus_score >= 10 & .data$iycf_plaus_score < 15, "Good (10-<15)",
                                                              ifelse(.data$iycf_plaus_score >= 15 & .data$iycf_plaus_score < 25, "Acceptable (15 - <25)", 
                                                                     ifelse(.data$iycf_plaus_score >= 25, "Problematic (>=25)", NA)))))
  }
  else {
    print(paste0("Not all necessary variables for IYCF plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(iycf_plaus_vars, names(df)))
    print(setdiff(c("prop_iycf_caregiver"), names(df)))
  }
  return(df)
}

######## ADD INDICATORS
add_fcs_new <- function (.dataset,
                         cutoffs = c("normal","alternative"),
                         fcs_cereal = "fcs_cereal",
                         fcs_legumes = "fcs_legumes", 
                         fcs_veg = "fcs_veg", 
                         fcs_fruit = "fcs_fruit", 
                         fcs_meat = "fcs_meat", 
                         fcs_dairy = "fcs_dairy", 
                         fcs_sugar = "fcs_sugar", 
                         fcs_oil = "fcs_oil") {
  .dataset <- as.data.frame(.dataset)
  
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  if ("fcs_score" %in% names(.dataset)) {
    cat("There is already a variable called fcs_score in your dataset, it will be overwritten")
  }
  if ("fcs_cat" %in% names(.dataset)) {
    cat("There is already a variable called fcs_cat in your dataset, it will be overwritten")
  }
  fcs_vars <- c(fcs_cereal, fcs_legumes, fcs_dairy, fcs_meat, 
                fcs_veg, fcs_fruit, fcs_oil, fcs_sugar)
  tryCatch({
    .dataset %>%
      dplyr::select(dplyr::all_of(c(fcs_vars)))
  }, error = function(e) {
    message("Missing fcs columns")
  })

  if (!all(.dataset[[fcs_cereal]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_cereal, 
                 paste0(unique(.dataset[[fcs_cereal]][!.dataset[[fcs_cereal]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[fcs_legumes]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_legumes, 
                 paste0(unique(.dataset[[fcs_legumes]][!.dataset[[fcs_legumes]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[fcs_dairy]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_dairy, 
                 paste0(unique(.dataset[[fcs_dairy]][!.dataset[[fcs_dairy]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[fcs_meat]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_meat, 
                 paste0(unique(.dataset[[fcs_meat]][!.dataset[[fcs_meat]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[fcs_veg]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_veg, 
                 paste0(unique(.dataset[[fcs_veg]][!.dataset[[fcs_veg]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[fcs_fruit]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_fruit, 
                 paste0(unique(.dataset[[fcs_fruit]][!.dataset[[fcs_fruit]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[fcs_oil]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_oil, 
                 paste0(unique(.dataset[[fcs_oil]][!.dataset[[fcs_oil]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[fcs_sugar]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_sugar, 
                 paste0(unique(.dataset[[fcs_sugar]][!.dataset[[fcs_sugar]] %in% c(0:7)]), collapse = "/")))
  }
  
  .dataset <- .dataset %>%
    dplyr::mutate_at(vars(fcs_vars), as.numeric) %>% 
    dplyr::mutate(fcs_weight_cereal1 = ifelse(is.na(!!rlang::sym(fcs_cereal)), NA, !!rlang::sym(fcs_cereal) * 2),
                  fcs_weight_legume2 = ifelse(is.na(!!rlang::sym(fcs_legumes)), NA, !!rlang::sym(fcs_legumes) * 3),
                  fcs_weight_dairy3 = ifelse(is.na(!!rlang::sym(fcs_dairy)), NA, !!rlang::sym(fcs_dairy) * 4),
                  fcs_weight_meat4 = ifelse(is.na(!!rlang::sym(fcs_meat)), NA, !!rlang::sym(fcs_meat) * 4),
                  fcs_weight_veg5 = ifelse(is.na(!!rlang::sym(fcs_veg)), NA, !!rlang::sym(fcs_veg) * 1),
                  fcs_weight_fruit6 = ifelse(is.na(!!rlang::sym(fcs_fruit)), NA, !!rlang::sym(fcs_fruit) * 1),
                  fcs_weight_oil7 = ifelse(is.na(!!rlang::sym(fcs_oil)), NA, !!rlang::sym(fcs_oil) * 0.5),
                  fcs_weight_sugar8 = ifelse(is.na(!!rlang::sym(fcs_sugar)), NA, !!rlang::sym(fcs_sugar) * 0.5)) %>% 
    dplyr::mutate(fcs_score = rowSums(across(c(fcs_weight_cereal1, fcs_weight_legume2, fcs_weight_dairy3, fcs_weight_meat4, fcs_weight_veg5,
                                               fcs_weight_fruit6, fcs_weight_oil7, fcs_weight_sugar8), .fns = as.numeric), na.rm = T))
  if (cutoffs == "normal") {
    .dataset <- .dataset %>% dplyr::mutate(fcs_cat = dplyr::case_when(fcs_score < 21.5 ~ "Poor",
                                                          fcs_score <= 35 ~ "Borderline", 
                                                          fcs_score > 35 ~ "Acceptable", 
                                                          TRUE ~ NA_character_))
  } else if (cutoffs == "alternative") {
    .dataset <- .dataset %>% dplyr::mutate(fcs_cat = dplyr::case_when(fcs_score <= 28 ~ "Poor",
                                                          fcs_score <= 42 ~ "Borderline",
                                                          fcs_score > 42 ~ "Acceptable",
                                                          TRUE ~ NA_character_))
  }
  return(.dataset)
}

add_hhs_new <- function (.dataset,
                         hhs_nofoodhh = "hhs_nofoodhh",
                         hhs_nofoodhh_freq = "hhs_nofoodhh_freq", 
                         hhs_sleephungry = "hhs_sleephungry",
                         hhs_sleephungry_freq = "hhs_sleephungry_freq", 
                         hhs_alldaynight = "hhs_alldaynight",
                         hhs_alldaynight_freq = "hhs_alldaynight_freq", 
                         yes_answer = "yes",no_answer = "no",
                         rarely_answer = "rarely",
                         sometimes_answer = "sometimes",
                         often_answer = "often") {
  
  .dataset <- as.data.frame(.dataset)
  
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  
  tryCatch({
    .dataset %>%
      dplyr::select(dplyr::all_of(c(hhs_nofoodhh, hhs_nofoodhh_freq,hhs_sleephungry,
                                    hhs_sleephungry_freq, hhs_alldaynight,hhs_alldaynight_freq)))
  }, error = function(e) {
    message("Missing hhs columns")
  })
  
  if (!all(.dataset[[hhs_nofoodhh]] %in% c(yes_answer, no_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_nofoodhh, 
                 paste0(unique(.dataset[[hhs_nofoodhh]][!.dataset[[hhs_nofoodhh]] %in% c(yes_answer, no_answer, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_sleephungry]] %in% c(yes_answer, no_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_sleephungry, 
                 paste0(unique(.dataset[[hhs_sleephungry]][!.dataset[[hhs_sleephungry]] %in% c(yes_answer, no_answer, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_alldaynight]] %in% c(yes_answer, no_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_alldaynight, 
                 paste0(unique(.dataset[[hhs_alldaynight_3]][!.dataset[[hhs_alldaynight_3]] %in% c(yes_answer, no_answer, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_nofoodhh_freq]] %in% c(rarely_answer, sometimes_answer, often_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_nofoodhh_freq, 
                 paste0(unique(.dataset[[hhs_nofoodhh_freq]][!.dataset[[hhs_nofoodhh_freq]] %in% c(rarely_answer, sometimes_answer, often_answer, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_sleephungry_freq]] %in% c(rarely_answer, sometimes_answer, often_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_sleephungry_freq, 
                 paste0(unique(.dataset[[hhs_sleephungry_freq]][!.dataset[[hhs_sleephungry_freq]] %in% c(rarely_answer, sometimes_answer, often_answer, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_alldaynight_freq]] %in% c(rarely_answer, sometimes_answer, often_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_alldaynight_freq, 
                 paste0(unique(.dataset[[hhs_alldaynight_freq]][!.dataset[[hhs_alldaynight_freq]] %in% c(rarely_answer, sometimes_answer, often_answer, NA)]), collapse = "/")))
  }
  .dataset_with_calculation <- .dataset %>%
    dplyr::mutate_at(c(hhs_nofoodhh, hhs_sleephungry, hhs_alldaynight),~dplyr::case_when(.x == yes_answer ~ 1,
                                                                                         .x == no_answer ~ 0)) %>%
    dplyr::mutate_at(c(hhs_nofoodhh_freq, hhs_sleephungry_freq, hhs_alldaynight_freq), ~dplyr::case_when(.x %in% c(rarely_answer, sometimes_answer) ~ 1,
                                                                                                   .x == often_answer ~ 2, TRUE ~ 0)) %>%
    dplyr::rowwise() %>% dplyr::mutate(hhs_comp1 = !!rlang::sym(hhs_nofoodhh) * !!rlang::sym(hhs_nofoodhh_freq),
                                       hhs_comp2 = !!rlang::sym(hhs_sleephungry) * !!rlang::sym(hhs_sleephungry_freq),
                                       hhs_comp3 = !!rlang::sym(hhs_alldaynight) * !!rlang::sym(hhs_alldaynight_freq)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(hhs_score = rowSums(.[grep("^hhs_comp\\d$", names(.))])) %>% 
    dplyr::mutate(hhs_cat_ipc = dplyr::case_when(hhs_score == 0 ~ "None",
                                                 hhs_score == 1 ~ "Little",
                                                 hhs_score <= 3 ~ "Moderate",
                                                 hhs_score == 4 ~ "Severe",
                                                 hhs_score <= 6 ~ "Very Severe"),
                  hhs_cat = dplyr::case_when(hhs_score <= 1 ~ "No or Little",
                                             hhs_score <= 3 ~ "Moderate",
                                             hhs_score <= 6 ~ "Severe",
                                             TRUE ~ NA_character_))
  columns_to_export <- .dataset_with_calculation %>%
    dplyr::rename_at(c(hhs_nofoodhh, hhs_nofoodhh_freq, hhs_sleephungry,
                       hhs_sleephungry_freq, hhs_alldaynight, hhs_alldaynight_freq), ~paste0(.x, "_recoded")) %>%
    dplyr::select(paste0(hhs_nofoodhh, "_recoded"),
                  paste0(hhs_nofoodhh_freq, "_recoded"),
                  paste0(hhs_sleephungry, "_recoded"),
                  paste0(hhs_sleephungry_freq, "_recoded"),
                  paste0(hhs_alldaynight, "_recoded"),
                  paste0(hhs_alldaynight_freq, "_recoded"),
                  hhs_comp1, hhs_comp2, hhs_comp3, hhs_score, hhs_cat_ipc, hhs_cat)
  .dataset <- .dataset %>%
    dplyr::select(-c(hhs_comp1, hhs_comp2, hhs_comp3, hhs_score, hhs_cat)) %>% 
    cbind(columns_to_export)
  
  return(.dataset)
}

add_rcsi_new <- function (.dataset,
                          rcsi_lessquality = "rcsi_lessquality",
                          rcsi_borrow = "rcsi_borrow", 
                          rcsi_mealsize = "rcsi_mealsize",
                          rcsi_mealadult = "rcsi_mealadult", 
                          rcsi_mealnb = "rcsi_mealnb") {
  
  .dataset <- as.data.frame(.dataset)
  
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  
  tryCatch({
    .dataset %>%
      dplyr::select(dplyr::all_of(c(rcsi_lessquality,rcsi_borrow,rcsi_mealsize,rcsi_mealadult,rcsi_mealnb)))
  }, error = function(e) {
    message("Missing rcsi columns")
  })
  
  if ("rcsi_score" %in% names(.dataset)) {
    cat("There is already a variable called rcsi_score in your dataset, it will be overwritten")
  }
  
  if ("rcsi_cat" %in% names(.dataset)) {
    cat("There is already a variable called rcsi_cat in your dataset, it will be overwritten")
  }
  
  if (!all(.dataset[[rcsi_lessquality]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", rcsi_lessquality, 
                 paste0(unique(.dataset[[rcsi_lessquality]][!.dataset[[rcsi_lessquality]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[rcsi_borrow]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", rcsi_borrow, 
                 paste0(unique(.dataset[[rcsi_borrow]][!.dataset[[rcsi_borrow]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[rcsi_mealsize]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", rcsi_mealsize, 
                 paste0(unique(.dataset[[rcsi_mealsize]][!.dataset[[rcsi_mealsize]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[rcsi_mealadult]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", rcsi_mealadult, 
                 paste0(unique(.dataset[[rcsi_mealadult]][!.dataset[[rcsi_mealadult]] %in% c(0:7)]), collapse = "/")))
  }
  
  if (!all(.dataset[[rcsi_mealnb]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", rcsi_mealnb, 
                 paste0(unique(.dataset[[rcsi_mealnb]][!.dataset[[rcsi_mealnb]] %in% c(0:7)]), collapse = "/")))
  }
  
  rcs_columns <- c(rcsi_lessquality,rcsi_borrow,rcsi_mealsize,rcsi_mealadult,rcsi_mealnb)
  
  .dataset <- .dataset %>%
    dplyr::mutate_at(vars(rcs_columns), as.numeric) %>% 
    dplyr::mutate(rcsi_lessquality_weighted = ifelse(is.na(!!rlang::sym(rcsi_lessquality)), NA,!!rlang::sym(rcsi_lessquality) * 1),
                  rcsi_borrow_weighted = ifelse(is.na(!!rlang::sym(rcsi_borrow)), NA,!!rlang::sym(rcsi_borrow) * 2),
                  rcsi_mealsize_weighted = ifelse(is.na(!!rlang::sym(rcsi_mealsize)), NA,!!rlang::sym(rcsi_mealsize) * 1),
                  rcsi_mealadult_weighted = ifelse(is.na(!!rlang::sym(rcsi_mealadult)), NA,!!rlang::sym(rcsi_mealadult) * 3),
                  rcsi_mealnb_weighted = ifelse(is.na(!!rlang::sym(rcsi_mealnb)), NA,!!rlang::sym(rcsi_mealnb) * 1),
                  rcsi_score = rowSums(across(c(rcsi_lessquality_weighted,
                                                rcsi_borrow_weighted,
                                                rcsi_mealsize_weighted,
                                                rcsi_mealadult_weighted,
                                                rcsi_mealnb_weighted), .fns = as.numeric), na.rm = T),
                  rcsi_cat = dplyr::case_when(rcsi_score <= 3 ~ "No to Low",
                                              rcsi_score <= 18 ~ "Medium",
                                              rcsi_score > 18 ~ "High",
                                              TRUE ~ NA_character_))
  return(.dataset)
}


add_lcsi_new <- function (.dataset,
                          lcsi_stress1 = "lcsi_stress1",
                          lcsi_stress2 = "lcsi_stress2",
                          lcsi_stress3 = "lcsi_stress3",
                          lcsi_stress4 = "lcsi_stress4",
                          lcsi_crisis1 = "lcsi_crisis1",
                          lcsi_crisis2 = "lcsi_crisis2",
                          lcsi_crisis3 = "lcsi_crisis3",
                          lcsi_emergency1 = "lcsi_emergency1",
                          lcsi_emergency2 = "lcsi_emergency2",
                          lcsi_emergency3 = "lcsi_emergency3",
                          yes_val = "yes",
                          no_val = "no_had_no_need",
                          exhausted_val = "no_exhausted", 
                          not_applicable_val = "not_applicable") {
  .dataset <- as.data.frame(.dataset)
  
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  
  tryCatch({
    .dataset %>%
      dplyr::select(dplyr::all_of(c(lcsi_stress1,lcsi_stress2,lcsi_stress3,lcsi_stress4,
                                    lcsi_crisis1,lcsi_crisis2,lcsi_crisis3,
                                    lcsi_emergency1,lcsi_emergency2,lcsi_emergency3)))
  }, error = function(e) {
    message("Missing lcsi columns")
  })
  
  if (!all(.dataset[[lcsi_stress1]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_stress1, 
                 paste0(unique(.dataset[[lcsi_stress1]][!.dataset[[lcsi_stress1]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[lcsi_stress2]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_stress2, 
                 paste0(unique(.dataset[[lcsi_stress2]][!.dataset[[lcsi_stress2]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[lcsi_stress3]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_stress3, 
                 paste0(unique(.dataset[[lcsi_stress3]][!.dataset[[lcsi_stress3]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[lcsi_stress4]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_stress4, 
                 paste0(unique(.dataset[[lcsi_stress4]][!.dataset[[lcsi_stress4]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[lcsi_crisis1]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_crisis1, 
                 paste0(unique(.dataset[[lcsi_crisis1]][!.dataset[[lcsi_crisis1]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[lcsi_crisis2]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_crisis2, 
                 paste0(unique(.dataset[[lcsi_crisis2]][!.dataset[[lcsi_crisis2]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[lcsi_crisis3]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_crisis3, 
                 paste0(unique(.dataset[[lcsi_crisis3]][!.dataset[[lcsi_crisis3]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[lcsi_emergency1]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_emergency1, 
                 paste0(unique(.dataset[[lcsi_emergency1]][!.dataset[[lcsi_emergency1]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[lcsi_emergency2]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_emergency2, 
                 paste0(unique(.dataset[[lcsi_emergency2]][!.dataset[[lcsi_emergency2]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[lcsi_emergency3]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", lcsi_emergency3, 
                 paste0(unique(.dataset[[lcsi_emergency3]][!.dataset[[lcsi_emergency3]] %in% c(yes_val,no_val,exhausted_val,not_applicable_val, NA)]), collapse = "/")))
  }
  
  .dataset <- .dataset %>%
    dplyr::select(-c(lcs_stress,lcs_crisis,lcs_emergency)) %>% 
    dplyr::mutate(lcsi_stress_yes = dplyr::case_when(!!rlang::sym(lcsi_stress1) == "yes" | !!rlang::sym(lcsi_stress2) == "yes" | !!rlang::sym(lcsi_stress3) == "yes" | !!rlang::sym(lcsi_stress4) == "yes" ~ "1", TRUE ~ "0"),
                  lcsi_stress_exhaust = dplyr::case_when(!!rlang::sym(lcsi_stress1) == "no_exhausted" | !!rlang::sym(lcsi_stress2) == "no_exhausted" | !!rlang::sym(lcsi_stress3) == "no_exhausted" | !!rlang::sym(lcsi_stress4) == "no_exhausted" ~ "1", 
                                                         TRUE ~ "0"),
                  lcsi_stress = dplyr::case_when(lcsi_stress_yes == "1" | lcsi_stress_exhaust == "1" ~ "1",
                                                 TRUE ~ "0"), 
                  lcsi_crisis_yes = dplyr::case_when(!!rlang::sym(lcsi_crisis1) == "yes" | !!rlang::sym(lcsi_crisis2) == "yes" | !!rlang::sym(lcsi_crisis3) == "yes" ~ "1",
                                                     TRUE ~ "0"),
                  lcsi_crisis_exhaust = dplyr::case_when(!!rlang::sym(lcsi_crisis1) == "no_exhausted" | !!rlang::sym(lcsi_crisis2) == "no_exhausted" | !!rlang::sym(lcsi_crisis3) == "no_exhausted" ~ "1",
                                                         TRUE ~ "0"), 
                  lcsi_crisis = dplyr::case_when(lcsi_crisis_yes == "1" | lcsi_crisis_exhaust == "1" ~ "1",
                                                 TRUE ~ "0"),
                  lcsi_emergency_yes = dplyr::case_when(!!rlang::sym(lcsi_emergency1) == "yes" | !!rlang::sym(lcsi_emergency2) == "yes" | !!rlang::sym(lcsi_emergency3) == "yes" ~ "1",
                                                        TRUE ~ "0"),
                  lcsi_emergency_exhaust = dplyr::case_when(!!rlang::sym(lcsi_emergency1) == "no_exhausted" | !!rlang::sym(lcsi_emergency2) == "no_exhausted" | !!rlang::sym(lcsi_emergency3) == "no_exhausted" ~ "1",
                                                            TRUE ~ "0"),
                  lcsi_emergency = dplyr::case_when(lcsi_emergency_yes == "1" | lcsi_emergency_exhaust == "1" ~ "1",
                                                    TRUE ~ "0"),
                  lcsi_cat_yes = dplyr::case_when(lcsi_stress_yes != "1" & lcsi_crisis_yes != "1" & lcsi_emergency_yes != "1" ~ "None",
                                                  lcsi_stress_yes == "1" & lcsi_crisis_yes != "1" & lcsi_emergency_yes != "1" ~ "Stress",
                                                  lcsi_crisis_yes == "1" & lcsi_emergency_yes != "1" ~ "Crisis", 
                                                  lcsi_emergency_yes == "1" ~ "Emergency", TRUE ~ NA_character_),
                  lcsi_cat_exhaust = dplyr::case_when(lcsi_stress_exhaust != "1" & lcsi_crisis_exhaust != "1" & lcsi_emergency_exhaust != "1" ~ "None",
                                                      lcsi_stress_exhaust == "1" & lcsi_crisis_exhaust != "1" & lcsi_emergency_exhaust != "1" ~ "Stress", 
                                                      lcsi_crisis_exhaust == "1" & lcsi_emergency_exhaust != "1" ~ "Crisis",
                                                      lcsi_emergency_exhaust == "1" ~ "Emergency", 
                                                      TRUE ~ NA_character_),
                  lcsi_cat = dplyr::case_when(lcsi_stress != "1" & lcsi_crisis != "1" & lcsi_emergency != "1" ~ "None",
                                              lcsi_stress == "1" & lcsi_crisis != "1" & lcsi_emergency != "1" ~ "Stress",
                                              lcsi_crisis == "1" & lcsi_emergency != "1" ~ "Crisis",
                                              lcsi_emergency == "1" ~ "Emergency",
                                              TRUE ~ NA_character_))
  
  return(.dataset)
}

add_hdds_new <- function(.dataset,
                         hdds_cereals = "hdds_cereals",
                         hdds_tubers = "hdds_tubers", 
                         hdds_veg = "hdds_veg", 
                         hdds_fruit = "hdds_fruit",
                         hdds_meat = "hdds_meat", 
                         hdds_eggs = "hdds_eggs",
                         hdds_fish = "hdds_fish", 
                         hdds_legumes = "hdds_legumes",
                         hdds_dairy = "hdds_dairy",
                         hdds_oil = "hdds_oil",
                         hdds_sugar = "hdds_sugar", 
                         hdds_condiments = "hdds_condiments",
                         yes_val = "yes", 
                         no_val = "no") {
  
  .dataset <- as.data.frame(.dataset)
  
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  
  tryCatch({
    .dataset %>%
      dplyr::select(dplyr::all_of(c(hdds_cereals,hdds_tubers,hdds_veg,hdds_fruit,
                                    hdds_meat,hdds_eggs,hdds_fish,hdds_legumes,
                                    hdds_dairy,hdds_oil,hdds_sugar,hdds_condiments)))
  }, error = function(e) {
    message("Missing hdds columns")
  })
  
  if (!all(.dataset[[hdds_cereals]] %in% c(yes_val, no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_cereals, 
                 paste0(unique(.dataset[[hdds_cereals]][!.dataset[[hdds_cereals]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_tubers]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_tubers, 
                 paste0(unique(.dataset[[hdds_tubers]][!.dataset[[hdds_tubers]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_veg]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_veg, 
                 paste0(unique(.dataset[[hdds_veg]][!.dataset[[hdds_veg]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_fruit]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_fruit, 
                 paste0(unique(.dataset[[hdds_fruit]][!.dataset[[hdds_fruit]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_meat]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_meat, 
                 paste0(unique(.dataset[[hdds_meat]][!.dataset[[hdds_meat]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_eggs]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_eggs, 
                 paste0(unique(.dataset[[hdds_eggs]][!.dataset[[hdds_eggs]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_fish]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_fish, 
                 paste0(unique(.dataset[[hdds_fish]][!.dataset[[hdds_fish]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_legumes]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_legumes, 
                 paste0(unique(.dataset[[hdds_legumes]][!.dataset[[hdds_legumes]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_dairy]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_dairy, 
                 paste0(unique(.dataset[[hdds_dairy]][!.dataset[[hdds_dairy]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_oil]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_oil, 
                 paste0(unique(.dataset[[hdds_oil]][!.dataset[[hdds_oil]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_sugar]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_sugar, 
                 paste0(unique(.dataset[[hdds_sugar]][!.dataset[[hdds_sugar]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hdds_condiments]] %in% c(yes_val,no_val, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hdds_condiments, 
                 paste0(unique(.dataset[[hdds_condiments]][!.dataset[[hdds_condiments]] %in% c(yes_val, no_val, NA)]), collapse = "/")))
  }
  
  hdds_col <- c(hdds_cereals,hdds_tubers,hdds_veg,hdds_fruit,
                hdds_meat,hdds_eggs,hdds_fish,hdds_legumes,
                hdds_dairy,hdds_oil,hdds_sugar,hdds_condiments)
  
  .dataset_with_calculation <- .dataset %>%
    dplyr::mutate_at(vars(hdds_col), ~ifelse(is.na(.),NA,
                                             ifelse(. == yes_val, 1,0))) %>% 
    dplyr::mutate(hdds_score = rowSums(across(c(hdds_col), .fns = as.numeric), na.rm = T),
                  hdds_cat = dplyr::case_when(hdds_score <= 2 ~ "Low",
                                              hdds_score <= 4 ~ "Medium", 
                                              hdds_score > 4 ~ "High",
                                              TRUE ~ NA_character_))
  
  columns_to_export <- .dataset_with_calculation %>%
    dplyr::rename_at(c(hdds_cereals,hdds_tubers,hdds_veg,hdds_fruit,
                       hdds_meat,hdds_eggs,hdds_fish,hdds_legumes,
                       hdds_dairy,hdds_oil,hdds_sugar,hdds_condiments), ~paste0(., "_recoded")) %>%
    dplyr::select(paste0(hdds_cereals, "_recoded"),
                  paste0(hdds_tubers, "_recoded"),
                  paste0(hdds_veg, "_recoded"),
                  paste0(hdds_fruit, "_recoded"),
                  paste0(hdds_meat, "_recoded"),
                  paste0(hdds_eggs, "_recoded"),
                  paste0(hdds_fish, "_recoded"),
                  paste0(hdds_legumes, "_recoded"),
                  paste0(hdds_dairy, "_recoded"),
                  paste0(hdds_oil, "_recoded"),
                  paste0(hdds_sugar, "_recoded"),
                  paste0(hdds_condiments, "_recoded"),
                  hdds_score, hdds_cat)
  .dataset <- .dataset %>%
    cbind(columns_to_export)
  
  return(.dataset)
}

add_fcm_phase_new <- function (.dataset, 
                               fcs_column_name = "fcs_cat",
                               rcsi_column_name = "rcsi_cat", 
                               hhs_column_name = "hhs_cat",
                               hdds_column_name = "hdds_cat",
                               fcs_categories_acceptable = "Acceptable", 
                               fcs_categories_poor = "Poor",
                               fcs_categories_borderline = "Borderline", 
                               rcsi_categories_low = "No to Low",
                               rcsi_categories_medium = "Medium", 
                               rcsi_categories_high = "High", 
                               hhs_categories_none = "None", 
                               hhs_categories_little = "No or Little",
                               hhs_categories_moderate = "Moderate", 
                               hhs_categories_severe = "Severe",
                               hhs_categories_very_severe = "Very Severe",
                               hdds_categories_low = "Low",
                               hdds_categories_medium = "Medium",
                               hdds_categories_high = "High") {
  .dataset <- as.data.frame(.dataset)
  
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  
  if(all(c(hdds_column_name,fcs_column_name) %in% names(.dataset))) {
    fews_vars <- c(fcs_column_name, rcsi_column_name, hhs_column_name, hdds_column_name)
  } else if (hdds_column_name %in% names(.dataset) & !fcs_column_name %in% names(.dataset)) {
    fews_vars <- c(rcsi_column_name, hhs_column_name, hdds_column_name)
  } else {
    fews_vars <- c(rcsi_column_name, hhs_column_name, fcs_column_name)
  }
  
  tryCatch({
    .dataset %>%
      dplyr::select(dplyr::all_of(c(fews_vars)))
  }, error = function(e) {
    message("Missing fews columns")
  })
  
  if(fcs_column_name %in% names(.dataset)){
    fcs_categories <- c(fcs_categories_acceptable, fcs_categories_poor, fcs_categories_borderline)
    if (!any(fcs_categories %in% .dataset[[fcs_column_name]])) {
      warning("Please check if the fcs categories parameter passes is matching the values in your data")
    }
  }
  
  hhs_categories <- c(hhs_categories_none, hhs_categories_little, 
                      hhs_categories_moderate, hhs_categories_severe, hhs_categories_very_severe)
  if (!any(hhs_categories %in% .dataset[[hhs_column_name]])) {
    warning("Please check if the hhs categories parameter passes is matching the values in your data")
  }
  
  rcsi_categories <- c(rcsi_categories_low, rcsi_categories_medium, 
                       rcsi_categories_high)
  if (!any(rcsi_categories %in% .dataset[[rcsi_column_name]])) {
    warning("Please check if the rcsi categories parameter passes is matching the values in your data")
  }
  
  if(hdds_column_name %in% names(.dataset)){
    hdds_categories <- c(hdds_categories_low, hdds_categories_medium, hdds_categories_high)
    if (!any(hdds_categories %in% .dataset[[hdds_column_name]])) {
      warning("Please check if the hdds categories parameter passes is matching the values in your data")
    }
  }
  if(all(c(fcs_column_name,rcsi_column_name) %in% names(.dataset))) {
    .dataset <-.dataset %>%
      dplyr::mutate(fc_cell = dplyr::case_when(!!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low ~ 1,
                                               !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low ~ 7,
                                               !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low ~ 4,
                                               !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium ~ 2,
                                               !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium ~ 8,
                                               !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium ~ 5,
                                               !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high ~ 3,
                                               !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high ~ 9,
                                               !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high ~ 6,
                                               TRUE ~ NA_real_))
    } else if(all(c(hdds_column_name,rcsi_column_name) %in% names(.dataset))) {
      .dataset <-.dataset %>%
      dplyr::mutate(fc_cell = dplyr::case_when(!!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_low ~ 1,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_low ~ 7,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_low ~ 4,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium ~ 2,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium ~ 8,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium ~ 5,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_high ~ 3,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_high ~ 9,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_high ~ 6,
                                               TRUE ~ NA_real_))
    } else if(all(c(fcs_column_name,hhs_column_name) %in% names(.dataset))) {
      .dataset <-.dataset %>%
        dplyr::mutate(fc_cell = dplyr::case_when(!!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low ~ 1,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low ~ 7,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low ~ 4,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium ~ 2,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium ~ 8,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium ~ 5,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high ~ 3,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high ~ 9,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high ~ 6,
                                                 TRUE ~ NA_real_))
    } else if(all(c(hdds_column_name,hhs_column_name) %in% names(.dataset))) {
      .dataset <-.dataset %>%
        dplyr::mutate(fc_cell = dplyr::case_when(!!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 1,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 11,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 6,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 2,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 12,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 7,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 3,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 13,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 8,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 4,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 14,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 9,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 5,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 15,
                                                 !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 10,
                                                 TRUE ~ NA_real_))
    } else if(all(c(hdds_column_name,rcsi_column_name,hhs_column_name) %in% names(.dataset))) {
      .dataset <-.dataset %>% 
      dplyr::mutate(fc_cell = dplyr::case_when(!!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 1,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 11,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 6,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 16,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 26,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 21,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 31,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 41,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 36,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 2,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 12, 
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 7,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 17,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 27,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 22,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 32,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 42,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 37,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 3,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 13,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 8,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 18, 
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 28,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 23, 
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 33,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 43,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 38,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 4,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 14,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 9, 
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 19,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 29,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 24,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 34,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 44,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 39,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 5,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 15,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 10,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 20,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 30,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 25,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_high & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 35,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_low & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 45,
                                               !!rlang::sym(hdds_column_name) == hdds_categories_medium & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 40,
                                               TRUE ~ NA_real_))
    } else if (all(c(fcs_column_name,rcsi_column_name,hhs_column_name) %in% names(.dataset))){
      .dataset <-.dataset %>%
        dplyr::mutate(fc_cell = dplyr::case_when(!!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 1,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 11,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 6,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 16,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 26,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 21,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 31,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 41,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 36,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 2,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 12,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 7,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 17,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 27,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 22,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 32,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 42,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 37,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 3,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 13,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 8,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 18,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 28,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 23,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 33,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 43,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 38,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 4,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 14,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 9,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 19,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 29,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 24,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 34,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 44,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 39,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 5,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 15,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 10,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 20,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 30,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 25,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 35,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 45,
                                                 !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 40,
                                                 TRUE ~ NA_real_))
  }
  
  if (all(c(fcs_column_name,rcsi_column_name) %in% names(.dataset)) | 
      all(c(hdds_column_name,rcsi_column_name) %in% names(.dataset))) {
    .dataset <- .dataset %>% 
      dplyr::mutate(fc_phase = dplyr::case_when(fc_cell %in% c(1, 4) ~ "Phase 1 FC",
                                                fc_cell %in% c(2, 5, 7) ~ "Phase 2 FC", 
                                                fc_cell %in% c(3, 6, 8) ~ "Phase 3 FC",
                                                fc_cell %in% c(9) ~ "Phase 4 FC",
                                                TRUE ~ NA_character_))
   } else if(all(c(fcs_column_name,hhs_column_name) %in% names(.dataset)) | 
             all(c(hdds_column_name,hhs_column_name) %in% names(.dataset))) {
     .dataset <- .dataset %>% 
       dplyr::mutate(fc_phase = dplyr::case_when(fc_cell %in% c(1, 6) ~ "Phase 1 FC",
                                                 fc_cell %in% c(2, 3, 7, 11) ~ "Phase 2 FC", 
                                                 fc_cell %in% c(4, 8, 12, 13) ~ "Phase 3 FC",
                                                 fc_cell %in% c(5, 9, 10, 14) ~ "Phase 4 FC",
                                                 fc_cell %in% c(15) ~ "Phase 5 FC",
                                                 TRUE ~ NA_character_))
     
   } else if(all(c(fcs_column_name,rcsi_column_name,hhs_column_name) %in% names(.dataset)) | 
     all(c(fcs_column_name,rcsi_column_name,hhs_column_name) %in% names(.dataset))) {
    .dataset <- .dataset %>% 
       dplyr::mutate(fc_phase = dplyr::case_when(fc_cell %in% c(1, 6) ~ "Phase 1 FC",
                                                 fc_cell %in% c(2, 3, 7, 11, 12, 16, 17, 18, 21, 22, 26, 31, 32, 36) ~ "Phase 2 FC", 
                                                 fc_cell %in% c(4, 5, 8, 9, 13, 19, 20, 23, 24, 27, 28, 33, 34, 37, 38, 41, 42, 43) ~ "Phase 3 FC",
                                                 fc_cell %in% c(10, 14, 15, 25, 29, 35, 39, 40, 44) ~ "Phase 4 FC", 
                                                 fc_cell %in% c(30, 45) ~ "Phase 5 FC",
                                                 TRUE ~ NA_character_))
  }
  return(.dataset)
}


