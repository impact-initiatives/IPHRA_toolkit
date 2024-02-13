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
      dplyr::mutate(flag_above7_fcs = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_cereal > 7 | fcs_legumes  > 7 | fcs_dairy  > 7 | fcs_meat  > 7 | fcs_veg  > 7 | fcs_fruit  > 7 | fcs_oil  > 7 | fcs_sugar > 7, 1, 0)),
                    flag_below0_fcs = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_cereal < 0 | fcs_legumes  < 0 | fcs_dairy  < 0 | fcs_meat  < 0 | fcs_veg  < 0 | fcs_fruit  < 0 | fcs_oil  < 0 | fcs_sugar < 0, 1, 0)),
                    flag_meat_cereal_ratio = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_cereal < fcs_meat, 1, 0)),
                    flag_zero_fcs = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_cereal == 0 & fcs_legumes == 0 & fcs_dairy == 0 & fcs_meat == 0 & fcs_veg == 0 & fcs_fruit == 0 & fcs_oil == 0 & fcs_sugar == 0, 1, 0)),
                    flag_all_fcs = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_cereal == 7 & fcs_legumes == 7 & fcs_dairy == 7 & fcs_meat == 7 & fcs_veg == 7 & fcs_fruit == 7 & fcs_oil == 7 & fcs_sugar == 7, 1, 0)),
                    flag_low_cereal = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_cereal < 5, 1, 0)),
                    flag_low_oil = ifelse(is.na(fcs_cereal), NA, ifelse(fcs_oil < 5, 1, 0)),
                    flag_low_fcs = ifelse(is.na(fcs_score), NA, ifelse(fcs_score <= 10, 1, 0)),
                    flag_high_fcs = ifelse(is.na(fcs_score), NA, ifelse(fcs_score >= 56, 1, 0)),
                    flag_protein_fcs = ifelse(is.na(fcs_score), NA, ifelse(fcs_score < 21 & (fcs_meat >= 5 | fcs_dairy >= 5), 1, 0))) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(sd_foods = sd(c(fcs_cereal, fcs_legumes, fcs_dairy, fcs_meat, fcs_veg, fcs_fruit, fcs_oil, fcs_sugar), na.rm = TRUE),
                    flag_sd_foodgroup = dplyr::case_when(sd_foods < 0.8 ~ 1, TRUE ~ 0)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(fcs_flag_columns,
                    fcs_cat,
                    flag_above7_fcs,
                    flag_below0_fcs,
                    flag_meat_cereal_ratio,
                    flag_zero_fcs,
                    flag_all_fcs,
                    flag_low_cereal,
                    flag_low_oil,
                    flag_low_fcs,
                    flag_high_fcs,
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
      dplyr::mutate(flag_hhs_no = ifelse(hhs_lack_food == "no" & !is.na(hhs_lack_food_freq) |
                                           hhs_sleep_nofood	== "no" & !is.na(hhs_sleep_nofood_freq) |
                                           hhs_wholedaynight_nofood == "no" &	!is.na(hhs_wholedaynight_nofood_freq),1,0),
                    flag_severe_hhs = ifelse(is.na(hhs_score), NA, ifelse(hhs_score >= 5, 1, 0))) %>% 
      dplyr::select(hhs_flag_columns,flag_hhs_no,flag_severe_hhs)
    
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
      displ <- c() 
        # lcs_variables[which(grepl("displaced|migration|migrated",get.label(lcs_variables)))]
    )
    
    if(length(agric)>0){
      results2$flag_lcsi_liv_agriculture <- dplyr::case_when(rowSums(sapply(results2[agric], function(i) grepl("yes",i))) > 0 & as.numeric(results2[["income_types/sell_agri_prod"]]) > 0  ~ 1, TRUE ~ 0) ## Fix second part to take only select_one from three columns
    }
    
    if(length(livest)>0){
      results2$flag_lcsi_liv_livestock  <- dplyr::case_when(rowSums(sapply(results2[livest], function(i) grepl("yes",i))) > 0 & as.numeric(results2["income_types/sell_agri_prod"]) > 0 ~ 1, TRUE ~ 0) ## Fix second part to take only select_one from three columns
    }
    
    if(length(displ)>0){
      results2$flag_lcsi_displ  <- dplyr::case_when(rowSums(sapply(results2[displ], function(i) grepl("yes",i))) > 0 & results2["residency_status"] %in% c("pdi_site","pdi_fam") ~ 1, TRUE ~ 0) ## Fix second part to take only select_one from three columns
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
                                                       ifelse((hdds_score <= 2 & hdds_sugars == "yes" & hdds_condiments == "yes") | 
                                                         (hdds_score <= 1 & hdds_sugars == "yes") |
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
  results <- healthyr::calculate_plausibility_report(df = results)
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

