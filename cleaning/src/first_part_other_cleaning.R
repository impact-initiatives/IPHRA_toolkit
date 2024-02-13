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

#-------------------------------------------------------------------------------
# 3) OTHERS AND TRANSLATIONS
################################################################################


other.db <- get.other.db()

other.db.main  <- other.db[other.db$name %in% colnames(raw.main),]
other.db.hh_roster  <- other.db[other.db$name %in% colnames(raw.hh_roster),]
other.db.ind_health  <- other.db[other.db$name %in% colnames(raw.ind_health),]
other.db.died_member <- other.db[other.db$name %in% colnames(raw.died_member),]

language <- strings['language_other']

if(language == "French") {
  lang <- "fr"
} else if (language == "Spanish"){
  lang <- "es"
} else {
  lang <- "ar"
} 
other.responses <- rbind(find.responses(raw.main, other.db.main, values_to = paste0("response.",lang)),
                         find.responses(raw.hh_roster, other.db.hh_roster, values_to = paste0("response.",lang),is.loop = T),
                         find.responses(raw.ind_health, other.db.ind_health, values_to = paste0("response.",lang),is.loop = T),
                         find.responses(raw.died_member, other.db.died_member, values_to = paste0("response.",lang),is.loop = T))
if(strings['api'] == "No Api"){
  other.responses.j <- other.responses %>%
    mutate(!!sym(paste0("response.",lang,".en")) := NA)
  save.other.requests(create.translate.requests(other.db, other.responses.j, is.loop = T),
                      make.short.name("other_requests"), use_template = T)
} else{
  other.responses.j <- other.responses %>% translate.responses_iphra(api_key = as.character(strings['api_key']), api = as.character(strings['api']),  values_from = paste0("response.",lang), language_codes = lang)
  
  save.other.requests(create.translate.requests(other.db, other.responses.j, is.loop = T),
                      make.short.name("other_requests"), use_template = T)
  
} 

#-----------------------------------------------------------------------------
# Translation
trans.db <- get.trans.db()


trans.responses <- find.responses(raw.died_member, trans.db, values_to = paste0("response.",lang), is.loop = T)
if(strings['api'] == "No Api"){
  trans.responses.j <- trans.responses %>%
    mutate(!!sym(paste0("response.",lang,".en")) := NA)
  save.trans.requests(create.translate.requests(trans.db, trans.responses.j, is.loop = T), make.short.name("translate_requests"), use_template = T)
} else{
  trans.responses.j <- trans.responses %>% translate.responses_iphra(api_key = as.character(strings['api_key']), api = as.character(strings['api']),  values_from = paste0("response.",lang), language_codes = lang)
  save.trans.requests(create.translate.requests(trans.db, trans.responses.j, is.loop = T), make.short.name("translate_requests"), use_template = T)
} 

svDialogs::dlg_message("Translation for both others and translations are done and a file is created in the folder output/checking/requests/ with other_requests in the title. Please check the READ_ME file for information on filling the file.", type = "ok")
