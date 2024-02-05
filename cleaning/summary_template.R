#######################
#Summary cleaning log
#######################

## Changed Variables
cleaning.log$variable_clean <- gsub("\\/.*","",cleaning.log$variable)
cleaning_count <- cleaning.log %>% count(variable_clean) %>% rename(observations_affected = "n")
cleaning <- cleaning.log[!duplicated(cleaning.log[c("issue", "variable_clean")]),]
cleaning <- aggregate(issue ~ variable_clean, unique(cleaning), paste, collapse = ", ")
cleaning <- cleaning %>% left_join(cleaning_count, by = "variable_clean")
cleaning.log$variable_clean <- NULL
cleaning <- cleaning %>% rename(variable = "variable_clean", action = "issue")

## Removed Variables (add or remove loops)
total_columns_rawmain <- colnames(read_xlsx("data/inputs/kobo_export/ROM_PP2_rawdata.xlsx", sheet = 1, col_types = "text")) %>% unique
total_columns_rawloop1 <- colnames(read_xlsx("data/inputs/kobo_export/ROM_PP2_rawdata.xlsx", sheet = 2, col_types = "text")) %>% unique
final_columns_rawmain <- colnames(raw.main) %>% unique
final_columns_rawloop1 <- colnames(raw.loop1) %>% unique
removed_columns_main <- setdiff(total_columns_rawmain, final_columns_rawmain)
removed_columns_loop1  <- setdiff(total_columns_rawloop1, final_columns_rawloop1)
##update the removed columns depending on number of loops
removed_columns <- c(removed_columns_main,removed_columns_loop1)
cleaning_removed <- data.frame(variable = removed_columns, action = "Columns removed", observations_affected = NA)
cleaning <- rbind(cleaning, cleaning_removed)

## Variables not changed
cleaning_untouched <- setdiff(tool.survey %>% pull(name),cleaning_count$variable_clean)
cleaning_not_changed <- data.frame(variable = cleaning_untouched, action = "Columns not changed", observations_affected = NA)
cleaning <- rbind(cleaning, cleaning_not_changed)

##Variables added
added_columns_main <- setdiff(final_columns_rawmain,total_columns_rawmain)
added_columns_loop1  <- setdiff( final_columns_rawloop1,total_columns_rawloop1)
if("uuid"%in%added_columns_main){
  if("submission_time"%in%added_columns_main){
  added_columns_main <- added_columns_main[!added_columns_main %in% c("uuid","submission_time")]
  }
}

if("uuid"%in%added_columns_loop1){
  if("parent_index"%in%added_columns_loop1){
    added_columns_loop1 <- added_columns_loop1[!added_columns_loop1 %in% c("uuid","parent_index")]
  }
}

added_columns <- c(added_columns_main,added_columns_loop1)
cleaning_added <- data.frame(variable = added_columns, action = "Columns added", observations_affected = NA)
cleaning <- rbind(cleaning, cleaning_added)

write.xlsx(cleaning, "output/cleaning_log/summary.xlsx")
