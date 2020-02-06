setwd("~/GitHub/LongMortalityTables/data-raw/2015VBT/2015VBT_Unismoke_ANB")

library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

#You need to set your working directory to the file with all of the excel files
file.list <- list.files(path = ".", pattern='*.xlsx')

#Initialize lists with elements for each select/ultimate table in each excel file
all_select <- vector(mode="list", length=length(file.list))
all_ultimate <- vector(mode="list", length=length(file.list))

#Iterate through files
for(i in seq_along(file.list)){
  #Read in table name
  table_name <- read_excel(path = file.list[i], col_names = FALSE, range = "B1") %>% unlist()
  #Extract gender from table name
  gender <- case_when(
    str_detect(table_name, "Male") ~ "Male",
    str_detect(table_name, "Female") ~ "Female"
  )

  #Table identifier from SOA
  table <- substr(file.list[i], 1, nchar(file.list[i])-5)

  all_select[[i]] <- read_excel(path = file.list[i], col_names = TRUE, range = "A24:Z120") %>%
    rename("issue_age" = "Row\\Column") %>%
    gather("duration", "q_sel", -issue_age) %>%
    mutate(table = table, gender = gender, tobacco = "Unismoke") %>%
    select(table, gender, tobacco, everything())

  all_ultimate[[i]] <- read_excel(path = file.list[i], col_names = TRUE, range = "A134:B255") %>%
    rename("attained_age" = "Row\\Column", "q_ult" = "1") %>%
    mutate(table = table, gender = gender, tobacco = "Unismoke") %>%
    select(table, gender, tobacco, everything())
}

#Collapse the select mortalities into a single data frame
VBT2015_ANB_Unismoke_Select <- bind_rows(all_select) %>%
  mutate(issue_age = as.integer(issue_age), duration = as.integer(duration), q_sel = as.double(q_sel)) %>%
  arrange(table, issue_age, duration)

#Collapse the ultimate mortalities into a single data frame
VBT2015_ANB_Unismoke_Ultimate <- bind_rows(all_ultimate) %>%
  mutate(attained_age = as.integer(attained_age), q_ult = as.double(q_ult))

usethis::use_data(VBT2015_ANB_Unismoke_Select)
usethis::use_data(VBT2015_ANB_Unismoke_Ultimate)
readr::write_csv(VBT2015_ANB_Unismoke_Select, "~/GitHub/LongMortalityTables/final-data-csv/2015VBT/2015VBT_Unismoke_ANB/2015VBT_Unismoke_ANB_Select.csv")
readr::write_csv(VBT2015_ANB_Unismoke_Ultimate, "~/GitHub/LongMortalityTables/final-data-csv/2015VBT/2015VBT_Unismoke_ANB/2015VBT_Unismoke_ANB_Ultimate.csv")

##Function called in combineSelectUltimate
makeRange <- function(max_age, table_list){
  issue_age = data.frame(issue_age = 0:max_age, dummy = TRUE)
  attained_age = data.frame(attained_age = 0:max_age, dummy = TRUE)

  dplyr::inner_join(issue_age, attained_age, by = "dummy") %>%
    dplyr::filter(attained_age >= issue_age) %>%
    mutate(duration = attained_age - issue_age + 1) %>%
    inner_join(table_list, by = "dummy")
}

##create the combined select and ultimate
combineSelectUltimate <- function(select, ultimate) {
  #This is joined to our range to produce ranges of all issue age/duration combinations for each table
  #May need to update aggregated quantities depending on table structure
  table_list <- ultimate %>%
    group_by(table) %>%
    summarise(gender = max(gender), tobacco = max(tobacco)) %>%
    mutate(dummy=TRUE)

  #Make range joins the table_list, may need to change the first argument to something other than 120
  combined_frame <- makeRange(120, table_list)

  #Join the select and ultimate tables to our frame
  #Join criteria vary by table basis
  combined_frame %>%
    left_join(select, by = c("table", "gender", "tobacco", "issue_age", "duration")) %>%
    left_join(ultimate, by = c("table", "gender", "tobacco", "attained_age")) %>%
    select(-dummy) %>%
    arrange(table, issue_age, duration)
}

VBT2015_ANB_Unismoke_Combined <- combineSelectUltimate(VBT2015_ANB_Unismoke_Select, VBT2015_ANB_Unismoke_Ultimate)

usethis::use_data(VBT2015_ANB_Unismoke_Combined)
readr::write_csv(VBT2015_ANB_Unismoke_Combined, "~/GitHub/LongMortalityTables/final-data-csv/2015VBT/2015VBT_Unismoke_ANB/2015VBT_Unismoke_ANB_Combined.csv")

