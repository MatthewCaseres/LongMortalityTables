setwd("~/GitHub/LongMortalityTables/data-raw/2017CSO/CSO2017 Loaded Smoker Distinct Gender Blended ALB")

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
  #Extract gender_blend from table name
  gender_blend <- str_extract(table_name, "[0-9]*%\\s[A-Za-z]*")
  #Extract smoker status
  tobacco <- case_when(
    str_detect(table_name, "Nonsmoker") ~ "Nonsmoker",
    str_detect(table_name, "Smoker") ~ "Smoker"
  )
  #Table identifier from SOA
  table <- substr(file.list[i], 1, nchar(file.list[i])-5)

  all_select[[i]] <- read_excel(path = file.list[i], col_names = TRUE, range = "A24:Z102") %>%
    rename("issue_age" = "Row\\Column") %>%
    gather("duration", "q_sel", -issue_age) %>%
    mutate(table = table, gender_blend = gender_blend, tobacco = tobacco) %>%
    select(table, gender_blend, tobacco, everything())

  all_ultimate[[i]] <- read_excel(path = file.list[i], col_names = TRUE, range = "A116:B219") %>%
    rename("attained_age" = "Row\\Column", "q_ult" = "1") %>%
    mutate(table = table, gender_blend = gender_blend, tobacco = tobacco) %>%
    select(table, gender_blend, tobacco, everything())
}

#Collapse the select mortalities into a single data frame
CSO2017_Loaded_SmokerDistinct_GenderBlend_ALB_Select <- bind_rows(all_select) %>%
  mutate(issue_age = as.integer(issue_age), duration = as.integer(duration), q_sel = as.double(q_sel)) %>%
  arrange(table, issue_age, duration)

#Collapse the ultimate mortalities into a single data frame
CSO2017_Loaded_SmokerDistinct_GenderBlend_ALB_Ultimate <- bind_rows(all_ultimate) %>%
  mutate(attained_age = as.integer(attained_age), q_ult = as.double(q_ult))

usethis::use_data(CSO2017_Loaded_SmokerDistinct_GenderBlend_ALB_Select)
usethis::use_data(CSO2017_Loaded_SmokerDistinct_GenderBlend_ALB_Ultimate)
