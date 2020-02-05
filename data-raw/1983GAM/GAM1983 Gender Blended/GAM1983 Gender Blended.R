setwd("~/GitHub/LongMortalityTables/data-raw/1983GAM/GAM1983 Gender Blended")

library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

#You need to set your working directory to the file with all of the excel files
file.list <- list.files(path = ".", pattern='*.xlsx')

#Initialize a list with elements for each file
all_files <- vector(mode="list", length=length(file.list))

#Iterate through files
for(i in seq_along(file.list)){
  #Read in table name
  table_name <- read_excel(path = file.list[i], col_names = FALSE, range = "B1") %>% unlist()
  #Extract gender from table name
  gender <- case_when(
    str_detect(table_name, "Male") ~ "Male",
    str_detect(table_name, "Female") ~ "Female"
  )
  #Extract gender_blend from table name
  gender_blend <- str_extract(table_name, "[0-9]*%\\s[A-Za-z]*")

  #Table identifier from SOA
  table <- substr(file.list[i], 1, nchar(file.list[i])-5)

  all_files[[i]] <- read_excel(path = file.list[i], col_names = TRUE, range = "A24:B130") %>%
    rename("attained_age" = "Row\\Column", "q" = "1") %>%
    mutate(table = table, gender_blend = gender_blend) %>%
    select(table, gender_blend, everything())
}

#Collapse the mortalities into a single data frame
GAM1983_GenderBlended <- bind_rows(all_files) %>%
  mutate(attained_age = as.integer(attained_age), q = as.double(q))

usethis::use_data(GAM1983_GenderBlended)
readr::write_csv(GAM1983_GenderBlended, "~/GitHub/LongMortalityTables/final-data-csv/1983GAM/GAM1983 Gender Blended/GAM1983 Gender Blended.csv")


