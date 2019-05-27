setwd("~/GitHub/LongMortalityTables/data-raw/AM92/AM92")

AM92_Select <- read_excel(path = "t2360.xlsx", col_names = TRUE, range = "A24:D98")
AM92_Ultimate <- read_excel(path = "t2360.xlsx", col_names = TRUE, range = "A112:C214")

AM92_Select <- AM92_Select %>% mutate(attained_age = as.integer(attained_age),
                       q_sel_yr_one = as.double(q_sel_yr_one),
                       q_sel_yr_two = as.double(q_sel_yr_two))
AM92_Ultimate <- AM92_Ultimate %>% mutate(attained_age = as.integer(attained_age),
                         q_ult = as.double(q_ult))

usethis::use_data(AM92_Select)
usethis::use_data(AM92_Ultimate)
