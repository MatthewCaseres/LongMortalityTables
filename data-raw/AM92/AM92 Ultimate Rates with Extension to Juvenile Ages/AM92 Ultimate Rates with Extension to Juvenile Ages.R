setwd("~/GitHub/LongMortalityTables/data-raw/AM92/AM92 Ultimate Rates with Extension to Juvenile Ages")

AM92_Ultimate_Juvenile_Extension <- read_excel(path = "t2513.xlsx", col_names = TRUE, range = "A24:C145")

usethis::use_data(AM92_Ultimate_Juvenile_Extension)
