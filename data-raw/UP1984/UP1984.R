setwd("~/GitHub/LongMortalityTables/data-raw/UP1984")

UP1984 <- read_excel(path = "t831.xlsx", col_names = TRUE, range = "A24:C120")

usethis::use_data(UP1984)
