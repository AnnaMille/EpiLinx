library("readr")

test_long_2021 <- read_csv2("data-raw/test_long_2021.csv")
usethis::use_data(test_long_2021, overwrite = TRUE)

