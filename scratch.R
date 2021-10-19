

library(cgmr)

filepath = system.file("extdata", package = "cgmr", "Firstname3Lastname3_nutrisense.csv")
firstline <- readLines(con = filepath, 1) %>%
  str_split(pattern = ",", simplify = TRUE)
firstline[2]
