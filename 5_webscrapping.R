library(dplyr)
library(rvest)

table <- read_html('http://ev-sales.blogspot.com/2020/01/netherlands-december-2019.html') %>%
  html_table(header = T) %>% .[[1]]

