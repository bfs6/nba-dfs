rm(list = ls())

####Read in Libraries####
library(data.table)
library(tidyverse)
library(rvest)
library(janitor)


####Read in Data####
##Scrape Daily Fantasy Fuel
url <- paste0("https://www.dailyfantasyfuel.com/nba/projections/draftkings/", Sys.Date())
url_res <- 
  url %>% 
  read_html() %>% 
  html_table()
proj_table <- url_res[[1]]
proj_tbl_names <- proj_table[1,]
proj_table <- proj_table[-1,]
proj_table <- proj_table[, -1]
names(proj_table) <- proj_tbl_names
row.names(proj_table) = NULL
proj_table_clean <- 
  proj_table %>% 
  clean_names()

##Scrape Number Fire
number_fire_url <- "https://www.numberfire.com/nba/daily-fantasy/daily-basketball-projections"
url_res_number_fire <- 
  number_fire_url %>% 
  read_html() %>% 
  html_table()
proj_table_number_fire <- url_res_number_fire[[4]]
proj_table_number_fire_names <- proj_table_number_fire[1, ]
proj_table_number_fire <- proj_table_number_fire[-1, ]
names(proj_table_number_fire) <- proj_table_number_fire_names
proj_table_number_fire_clean <- 
  proj_table_number_fire %>% 
  clean_names() %>% 
  mutate(player = 
           player %>% 
           str_replace_all("\\s+\\s+", "; ")) %>% 
  separate(player, c("abbrev_name", "full_name", "pos", "team"), sep = "; ")

####Write Data####
fwrite(proj_table_clean, file = paste0("data/", Sys.Date(), "_points_projections.csv"))
fwrite(proj_table_number_fire_clean, file = paste0("data/", Sys.Date(), "_points_projections_number_fire.csv"))
