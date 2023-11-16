rm(list = ls())

####Read in Libraries####
library(data.table)
library(tidyverse)
library(rvest)
library(janitor)
library(jsonlite)


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

##Scrape Draft Edge
draft_edge_url <- "https://draftedge.com/draftedge-data/nba_proj_dk.json?_=1700032809465"
df_draft_edge <- 
  draft_edge_url %>% 
  fromJSON() %>% 
  `[[`(2) %>% 
  clean_names() %>% 
  mutate(team_name = 
           name %>% 
           str_extract_all("[:alpha:][:alpha:][:alpha:][.]png") %>% 
           str_replace_all("[.]png", "") %>% 
           toupper()) %>% 
  separate(col = name, into = c("name1", "name2"), sep = "'nba', 'nba-", remove = TRUE) %>% 
  dplyr::select(-name1) %>% 
  mutate(name2 = 
           name2 %>% 
           str_replace_all("'[)]", "") %>% 
           str_replace_all('\">', "___")) %>% 
  separate(col = name2, into = c("name", "extra"), sep = "</a> <span class=", remove = TRUE) %>% 
  separate(col = extra, into = c("extra1", "gametime"), sep = "___", remove = TRUE) %>% 
  dplyr::select(-extra1) %>% 
  separate(col = gametime, into = c("gametime", "extra2"), sep = " </p>\n", remove = TRUE) %>%
  dplyr::select(-extra2) %>% 
  separate(col = name, into = c("nba_id", "player_name"), sep = "___", remove = TRUE) %>% 
  mutate(across(c("proj", "l5", "ceil", "lopp", "opp_rank"), 
                ~.x %>% 
                  str_replace_all("<span class='text-warning'>|<span class='text-danger'>|<span class='text-success'>", "") %>% 
                  str_replace_all("</span>", "") %>% 
                  str_replace_all("#", "") %>% 
                  str_replace_all("<span class='text-info'>", "") %>% 
                  as.numeric())) %>% 
  mutate(gametime = 
           gametime %>% 
           str_replace_all("@", "@ ") %>% 
           str_replace_all("vs", "vs "),
         player_name = 
           player_name %>% 
           str_replace_all("S. Gilgeous-Alexander", "Shai Gilgeous-Alexander") %>% 
           str_replace_all("J. Robinson-Earl", "Jeremiah Robinson-Earl") %>% 
           str_replace_all("N. Alexander-Walker", "Nickeil Alexander-Walker") %>% 
           str_replace_all("K. Caldwell-Pope", "Kentavious Caldwell-Pope")) %>% 
  dplyr::select(-index)


####Write Data####
date = ifelse((Sys.time() <= paste0(Sys.Date(), " 08:00:00 EST")), Sys.Date() - 1, Sys.Date())
fwrite(proj_table_clean, file = paste0("data/", Sys.Date(), "_points_projections.csv"))
fwrite(proj_table_number_fire_clean, file = paste0("data/", Sys.Date(), "_points_projections_number_fire.csv"))
fwrite(df_draft_edge, file = paste0("data/", as.Date(date), "_points_projections_draft_edge.csv"))





