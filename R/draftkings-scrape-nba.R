rm(list = ls())


####Read in Libraries####
library(tidyverse)
library(data.table)
library(rvest)
library(jsonlite)
library(openssl)
library(httr)
library(magrittr)
library(rlist)


####Helper Functions####
flatten_weird_list <- function(listy){
  final_list <-
    listy %>% 
    map(~unlist(.) %>%
          as.list() %>%
          as.data.frame()) %>%
    bind_rows()
  return(final_list)
}


####Start Draft Kings Scrape####
##Getting NFL Contests
nfl_contests_url <- "https://www.draftkings.com/lobby/getcontests?sport=NBA"
contest_data_raw <- 
  nfl_contests_url %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  httr::content(as = "text") %>% 
  jsonlite::fromJSON()  

contest_data_raw <- contest_data_raw$Contests 
contest_data <- 
  contest_data_raw %>% 
  select(n, crownAmount, gameType, id, fpp, mec, po, sdstring, nt, m, dg) %>% 
  rename("contest_name" = "n",
         "entry_fee" = "fpp", 
         "entry_max" = "mec", 
         "prizeAmount" = "po", 
         "start_time" = "sdstring", 
         "total_entries" = "nt",
         "max_total_entries" = "m",
         "draft_group" = "dg")
filtered_contest_data <- 
  contest_data %>% 
  filter(gameType == "Classic", 
         entry_fee <= 15,
         entry_fee > 0) %>% 
  mutate(entry_fee_to_total_prize_ratio = prizeAmount/entry_fee) %>% 
  filter(entry_fee_to_total_prize_ratio >= 1000) %>% 
  filter(entry_max > 1) %>% 
  arrange(-entry_fee_to_total_prize_ratio) %>% 
  slice(1:20)

##More Contest Details
contest_specific_base_url <- "https://api.draftkings.com/contests/v1/contests/CONTEST_ID?format=json"
contest_specific_urls <- map_chr(filtered_contest_data$id %>% 
                                   as.character(), 
                                 ~str_replace_all(contest_specific_base_url, "CONTEST_ID", .x))

##Draft Group Players
draft_group_base_url <- "https://api.draftkings.com/draftgroups/v1/draftgroups/DRAFT_GROUP/draftables"
draft_group_urls <- 
  contest_data %>%
  pull(draft_group) %>% 
  unique() %>% 
  as.character() %>% 
  map_chr(~str_replace_all(draft_group_base_url, "DRAFT_GROUP", .x) %>% 
            paste0(".json"))
draft_group_ids <- 
  contest_data %>% 
  pull(draft_group) %>% 
  unique()
httr::set_config(httr::config(ssl_verifypeer=0L))
draft_group_data <- 
  draft_group_urls %>% 
  map(~content(httr::GET(.)))
names(draft_group_data) <- draft_group_ids
httr::set_config(httr::config(ssl_verifypeer=1L))
draftable_players <- 
  draft_group_data %>% 
  map(~{ .[[1]] }) %>%
  list.clean(function(x) length(x) == 0L) %>%
  map(~flatten_weird_list(.) %>% 
        group_by(playerId) %>% 
        slice(1) %>% 
        ungroup())
competitions <- 
  draft_group_data %>% 
  map(~{ .[[2]] }) %>%
  map(~flatten_weird_list(.)) %>% 
  bind_rows()
# names(draftable_players) <- draft_group_ids
draftable_players_final <- 
  draftable_players %>% 
  bind_rows(.id = "draft_group")

##Save Data
fwrite(draftable_players_final, file = paste0("data/", Sys.Date(), "_dk_draftable_players.csv"))
fwrite(filtered_contest_data, file = paste0("data/", Sys.Date(), "_dk_contest_data.csv"))
fwrite(contest_data, file = paste0("data/", Sys.Date(), "_dk_full_contest_data.csv"))






