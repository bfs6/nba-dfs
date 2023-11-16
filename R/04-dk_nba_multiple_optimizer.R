rm(list = ls())

####Read in Libraries####
library(data.table)
library(tidyverse)
library(Hmisc)
library(lpSolve)
library(lpSolveAPI)
library(janitor)
library(fuzzyjoin)
library(coach)


####Source Other Scripts####
source("R/01-scrape_player_projections.R")
source("R/02-draftkings_scrape_nba.R")


####Read in Data####
# ##DK Salaries
# dk_salaries_raw <- 
#   "data-raw/DKSalaries.csv" %>% 
#   fread(sep = ",", header = TRUE, stringsAsFactors = FALSE, fill = TRUE) %>% 
#   select_if(~ ! all(is.na(.)))
# 
# col_names_row <- which(dk_salaries_raw == "Position")
# 
# dk_salaries <- 
#   dk_salaries_raw %>% 
#   row_to_names(row_number = col_names_row) %>% 
#   as.data.frame()
# 
# fwrite(dk_salaries, paste0("data/", Sys.Date(), "_", "DKSalaries.csv"), row.names = FALSE, sep = ",")
# 
# dk_salaries_clean <- read_dk(paste0("data/", Sys.Date(), "_", "DKSalaries.csv"))

##Read in Projections
fullDF <- fread(paste0("data/", Sys.Date(), "_points_projections.csv"), stringsAsFactors = FALSE, sep = ",")
fullDF_number_fire <- fread(paste0("data/", Sys.Date(), "_points_projections_number_fire.csv"), stringsAsFactors = FALSE, sep = ",")
fullDF_draft_edge <- fread(paste0("data/", Sys.Date(), "_points_projections_draft_edge.csv"), stringsAsFactors = FALSE, sep = ",")

##Read in DraftKings Data
dk_player_data_files <- 
  "data" %>% 
  list.files() %>% 
  str_subset("dk_draftable_players.csv")

latest_draftable_data_file  <- 
  dk_player_data_files %>% 
  map_chr(~str_replace_all(., "_dk_draftable_players.csv", "")) %>% 
  as.Date() %>% 
  max()

dk_player_data <-
  paste0("data/", latest_draftable_data_file, "_dk_draftable_players.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names()

##Merge Number Fire Data and Clean Up for Other Merge
dk_competitions <- fread(paste0("data/", latest_draftable_data_file, "_dk_contest_data.csv"), 
                         sep = ",", header = TRUE, stringsAsFactors = FALSE)
full_dk_competitions <- 
  paste0("data/", latest_draftable_data_file, "_dk_full_contest_data.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  filter(gameType != "Showdown Captain Mode") %>% 
  as.data.frame()

comp_id <- full_dk_competitions$id[1]
# comp_id <- 123141959
comp_to_join <- 
  full_dk_competitions %>% 
  filter(id == comp_id)


####Clean and Merge Data####
##Create Master Dataset
draft_group_players <- 
  dk_player_data %>% 
  filter(draft_group == comp_to_join$draft_group) %>% 
  pivot_longer(cols = c("display_name", "short_name"), 
               names_to = "name_type", 
               values_to = "name") %>% 
  stringdist_left_join(fullDF %>% 
                         mutate(team = ifelse(team == "PHO", "PHX", team),
                                team = ifelse(team == "SA", "SAS", team),
                                team = ifelse(team == "NY", "NYK", team),
                                team = ifelse(team == "NO", "NOP", team)) %>% 
                         filter(team != "") %>% 
                         dplyr::select(name, team, dk_fp_projected, fp_avg, rest, start),
                       by = c(name = "name", team_abbreviation = "team")) %>% 
  rename("name" = "name.x") %>% 
  dplyr::select(-c("name.y", "team")) %>% 
  stringdist_left_join(fullDF_number_fire %>% 
                         dplyr::select(full_name, fp) %>% 
                         rename("number_fire_fp_projected" = "fp"),
                       by = c(name = "full_name")) %>% 
  dplyr::select(-c("full_name")) %>% 
  stringdist_left_join(fullDF_draft_edge %>% 
                         dplyr::select(nba_id, player_name, team_name, proj, ceil) %>% 
                         rename("draft_edge_fp_projected" = "proj",
                                "draft_edge_fp_ceiling" = "ceil"),
                       by = c(name = "player_name", team_abbreviation = "team_name")) %>% 
  dplyr::select(-c("player_name", "team_name")) %>% 
  group_by(player_id) %>% 
  arrange(dk_fp_projected, .by_group = TRUE) %>% 
  fill(c("dk_fp_projected", "fp_avg", "rest", "start"), .direction = "downup") %>% 
  arrange(number_fire_fp_projected, .by_group = TRUE) %>% 
  fill("number_fire_fp_projected", .direction = "downup") %>% 
  arrange(draft_edge_fp_projected, .by_group = TRUE) %>% 
  fill(c("draft_edge_fp_projected", "draft_edge_fp_ceiling", "nba_id"), .direction = "downup") %>% 
  ungroup() %>%
  pivot_wider(names_from = "name_type", values_from = "name") 

##Get Data in Model Format
fpts_proj_source_names <- 
  draft_group_players %>% 
  dplyr::select(ends_with("_fp_projected")) %>% 
  names() %>% 
  sort() %>% 
  str_replace_all("dk", "daily_fantasy_fuel") %>% 
  str_replace_all("_fp_projected", "") %>% 
  str_replace_all("_", " ") %>% 
  str_to_title()

dk_model_data_clean <- 
  draft_group_players %>% 
  rename("extra_player_id" = "player_id", 
         "player_id" = "draftable_id", 
         "player" = "display_name",
         "team" = "team_abbreviation",
         "fpts_avg" = "draft_stat_attributes_value") %>% 
  mutate(opp_team = 
           competitions_name %>% 
           str_replace_all(team, "") %>% 
           str_replace_all(" @ | vs | vs[.] ", "") %>% 
           trimws(which = "both"),
         location = 
           competitions_name %>% 
           str_replace_all("[:alpha:][:alpha:][:alpha:] @ | vs[.] [:alpha:][:alpha:][:alpha:]| vs [:alpha:][:alpha:][:alpha:]", "") %>% 
           trimws(which = "both")) %>% 
  dplyr::select(c("player_id", "player", "team", "opp_team", "location", "position", "salary", "fpts_avg", ends_with("_fp_projected"))) %>% 
  separate_longer_delim(position, delim = "/") %>% 
  arrange(team, -salary, player) %>%
  rownames_to_column(var = "row_id") %>%
  mutate(row_id = as.integer(row_id),
         player_id = as.character(player_id)) %>% 
  pivot_longer(cols = ends_with("_fp_projected"), 
               names_to = "fpts_proj_source",
               values_to = "fpts_proj") %>% 
  arrange(player, position, fpts_proj_source) %>% 
  group_split(fpts_proj_source, .keep = FALSE) %>% 
  setNames(fpts_proj_source_names) %>% 
  map(~.x %>% 
        arrange(row_id) %>% 
        relocate(row_id) %>% 
        filter(is.na(fpts_proj) == FALSE) %>% 
        dplyr::select(-row_id) %>% 
        rownames_to_column(var = "row_id") %>% 
        mutate(row_id = as.integer(row_id)))


####Get Results####
##Set Number of Optimal Outcomes per Source
n_optimal_outcomes <- 10

##Run Models and Optimizer
dk_optimized_results <- 
  dk_model_data_clean %>% 
  map(~.x %>% 
        optimize_generic(model_dk_nba(.x), L = n_optimal_outcomes))

dk_optimized_results_flat <- 
  dk_optimized_results %>% 
  unlist(recursive = FALSE, use.name = TRUE)
  ##Toy Around w/ These Hyper Parameters!!! 

##Clean Up Results
dk_optimized_results_flat_names <- 
  dk_optimized_results_flat %>% 
  names() %>% 
  str_replace("Fuel", "Fuel ") %>% 
  str_replace("Edge", "Edge ") %>%  
  str_replace("Fire", "Fire ")

dk_optimized_results_final <- 
  dk_optimized_results_flat %>% 
  setNames(dk_optimized_results_flat_names) %>% 
  map(~.x %>% 
        adorn_totals("row"))

##Create Function to Look At Results
results_finder_dk_optimizer <- function(data = dk_optimized_results_final, 
                                        projection_source = c("Daily Fantasy Fuel", "Draft Edge", "Number Fire", "All"), 
                                        table_num = c(1:n_optimal_outcomes), 
                                        view = FALSE){
  
  if(exists("projection_source")){
    if(!any(projection_source %in% c("Daily Fantasy Fuel", "Draft Edge", "Number Fire", "All"))) {
      print("Incorrect Projection Source. Consider changing name of projection source")
    }
  }
  
  if(exists("table_num")){
    if(any(table_num < 1 | table_num > n_optimal_outcomes)){
      print("Incorrect Table Number. Consider changing table number specifications")
    }
  }
  
  if(exists("projection_source") & exists("table_num")){
    if(all(projection_source != "All")){
      combinations <- paste(rep(projection_source, each = length(table_num)), 
                            rep(table_num, times = length(projection_source)))
      
      custom_sort <- function(x) {
        order(as.integer(gsub("\\D", "", x)))
      }
      
      sorted_combinations <- combinations[order(custom_sort(combinations))]
      
      table_list <- 
        sorted_combinations %>% 
        map(~data[[.]] %>% 
              mutate(row_id = 
                       row_id %>% 
                       str_replace_all("Total", .x)))
      
    }
    else if(any(projection_source == "All")){
      projection_source_local <- c("Daily Fantasy Fuel", "Draft Edge", "Number Fire")
      
      combinations <- paste(rep(projection_source_local, each = length(table_num)), 
                            rep(table_num, times = length(projection_source_local)))
      
      custom_sort <- function(x) {
        order(as.integer(gsub("\\D", "", x)))
      }
      
      sorted_combinations <- combinations[order(custom_sort(combinations))]
      
      table_list <- 
        sorted_combinations %>% 
        map(~data[[.]] %>% 
              mutate(row_id = 
                       row_id %>% 
                       str_replace_all("Total", .x)))
    }
    
    if(view == TRUE){
      View(table_list)
    }else{
      print(table_list)
    }
  }
  
  if(exists("projection_source") & !exists("table_num")){
    if(all(projection_source != "All")){
      table_list <- 
        paste0(projection_source, " ", c(1:n_optimal_outcomes)) %>% 
        map(~data[[.]] %>% 
              mutate(row_id = 
                       row_id %>% 
                       str_replace_all("Total", .x)))
    }else{
      table_list <- 
        names(data) %>% 
        map(~data[[.]] %>% 
              mutate(row_id = 
                       row_id %>% 
                       str_replace_all("Total", .x)))
    }
    
    if(view == TRUE){
      table_list %>% 
        map(~View(.x))
    }else{
      print(table_list)
    }
  }
  
  if(!exists("projection_source") & exists("table_num")){
    projection_source_local <- c("Daily Fantasy Fuel", "Draft Edge", "Number Fire")
    
    combinations <- paste(rep(projection_source_local, each = length(table_num)), 
                          rep(table_num, times = length(projection_source_local)))
    
    numeric_parts <- as.integer(gsub("\\D", "", combinations))
    
    sorted_strings <- combinations[order(numeric_parts)]
    
    table_list <- 
      sorted_strings %>% 
      map(~data[[.]] %>% 
            mutate(row_id = 
                     row_id %>% 
                     str_replace_all("Total", .x)))
    
    if(view == TRUE){
      table_list %>% 
        map(~View(.x))
    }else{
      print(table_list)
    }
  }
  
}


####Run Examples of Function####
##Gets Optimal Results for All Sources
results_finder_dk_optimizer(table_num = 1)

##Gets Top 2 Results for All Sources
results_finder_dk_optimizer(table_num = c(1:2))

##Gets All Results for Draft Edge
results_finder_dk_optimizer(projection_source = "Draft Edge")

##Gets Top 4 Results for Draft Edge
results_finder_dk_optimizer(projection_source = "Draft Edge", table_num = c(1:4))

##Gets Top 3 Results for Both Draft Edge and Daily Fantasy Fuel
results_finder_dk_optimizer(projection_source = c("Draft Edge", "Daily Fantasy Fuel"), table_num = c(1:3))






