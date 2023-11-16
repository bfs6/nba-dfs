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
#source("ScrapeData.R")
##Read in Projections
fullDF <- fread(paste0("data/", Sys.Date(), "_points_projections.csv"), stringsAsFactors = FALSE, sep = ",")
fullDF_number_fire <- fread(paste0("data/", Sys.Date(), "_points_projections_number_fire.csv"), stringsAsFactors = FALSE, sep = ",")
# fullDF <- filter(fullDF, avg_type == "weighted")
# fullDF$full_name <- paste(fullDF$first_name, fullDF$last_name, sep = " ")
# fullDFRefIDs <- fullDF[,c("full_name", "id")]
# fullDFRefIDs <- unique(fullDFRefIDs)

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
dk_player_data <- fread(paste0("data/", latest_draftable_data_file, "_dk_draftable_players.csv"), 
                        sep = ",", header = TRUE, stringsAsFactors = FALSE)
dk_player_data <- 
  dk_player_data %>% 
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

draft_group_players <- 
  dk_player_data %>% 
  filter(draft_group == comp_to_join$draft_group)

fullDF_clean <- 
  fullDF %>% 
  mutate(team = ifelse(team == "PHO", "PHX", team),
         team = ifelse(team == "SA", "SAS", team),
         team = ifelse(team == "NY", "NYK", team),
         team = ifelse(team == "NO", "NOP", team))%>%
  filter(team %in% unique(draft_group_players$team_abbreviation))

player_salaries <- 
  draft_group_players %>%
  group_by(player_id) %>% 
  arrange(draftable_id) %>% 
  slice(1) %>% 
  ungroup() %>%
  stringdist_left_join(fullDF_number_fire %>%
                         filter(fp > 0) %>%
                         select(full_name, fp) %>%
                         rename(fp_number_fire = fp,
                                display_name = full_name),
                       by = c(display_name = "display_name")) %>%
  select(-display_name.y) %>%
  rename(display_name = display_name.x) %>%
  pivot_longer(cols = c("display_name", "short_name"), names_to = "name_type", values_to = "player_name") %>% 
  select(any_of(c("player_name", "player_id", "salary", "team_abbreviation", "fp_number_fire"))) %>%
  rename(team = team_abbreviation, salary_num = salary) %>% 
  distinct()

##Merge Other Data and Customize Filtering
full_data <- 
  player_salaries %>% 
  mutate(player_name_matching = 
           player_name %>% 
           paste0("_", team)) %>%
  stringdist_inner_join(fullDF_clean %>% 
                         mutate(player_name_matching = 
                                  name %>% 
                                  paste0("_", team)), 
                        method = "lv",
                        by = "player_name_matching") %>% 
  mutate(name = 
           name %>% 
           str_remove_all(" Q$"),
         out = ifelse(str_detect(name, " O$") == TRUE, 1, 0)) %>% 
  filter(out == 0) %>% 
  filter(is.na(name) == FALSE) %>% 
  distinct() %>% 
  filter(player_name == name) %>% 
  rename("team" = "team.x") %>%
  select(any_of(c("player_name", "player_id", "salary_num", "pos", "dk_fp_projected", "fp_number_fire", "team"))) %>%
  pivot_longer(cols = contains("fp"), names_to = "fp_source", values_to = "fp") 
# %>% 
#   filter(player_id != 1231792)

####Function for Linear Optimal Lineup####
optimizeLineup <- function(ourPlayers, ourPlayersPrice, draftedPlayers, maxPrice, minPrice){
  ##Create Position Binary Vars
  fp_sources <- 
    full_data %>% 
    pull(fp_source) %>% 
    unique() 
  optimal_list <- list()
  for(eh in seq_along(fp_sources)){
    #print(eh)
    nba <- 
      full_data %>% 
      filter(fp_source == fp_sources[eh]) %>% 
      select(-fp_source) %>% 
      # cbind(model.matrix(~ pos + 0, full_data)) %>% 
      mutate(posPG = ifelse(str_detect(pos, "PG") == TRUE, 1, 0),
             posSG = ifelse(str_detect(pos, "SG") == TRUE, 1, 0),
             posSF = ifelse(str_detect(pos, "SF") == TRUE, 1, 0),
             posPF = ifelse(str_detect(pos, "PF") == TRUE, 1, 0),
             posC = ifelse(str_detect(pos, "C") == TRUE, 1, 0),
             posG = ifelse(str_detect(pos, "PG|SG") == TRUE, 1, 0),
             posF = ifelse(str_detect(pos, "SF|PF") == TRUE, 1, 0),
             posUTIL = 1) %>% 
      rename(id = player_id)
    nba$id <- as.character(nba$id)
    nba <- cbind(nba, model.matrix(~ id + 0, nba))
    nba$id <- as.numeric(nba$id)
    idVars <- names(nba)[which(grepl("id[[:digit:]]", names(nba)) == TRUE)]
    if(length(nba$id[which(nba$player_name %in% ourPlayers)]) == 0){
      idVarsKeep = c()
    }else{
      idVarsKeep = paste0("id", nba$id[which(nba$player_name %in% ourPlayers)])
    }
    idVarsDrop <- idVars[which(idVars %nin% idVarsKeep)]
    nba <- as.data.frame(nba)
    nba <- nba[,names(nba) %nin% idVarsDrop]
    nba <- filter(nba, is.na(fp) == FALSE)
    nba <- 
      nba %>% 
      distinct()
    # nfl$AAV_Avg[is.na(nfl$AAV_Avg)] = 1
    # nfl$MaxAAV <- nfl$AAV_Avg + sd(nfl$AAV_Avg, na.rm = T)
    if(length(draftedPlayers) > 0){
      nba <- filter(nba, player_name %nin% draftedPlayers)
    }
    
    ##Fix Actual Prices vs. AAVs
    # if(length(ourPlayers) > 0){
    #   for(i in seq_along(ourPlayers)){
    #     nba$salary_num[which(nba$player_name == ourPlayers[i])] = ourPlayersPrice[i]
    #   }
    # }
    
    
    ##Create Constraints
    if(length(ourPlayers) > 0){
      cons <- c(nba$posPG, nba$posSG, nba$posSF, nba$posPF, nba$posC, nba$posG, nba$posF, nba$posUTIL, unlist(as.list(nba[,names(nba)[which(grepl("id[[:digit:]]", names(nba)) == TRUE)]])), nba$salary_num)
      con <- matrix(cons, nrow = 9 + length(idVarsKeep), byrow = TRUE)
    }else{
      cons <- c(nba$posPG, nba$posSG, nba$posSF, nba$posPF, nba$posC, nba$posG, nba$posF, nba$posUTIL, nba$salary_num)
      con <- matrix(cons, nrow = 9, byrow = TRUE)
    }
    allcons <- rbind(con, con)
    
    ##set right hand side coefficients for both max and min
    if(length(ourPlayers) > 0){
      maxrhs <- c(3, 3, 3, 3, 2, 3, 3, 8, rep(1, length(idVarsKeep)), maxPrice) #max salary,qb,rb,wr,te,flex,def
      minrhs <- c(1, 1, 1, 1, 1, 3, 3, 8, rep(1, length(idVarsKeep)), minPrice) #min salary, #qb, rb, wr, te, flex, def
    }else{
      maxrhs <- c(2, 3, 3, 3, 2, 4, 4, 8, maxPrice) #max salary,qb,rb,wr,te,flex,def
      minrhs <- c(1, 1, 1, 1, 1, 3, 3, 8, minPrice) #min salary, #qb, rb, wr, te, flex, def
    }
    
    maxrel <- rep("<=", length(maxrhs))
    minrel <- rep(">=", length(minrhs))
    
    ##Define Final Vars and Create Optimal Lineup
    obj <- nba$fp
    obj <- 
      obj %>% 
      replace_na(0)
    rel <- c(maxrel, minrel)
    rhs <- c(maxrhs, minrhs)
    mylp <- lp("max", obj, allcons, rel, rhs, all.bin = TRUE)
    solindex <- which(mylp$solution == 1)
    optsolution <- nba[solindex,]
    optsolution <- subset(optsolution, select = c(player_name, pos, salary_num, fp, id, team))
    optsolutionLastRow <- setDT(optsolution)[, lapply(.SD, sum), .SDcols = c("salary_num", "fp")]
    optsolutionLastRow <- as.vector(do.call(c, c(rep("Total", 2), optsolutionLastRow, c("Total", "Total"))))
    optsolution <- as.data.table(rbind(as.data.frame(optsolution), optsolutionLastRow))
    optimal_list[[eh]] <- optsolution
  }
  names(optimal_list) <- fp_sources
  return(optimal_list)
}


####Test Function####
ourPlayers <- c()
ourPlayersPrice <- c()
draftedPlayers <- c()
maxPrice = 50000
minPrice = 40000
optimalLineup <- optimizeLineup(ourPlayers = ourPlayers, 
                                draftedPlayers = draftedPlayers, 
                                ourPlayersPrice = ourPlayersPrice, 
                                maxPrice = 50000, 
                                minPrice = 40000)
optimalLineup



