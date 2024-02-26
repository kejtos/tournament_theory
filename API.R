################################################################################################################################################################################################
################################################################################################################################################################################################
####################################################################################  LOADING PACKAGES #########################################################################################
################################################################################################################################################################################################
################################################################################################################################################################################################

library("systemfit")
library("nlme")
library("strucchange")
library("tseries")
library("lmtest")
library("urca")
library("sandwich")
library("stats")
library("aTSA")
library("FinTS")
library("bstats")
library("plm")
library("ts")
library("margins")
library("generalhoslem")
library("broom")
library("tidyverse")
library("car")
library("effects")
library("ggplot2")
library("dplyr")
library("AER")
library("stargazer")
library("DescTools")
library("aod")
library("ltm")
library("glmnet")
library("rugarch")
library("forecast")
library("fGarch")
library("scales")
library("selectiveInference")
library("ltm")
library("crch")
library("httr")
library("jsonlite")
library("purrr")
library("tidyr")
library("xlsx")
library("repurrrsive")

################################################################################################################################################################################################
################################################################################################################################################################################################
####################################################################################  API  #####################################################################################################
################################################################################################################################################################################################
################################################################################################################################################################################################

k <- 1:400 # will be used for offset
offset <- 100*k
apikey <- "fc70674b771908d3e6649cb2ee4e2043143b7564de39bd45accd4e3fed4fe70b" # my API key
rate <- rate_delay(1)  # the API has a limit of 1 access/second
attempts <- rate_backoff(3) # attempts in the case of unsuccessful access
options(timeout = 4000000) # to avoid timeouts

slow_map <- possibly(insistently(slowly(jsonlite::fromJSON, rate = rate), rate = attempts), otherwise = NA_character_) # to ensure the function is able to pick up all the data

############ looking up tournaments ###############

url_lookup_all_tournaments <- 'https://api.esportsearnings.com/v0/LookupRecentTournaments?apikey=' # start of the API access URL

all_tournaments <- paste0(url_lookup_all_tournaments, apikey, '&offset=', offset) %>% map(slow_map) # to get last 40 000 tournaments as instructed on the site

df_all_tournaments <- all_tournaments %>% 
  map(as_tibble) %>% 
  reduce(bind_rows) # to structure the data into a dataframe

############ adding comlumns ###############

df_all_tournaments$Rank_1_prize <- 0
df_all_tournaments$Rank_2_prize <- 0
df_all_tournaments$Rank_3_prize <- 0
df_all_tournaments$Rank_4_prize <- 0
df_all_tournaments$Rank_5_prize <- 0
df_all_tournaments$Rank_6_prize <- 0
df_all_tournaments$Rank_7_prize <- 0
df_all_tournaments$Rank_8_prize <- 0

############ pairing IDs of the games with their names ###############

unique_game_id <- unique(df_all_tournaments$GameId) # getting vector of unique IDs

url_lookup_id_games <- 'http://api.esportsearnings.com/v0/LookupGameById?apikey=' # start of the API access URL

all_id_games <- paste0(url_lookup_id_games, apikey, '&gameid=', unique_game_id) %>% map(slow_map) # getting names of the games

game_names <- seq(1:407) # making a vector

for (i in 1:407){ # pairing the IDs with the names
  game_names[i] <- all_id_games %>%
    pluck(i, "GameName")
}

df_unique_game_id <- data.frame(unique_game_id, game_names) # making a dataframe
colnames(df_unique_game_id) <- c("GameId", "GameName") # renaming the columns

team <- subset(df_all_tournaments, Teamplay == 1) # making a subset of team games (for the purpose of the API)
indi <- subset(df_all_tournaments, Teamplay == 0) # making a subset of individualistic games (for the purpose of the API))

############ looking up results of team tournaments ###############

url_lookup_team_results <- 'http://api.esportsearnings.com/v0/LookupTournamentTeamResultsByTournamentId?apikey=' # start of the API access URL

all_team_results <- paste0(url_lookup_team_results, apikey, '&tournamentid=', team$TournamentId) %>% map(slow_map) # getting the results

############ looking up results of individualistic tournaments ###############

url_lookup_indi_results <- 'http://api.esportsearnings.com/v0/LookupTournamentResultsByTournamentId?apikey=' # start of the API access URL

all_indi_results <- paste0(url_lookup_indi_results, apikey, '&tournamentid=', indi$TournamentId) %>% map(slow_map) # getting the results

############ adding rewards to dataframes ###############

for (j in 1:length(all_team_results)){ # a loop moving results into team dataframe
  for (i in 1:8){
    team[j, (i+8)] <- tryCatch(all_team_results[[j]]$PrizeUSD[i], error = function(e) 0)
  }
}

for (j in 1:length(all_indi_results)){ # a loop moving results into indi dataframe
  for (i in 1:8){
    indi[j, (i+8)] <- tryCatch(all_indi_results[[j]]$PrizeUSD[i], error = function(e) 0)
  }
}

############ saving data ###############

write.xlsx(df_all_tournaments, "C:/Users/Honzík/Desktop/Diplomová práce/all_tournaments.xlsx") # saving data
write.xlsx(df_unique_game_id, "C:/Users/Honzík/Desktop/Diplomová práce/id_gamenames.xlsx") # saving data
write.xlsx(team, "C:/Users/Honzík/Desktop/Diplomová práce/all_team_results.xlsx") # saving data for teams
write.xlsx(indi, "C:/Users/Honzík/Desktop/Diplomová práce/all_indi_results.xlsx") # saving data