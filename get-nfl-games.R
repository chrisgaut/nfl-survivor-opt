# Pull all NFL events
library(rvest)
library(tidyverse)
library(janitor)


# Create table for teams and their abbreviations
team_abbr <- data.frame(
  team_name = c(
    "Arizona Cardinals"	
    , "Atlanta Falcons"	
    , "Baltimore Ravens"	
    , "Buffalo Bills"	
    , "Carolina Panthers"	
    , "Chicago Bears"	
    , "Cincinnati Bengals"
    , "Cleveland Browns"	
    , "Dallas Cowboys"	
    , "Denver Broncos"	
    , "Detroit Lions"	
    , "Green Bay Packers"	
    , "Houston Texans"	
    , "Indianapolis Colts"	
    , "Jacksonville Jaguars"	
    , "Kansas City Chiefs"	
    , "Las Vegas Raiders"	
    , "Los Angeles Chargers"	
    , "Los Angeles Rams"	
    , "Miami Dolphins"	
    , "Minnesota Vikings"	
    , "New England Patriots"	
    , "New Orleans Saints"	
    , "New York Giants"	
    , "New York Jets"	
    , "Philadelphia Eagles"	
    , "Pittsburgh Steelers"	
    , "San Francisco 49ers"	
    , "Seattle Seahawks"	
    , "Tampa Bay Buccaneers"	
    , "Tennessee Titans"	
    , "Washington Commanders"
  )
  , team_abbr = c(
    "ARI"	
    , "ATL"	
    , "BAL"	
    , "BUF"	
    , "CAR"	
    , "CHI"	
    , "CIN"
    , "CLE"	
    , "DAL"	
    , "DEN"	
    , "DET"	
    , "GB"	
    , "HOU"	
    , "IND"	
    , "JAX"	
    , "KC"	
    , "LV"	
    , "LAC"	
    , "LAR"	
    , "MIA"	
    , "MIN"	
    , "NE"	
    , "NO"	
    , "NYG"	
    , "NYJ"	
    , "PHI"	
    , "PIT"	
    , "SF"	
    , "SEA"	
    , "TB"	
    , "TEN"	
    , "WAS"
  )
  , pfr_team_abbr = c(
    "CRD"	
    , "ATL"	
    , "RAV"	
    , "BUF"	
    , "CAR"	
    , "CHI"	
    , "CIN"
    , "CLE"	
    , "DAL"	
    , "DEN"	
    , "DET"	
    , "GNB"	
    , "HTX"	
    , "CLT"	
    , "JAX"	
    , "KAN"	
    , "RAI"	
    , "SDG"	
    , "RAM"	
    , "MIA"	
    , "MIN"	
    , "NWE"	
    , "NOR"	
    , "NYG"	
    , "NYJ"	
    , "PHI"	
    , "PIT"	
    , "SFO"	
    , "SEA"	
    , "TAM"	
    , "OTI"	
    , "WAS"
  )
)

# FUNCTION: Pull a team's schedule
pull_team_schedule <- function(pfr_id, year) { 
  url <- paste0("https://www.pro-football-reference.com/teams/", tolower(pfr_id), "/", year, ".htm")
  tbls <- url |>
    read_html() |>
    html_nodes("table") |>
    html_table()
  sched <- tbls[[2]] |>
    row_to_names(row_number = 1) |>
    clean_names() |>
    mutate(team = pfr_id
           , home = case_when(
             x_4 == "@" ~ 0
             , opp == "Bye Week" ~ 0
             , TRUE ~ 1
           )) |>
    select(team, week, opp, home)
  return(sched)
}

# Create csv of all games
nfl_games <- data.frame(
  team = c()
  , week = c()
  , opp = c()
  , home = c()
)
for(i in 30:nrow(team_abbr)){
  pfr_team_abbr_temp <- team_abbr$pfr_team_abbr[i]
  print(pfr_team_abbr_temp)
  data_temp <- pull_team_schedule(pfr_team_abbr_temp, 2025)
  nfl_games <- bind_rows(nfl_games, data_temp)
}
nfl_games_2 <- nfl_games %>%
  left_join(select(team_abbr, team_name, team_abbr), by = c("opp" = "team_name")) |>
  mutate(opponent = team_abbr) |>
  select(-team_abbr, -opp) |>
  left_join(select(team_abbr, pfr_team_abbr, team_abbr), by = c("team" = "pfr_team_abbr")) |>
  mutate(team = team_abbr) |>
  select(team, week, opponent, home)
write_csv(nfl_games_2, 'nfl_games.csv')
