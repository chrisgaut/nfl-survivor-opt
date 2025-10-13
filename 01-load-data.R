
# Script 01: Load Data





# 1. Prepare Workspace ---------------------------------------------------------

# load libraries
library(tidyverse)

# load data - NFL games
nfl_games <- read_csv("data/nfl-games.csv")

# load data - picks made
picks <- read_csv("data/picks.csv")

# load data - PFF power rankings
pff_rankings <- read_csv("data/pff-power-rankings.csv")

# load custom constraints
constraints <- read_csv("data/constraints.csv",
                        col_types = list(col_character(), col_character(), col_double(), col_character()))





# 2. Prepare Game Set ----------------------------------------------------------

# Filter out completed weeks, bye weeks, picked teams and custom constraints
week_completed <- max(picks$week)
picked_teams <- picks$team

game_set <- nfl_games %>%
  filter(week > week_completed,
         opponent != "BYE",
         !team %in% picked_teams) %>%
  anti_join(constraints, by = c("team", "week"))

# filter out opponents (teams selections face) from custom constraints
game_set <- game_set %>%
  anti_join(constraints, by = c("opponent", "week"))

# filter out teams (selections) from custom constraints
game_set <- game_set %>%
  anti_join(constraints, by = c("team", "week"))

# Join power rankings for teams
game_set <- game_set %>%
  left_join(pff_rankings, by = "team") %>%
  mutate(pff_rank_team = pff_rank) %>%
  select(-pff_rank, -week_as_of)

# Join power rankings for opponents
game_set <- game_set %>%
  left_join(pff_rankings, by = c("opponent" = "team")) %>%
  mutate(pff_rank_opponent = pff_rank) %>%
  select(-pff_rank, -week_as_of)

# define "strength": difference between team and opponent PFF power rankings
game_set <- game_set %>%
  mutate(strength = pff_rank_opponent - pff_rank_team)




