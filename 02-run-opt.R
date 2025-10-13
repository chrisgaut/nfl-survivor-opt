
# Script 02: Run Optimization

# TODO:
# - Allow custom constraints to filter out undesired edges





# 1. Prepare Workspace ---------------------------------------------------------

# load libraries
library(tidyverse)
library(igraph)
library(maxmatching)





# 2. Define weighted graph -----------------------------------------------------

# create edges table with formatting
edges <- game_set %>%
  mutate(from = paste0("week_", week),
         to = team,
         weight = strength) %>%
  select(from, to, weight)

# create weighted graph from edges
weighted_graph <- graph_from_data_frame(edges, directed = TRUE)





# 3. Compute optimal survivor path using maximum weight matching ---------------

# perform maximum weight matching
mwm <- maxmatching(weighted_graph, weighted = TRUE)

# extract matches
matching_vector <- mwm$matching

# convert to data frame
survivor_path <- data.frame(as.list(matching_vector))

survivor_path <- survivor_path %>%
  select(contains("week")) %>%
  pivot_longer(cols = starts_with("week"),
               names_to = "week",
               values_to = "team") %>%
  mutate(week = substr(week, 6, nchar(week)))




