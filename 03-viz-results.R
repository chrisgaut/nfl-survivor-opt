
# Script 03: Visualize results





# 1. Prepare Workspace ---------------------------------------------------------

# load libraries
library(tidyverse)
library(gt)





# 2. Table Formatting ----------------------------------------------------------

# combine made picks and remaining survivor path
survivor_path <- survivor_path %>%
  mutate(result = NA)

survivor_table <- survivor_path %>%
  rbind(picks) %>%
  mutate(week = as.numeric(week)) %>%
  arrange(week)

# bring in game information
survivor_table <- survivor_table %>%
  left_join(nfl_games, by = c("week", "team"))

# bring in strength for survivor path
survivor_table <- survivor_table %>%
  left_join(select(game_set, week, team, strength), by = c("week", "team"))

# reorganize columns
survivor_table <- survivor_table %>%
  select(week, team, opponent, strength)







# 3. Survivor Table Graphic ----------------------------------------------------

# table
gt_survivor_tbl <- gt(survivor_table) %>%
  tab_header(
    title = md("**NFL Survivor Table**"),
    subtitle = "The optimal remaining path to survival"
    ) %>%
  tab_source_note(
    source_note = md("*Strength* is defined as difference between the 
                     opponent's and team's PFF current power ranking")
    ) %>%
  tab_style(
    style = cell_fill(color = "#009900"),
    locations = cells_body(
      rows =  week <= week_completed
    )
    )




