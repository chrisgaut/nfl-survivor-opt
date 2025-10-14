
# Script 03: Visualize results

# TODO:
# - Note at bottom for date in which script was ran
# - General visualization improvements
# - Another visualization that shows the entire table, and the path highlighted
# - Team logos for survivor list
# - Weighted graph visualization + project description for README





# 1. Prepare Workspace ---------------------------------------------------------

# load libraries
library(tidyverse)
library(gt)





# 2. Survivor Path Formatting --------------------------------------------------

# combine made picks and remaining survivor path
survivor_path <- survivor_df %>%
  mutate(result = "")

survivor_path <- survivor_path %>%
  rbind(picks) %>%
  mutate(week = as.numeric(week)) %>%
  arrange(week)

# bring in game information
survivor_path <- survivor_path %>%
  left_join(nfl_games, by = c("week", "team"))

# bring in strength for survivor path
survivor_path <- survivor_path %>%
  left_join(select(game_set, week, team, strength), by = c("week", "team")) %>%
  mutate(strength = replace_na(strength, ""))





# 3. Survivor Path Graphic -----------------------------------------------------

# table
gt_survivor_tbl <- gt(survivor_path) %>%
  tab_header(
    title = md("**NFL Survivor Path**"),
    subtitle = "The optimal remaining path to survival"
    ) %>%
  tab_source_note(
    source_note = md("*Strength* is defined as the difference between the<br> 
                     opponent's and team's current PFF power rankings.")
    ) %>%
  tab_style(
    style = cell_fill(color = "#009900"),
    locations = cells_body(
      rows =  week <= week_completed
    )
    ) %>%
  cols_hide(c(home, result))





# 4. Survivor Table Graphic ----------------------------------------------------