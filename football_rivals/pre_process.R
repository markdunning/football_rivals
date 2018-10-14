## Credit to https://github.com/stefangouyet/most_one_sided_EPL
require("engsoccerdata")
df <- engsoccerdata::england
library(dplyr)


#winner of game
df <- df %>% mutate(winner = case_when(
  hgoal > vgoal ~ as.character(home),
  hgoal < vgoal ~ as.character(visitor),
  TRUE ~ "Draw"
),
loser = case_when(
  hgoal < vgoal ~ as.character(home),
  hgoal > vgoal ~ as.character(visitor),
  TRUE ~ "Draw"
))

