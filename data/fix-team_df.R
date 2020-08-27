load("/home/john/projects/fantasy-football/data/team_df.Rda")
team_df <-
  team_df %>%
  mutate(abbr = ifelse(team_name == 'Raiders','LV',abbr),
         city = ifelse(team_name == 'Raiders','Las Vegas',city))
View(team_df)
save(team_df,
     file="/home/john/projects/fantasy-football/data/team_df.Rda")