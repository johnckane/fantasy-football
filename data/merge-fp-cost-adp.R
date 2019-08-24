library(tidyverse)

load("/home/john/fantasy-football/data/points_week_data.Rda")
load("/home/john/fantasy-football/data/adp-data/adp.Rda")
load("/home/john/fantasy-football/data/bn-draft-data/draft_data.Rda")

colnames(points_week_data)
colnames(adp)
colnames(draft_data)
head(adp)


table(points_week_data$week)

table(draft_data$pos)
table(adp$pos)

# We want only through Week 16

table(points_week_data$week)
points_year_data <- points_week_data %>%
  filter(week <= 16) %>%
  group_by(player,year) %>%
  summarise(season_points = sum(points),
            ppg = mean(points)) %>%
  ungroup()


## We need all data, not just the draft data here. 
## So we can calculate previous lags...

  
points_adp_draft <-
  points_year_data %>%
  full_join(draft_data %>% select(player_code,pos,year,keeper,pick,adj_value), by = c("year","player" = "player_code")) %>%
  full_join(adp %>% select(player_code,year,overall,pos_adp,overall_adp), by = c("year", "player" = "player_code"))
  

colnames(points_adp_draft)
save(points_adp_draft, file = "/home/john/fantasy-football/data/points_adp_draft.Rda")

