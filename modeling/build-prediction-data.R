library(tidyverse)
load("/home/john/fantasy-football/modeling/model_data.Rda")
colnames(model_data)


base_data <- model_data %>% select(player,year,pos,adj_value,overall,pos_adp,overall_adp) %>% rename(player_code = player)

## Load 2018 ADP data
load("/home/john/fantasy-football/data/adp-data/adp_2018.Rda")
colnames(adp_2018)

## From this we need player, player_code, pos, overall
adp_2018 <- 
adp_2018 %>%
  select(player, player_code, pos, overall) %>%
  arrange(pos,overall) %>%
  group_by(pos) %>%
  mutate(pos_adp = row_number()) %>%
  ungroup() %>%
  arrange(overall) %>%
  mutate(overall_adp = row_number(),
         year = 2018)

## Get the points-year data
load("/home/john/fantasy-football/data/points_week_data.Rda")

points_year_data <-
  points_week_data %>%
  filter(week <= 16) %>%
  group_by(player,year) %>%
  summarise(season_points = sum(points),
            ppg = sum(points)/n()) %>%
 rename(player_code = player)

colnames(points_year_data)

## Load Draft Data and generate lag values for price and keeper for three years
load("/home/john/fantasy-football/data/bn-draft-data/draft_data.Rda")


  
## Load the projection data
load("/home/john/fantasy-football/data/FFA/ffa_nppr.Rda")  




players_2018 <- ffa_nppr %>% 
  mutate(proj_ppg = points/16) %>% 
  select(player_code, proj_ppg,team,age,bye) %>%
  left_join(adp_2018, by = "player_code")

players_2018 %>%
  full_join(points_year_data, by = c("player_code" = "player_code","year")) %>%
  full_join(base_data,by = c("player_code","year"))




predict_data <-  bind_rows(players_2018,
                           points_year_data %>% 
                             full_join(base_data, by = c("player_code","year")) %>%
                             full_join(draft_data %>% select("player_code","year","keeper") , by = c("player_code","year"))
                           )

## Calculate lags...
  # Season points -> 8 lags
  # ppg - > 8 lags
  # pos adp 4 lags
  # overall adp 4 lags
  # keeper 3 lag
  # draft value 3 lag
  
  predict_data <-
    predict_data %>%
    ungroup() %>%
    arrange(player_code,year) %>%
    group_by(player_code) %>%
    mutate(lag_value1 = case_when(lag(year,1) == year - 1 ~ lag(adj_value,1),
                                  TRUE ~ NA_real_),
           lag_value2 = case_when(lag(year,1) == year - 2 ~ lag(adj_value,1),
                                  lag(year,2) == year - 2 ~ lag(adj_value,2),
                                  TRUE ~ NA_real_),
           lag_value3 = case_when(lag(year,1) == year - 3 ~ lag(adj_value,1),
                                  lag(year,2) == year - 3 ~ lag(adj_value,2),
                                  lag(year,3) == year - 3 ~ lag(adj_value,3),
                                  TRUE ~ NA_real_)) %>%
    mutate(lag_keeper1 = case_when(lag(year,1) == year - 1 ~ lag(keeper,1),
                                   TRUE ~ NA_real_),
           lag_keeper2 = case_when(lag(year,1) == year - 2 ~ lag(keeper,1),
                                   lag(year,2) == year - 2 ~ lag(keeper,2),
                                   TRUE ~ NA_real_),
           lag_keeper3 = case_when(lag(year,1) == year - 3 ~ lag(keeper,1),
                                   lag(year,2) == year - 3 ~ lag(keeper,2),
                                   lag(year,3) == year - 3 ~ lag(keeper,3),
                                   TRUE ~ NA_real_)) %>%
    mutate(lag_overall1 = case_when(lag(year,1) == year - 1 ~ lag(overall,1),
                                    TRUE ~ NA_real_),
           lag_overall2 = case_when(lag(year,1) == year - 2 ~ lag(overall,1),
                                    lag(year,2) == year - 2 ~ lag(overall,2),
                                    TRUE ~ NA_real_),
           lag_overall3 = case_when(lag(year,1) == year - 3 ~ lag(overall,1),
                                    lag(year,2) == year - 3 ~ lag(overall,2),
                                    lag(year,3) == year - 3 ~ lag(overall,3),
                                    TRUE ~ NA_real_),
           lag_overall4 = case_when(lag(year,1) == year - 4 ~ lag(overall,1),
                                    lag(year,2) == year - 4 ~ lag(overall,2),
                                    lag(year,3) == year - 4 ~ lag(overall,3),
                                    lag(year,4) == year - 4 ~ lag(overall,4),
                                    TRUE ~ NA_real_)) %>%
    mutate(lag_overall_adp1 = case_when(lag(year,1) == year - 1 ~ lag(overall_adp,1),
                                        TRUE ~ as.integer(NA_real_)),
           lag_overall_adp2 = case_when(lag(year,1) == year - 2 ~ lag(overall_adp,1),
                                        lag(year,2) == year - 2 ~ lag(overall_adp,2),
                                        TRUE ~ as.integer(NA_real_)),
           lag_overall_adp3 = case_when(lag(year,1) == year - 3 ~ lag(overall_adp,1),
                                        lag(year,2) == year - 3 ~ lag(overall_adp,2),
                                        lag(year,3) == year - 3 ~ lag(overall_adp,3),
                                        TRUE ~ as.integer(NA_real_)),
           lag_overall_adp4 = case_when(lag(year,1) == year - 4 ~ lag(overall_adp,1),
                                        lag(year,2) == year - 4 ~ lag(overall_adp,2),
                                        lag(year,3) == year - 4 ~ lag(overall_adp,3),
                                        lag(year,4) == year - 4 ~ lag(overall_adp,4),
                                        TRUE ~ as.integer(NA_real_))) %>%
    mutate(lag_pos_adp1 = case_when(lag(year,1) == year - 1 ~ lag(pos_adp,1),
                                    TRUE ~ as.integer(NA_real_)),
           lag_pos_adp2 = case_when(lag(year,1) == year - 2 ~ lag(pos_adp,1),
                                    lag(year,2) == year - 2 ~ lag(pos_adp,2),
                                    TRUE ~ as.integer(NA_real_)),
           lag_pos_adp3 = case_when(lag(year,1) == year - 3 ~ lag(pos_adp,1),
                                    lag(year,2) == year - 3 ~ lag(pos_adp,2),
                                    lag(year,3) == year - 3 ~ lag(pos_adp,3),
                                    TRUE ~ as.integer(NA_real_)),
           lag_pos_adp4 = case_when(lag(year,1) == year - 4 ~ lag(pos_adp,1),
                                    lag(year,2) == year - 4 ~ lag(pos_adp,2),
                                    lag(year,3) == year - 4 ~ lag(pos_adp,3),
                                    lag(year,4) == year - 4 ~ lag(pos_adp,4),
                                    TRUE ~ as.integer(NA_real_))) %>%
    mutate(lag_ppg1 = case_when(lag(year,1) == year - 1 ~ lag(ppg,1),
                                TRUE ~ NA_real_),
           lag_ppg2 = case_when(lag(year,1) == year - 2 ~ lag(ppg,1),
                                lag(year,2) == year - 2 ~ lag(ppg,2),
                                TRUE ~ NA_real_),
           lag_ppg3 = case_when(lag(year,1) == year - 3 ~ lag(ppg,1),
                                lag(year,2) == year - 3 ~ lag(ppg,2),
                                lag(year,3) == year - 3 ~ lag(ppg,3),
                                TRUE ~ NA_real_),
           lag_ppg4 = case_when(lag(year,1) == year - 4 ~ lag(ppg,1),
                                lag(year,2) == year - 4 ~ lag(ppg,2),
                                lag(year,3) == year - 4 ~ lag(ppg,3),
                                lag(year,4) == year - 4 ~ lag(ppg,4),
                                TRUE ~ NA_real_),
           lag_ppg5 = case_when(lag(year,1) == year - 5 ~ lag(ppg,1),
                                lag(year,2) == year - 5 ~ lag(ppg,2),
                                lag(year,3) == year - 5 ~ lag(ppg,3),
                                lag(year,4) == year - 5 ~ lag(ppg,4),
                                lag(year,5) == year - 5 ~ lag(ppg,5),
                                TRUE ~ NA_real_),
           lag_ppg6 = case_when(lag(year,1) == year - 6 ~ lag(ppg,1),
                                lag(year,2) == year - 6 ~ lag(ppg,2),
                                lag(year,3) == year - 6 ~ lag(ppg,3),
                                lag(year,4) == year - 6 ~ lag(ppg,4),
                                lag(year,5) == year - 6 ~ lag(ppg,5),
                                lag(year,6) == year - 6 ~ lag(ppg,6),
                                TRUE ~ NA_real_),
           lag_ppg7 = case_when(lag(year,1) == year - 7 ~ lag(ppg,1),
                                lag(year,2) == year - 7 ~ lag(ppg,2),
                                lag(year,3) == year - 7 ~ lag(ppg,3),
                                lag(year,4) == year - 7 ~ lag(ppg,4),
                                lag(year,5) == year - 7 ~ lag(ppg,5),
                                lag(year,6) == year - 7 ~ lag(ppg,6),
                                lag(year,7) == year - 7 ~ lag(ppg,7),
                                TRUE ~ NA_real_),
           lag_ppg8 = case_when(lag(year,1) == year - 8 ~ lag(ppg,1),
                                lag(year,2) == year - 8 ~ lag(ppg,2),
                                lag(year,3) == year - 8 ~ lag(ppg,3),
                                lag(year,4) == year - 8 ~ lag(ppg,4),
                                lag(year,5) == year - 8 ~ lag(ppg,5),
                                lag(year,6) == year - 8 ~ lag(ppg,6),
                                lag(year,7) == year - 8 ~ lag(ppg,7),
                                lag(year,8) == year - 8 ~ lag(ppg,8),
                                TRUE ~ NA_real_)) %>%
    mutate(lag_season_points1 = case_when(lag(year,1) == year - 1 ~ lag(season_points,1),
                                          TRUE ~ NA_real_),
           lag_season_points2 = case_when(lag(year,1) == year - 2 ~ lag(season_points,1),
                                          lag(year,2) == year - 2 ~ lag(season_points,2),
                                          TRUE ~ NA_real_),
           lag_season_points3 = case_when(lag(year,1) == year - 3 ~ lag(season_points,1),
                                          lag(year,2) == year - 3 ~ lag(season_points,2),
                                          lag(year,3) == year - 3 ~ lag(season_points,3),
                                          TRUE ~ NA_real_),
           lag_season_points4 = case_when(lag(year,1) == year - 4 ~ lag(season_points,1),
                                          lag(year,2) == year - 4 ~ lag(season_points,2),
                                          lag(year,3) == year - 4 ~ lag(season_points,3),
                                          lag(year,4) == year - 4 ~ lag(season_points,4),
                                          TRUE ~ NA_real_),
           lag_season_points5 = case_when(lag(year,1) == year - 5 ~ lag(season_points,1),
                                          lag(year,2) == year - 5 ~ lag(season_points,2),
                                          lag(year,3) == year - 5 ~ lag(season_points,3),
                                          lag(year,4) == year - 5 ~ lag(season_points,4),
                                          lag(year,5) == year - 5 ~ lag(season_points,5),
                                          TRUE ~ NA_real_),
           lag_season_points6 = case_when(lag(year,1) == year - 6 ~ lag(season_points,1),
                                          lag(year,2) == year - 6 ~ lag(season_points,2),
                                          lag(year,3) == year - 6 ~ lag(season_points,3),
                                          lag(year,4) == year - 6 ~ lag(season_points,4),
                                          lag(year,5) == year - 6 ~ lag(season_points,5),
                                          lag(year,6) == year - 6 ~ lag(season_points,6),
                                          TRUE ~ NA_real_),
           lag_season_points7 = case_when(lag(year,1) == year - 7 ~ lag(season_points,1),
                                          lag(year,2) == year - 7 ~ lag(season_points,2),
                                          lag(year,3) == year - 7 ~ lag(season_points,3),
                                          lag(year,4) == year - 7 ~ lag(season_points,4),
                                          lag(year,5) == year - 7 ~ lag(season_points,5),
                                          lag(year,6) == year - 7 ~ lag(season_points,6),
                                          lag(year,7) == year - 7 ~ lag(season_points,7),
                                          TRUE ~ NA_real_),
           lag_season_points8 = case_when(lag(year,1) == year - 8 ~ lag(season_points,1),
                                          lag(year,2) == year - 8 ~ lag(season_points,2),
                                          lag(year,3) == year - 8 ~ lag(season_points,3),
                                          lag(year,4) == year - 8 ~ lag(season_points,4),
                                          lag(year,5) == year - 8 ~ lag(season_points,5),
                                          lag(year,6) == year - 8 ~ lag(season_points,6),
                                          lag(year,7) == year - 8 ~ lag(season_points,7),
                                          lag(year,8) == year - 8 ~ lag(season_points,8),
                                          TRUE ~ NA_real_)) %>%
    filter(year == 2018)
  
write_csv(predict_data, "/home/john/fantasy-football/modeling/prediction_data_pre.csv")
  