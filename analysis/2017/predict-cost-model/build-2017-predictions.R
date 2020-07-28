library(xgboost)
library(tidyverse)
library(magrittr)
library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='cz14F4b12', dbname='armchair_analysis', host='localhost')

player_info <- dbGetQuery(con = mydb,
                          "select player, yob, start from player")
load("/home/john/stats_corner/2017/predict-cost-model/model2017.Rda")


load("/home/john/stats_corner/2017/adp2017_final.Rda")
load("/home/john/stats_corner/fantasy_football/coded_adp_data_2010_2016.Rda")
coded_adp_data2$pick <- as.numeric(coded_adp_data2$pick)
# Want to get lag values of adp overall and by position
adp_data2 <-
  coded_adp_data2 %>%
  arrange(year,pick) %>%
  group_by(year) %>%
  mutate(overall_adp = row_number()) %>%
  ungroup() %>%
  arrange(year,pos,pick) %>%
  group_by(year,pos) %>%
  mutate(position_adp = row_number())

adp2017_final2 %<>% mutate(pick = as.numeric(adp2017_final2$pick))


adp_final <- bind_rows(adp2017_final2,adp_data2) %>%
  arrange(year,pick) %>%
  group_by(year) %>%
  mutate(overall_adp = row_number()) %>%
  ungroup() %>%
  arrange(year,pos,pick) %>%
  group_by(year,pos) %>%
  mutate(position_adp = row_number())


load("/home/john/stats_corner/fantasy_football/draft_data.Rda")

# point week data
load("/home/john/stats_corner/fantasy_football/points_week_data.Rda")
points_week_data %<>%
  filter(week <= 17) 

pwd_total <-
  points_week_data %>%
  group_by(player,year) %>%
  summarise(games = n(),
            ppg = mean(points),
            ttl = sum(points))



###################
colnames(adp_final)
colnames(draft_data)
colnames(pwd_total)
colnames(player_info)
no_lags <-
  adp_final %>%
  left_join(.,
            draft_data %>% filter(keeper == 0) %>% select(player_code,year,adj_value), by = c("player_code","year")) %>%
  left_join(.,
            draft_data %>% select(player_code,year,keeper), by = c("player_code","year")) %>%
  left_join(.,
            pwd_total, by = c("player_code"="player","year")) %>%
  left_join(.,
            player_info, by = c("player_code"="player"))

###################

w_lags <-
  no_lags %>%
  ungroup() %>%
  arrange(player_code,year) %>%
  group_by(player_code) %>%
  mutate(lag_cost1 = ifelse(lag(year,1) == year - 1,lag(adj_value,1), NA),
         lag_cost2 = ifelse(lag(year,1) == year - 2,
                            lag(adj_value,1),
                            ifelse(lag(year,2) == year - 2, 
                                   lag(adj_value,2),
                                   NA)),
         lag_cost3 = ifelse(lag(year,1) == year - 3, 
                            lag(adj_value,1),
                            ifelse(lag(year,2) == year - 3,
                                   lag(adj_value,2),
                                   ifelse(lag(year,3) == year - 3,
                                          lag(adj_value,3),
                                          NA))),
         lag_cost4 = ifelse(lag(year,1) == year - 4,
                            lag(adj_value,1),
                            ifelse(lag(year,2) == year - 4,
                                   lag(adj_value,2),
                                   ifelse(lag(year,3) == year - 4,
                                          lag(adj_value,3),
                                          ifelse(lag(year,4) == year -  4,
                                                 lag(adj_value,4),
                                                 NA)))),
         lag_cost5 = ifelse(lag(year,1) == year - 5,
                            lag(adj_value,1),
                            ifelse(lag(year,2) == year - 5,
                                   lag(adj_value,2),
                                   ifelse(lag(year,3) == year - 5,
                                          lag(adj_value,3),
                                          ifelse(lag(year,4) == year -  5,
                                                 lag(adj_value,4),
                                                 ifelse(lag(year,5) == year - 5,
                                                        lag(adj_value,5),
                                                        NA))))),
         lag_cost6 = ifelse(lag(year,1) == year - 6,
                            lag(adj_value,1),
                            ifelse(lag(year,2) == year - 6,
                                   lag(adj_value,2),
                                   ifelse(lag(year,3) == year - 6,
                                          lag(adj_value,3),
                                          ifelse(lag(year,4) == year -  6,
                                                 lag(adj_value,4),
                                                 ifelse(lag(year,5) == year - 6,
                                                        lag(adj_value,5),
                                                        ifelse(lag(year,6) == year - 6,
                                                               lag(adj_value,6),
                                                               NA))))))) %>%
  ungroup() %>%
  arrange(player_code,year) %>%
  group_by(player_code) %>%
  mutate(lag_keep1 = ifelse(lag(year,1) == year - 1,lag(keeper,1), NA),
         lag_keep2 = ifelse(lag(year,1) == year - 2,
                            lag(keeper,1),
                            ifelse(lag(year,2) == year - 2, 
                                   lag(keeper,2),
                                   NA)),
         lag_keep3 = ifelse(lag(year,1) == year - 3, 
                            lag(keeper,1),
                            ifelse(lag(year,2) == year - 3,
                                   lag(keeper,2),
                                   ifelse(lag(year,3) == year - 3,
                                          lag(keeper,3),
                                          NA))),
         lag_keep4 = ifelse(lag(year,1) == year - 4,
                            lag(keeper,1),
                            ifelse(lag(year,2) == year - 4,
                                   lag(keeper,2),
                                   ifelse(lag(year,3) == year - 4,
                                          lag(keeper,3),
                                          ifelse(lag(year,4) == year -  4,
                                                 lag(keeper,4),
                                                 NA)))),
         lag_keep5 = ifelse(lag(year,1) == year - 5,
                            lag(keeper,1),
                            ifelse(lag(year,2) == year - 5,
                                   lag(keeper,2),
                                   ifelse(lag(year,3) == year - 5,
                                          lag(keeper,3),
                                          ifelse(lag(year,4) == year -  5,
                                                 lag(keeper,4),
                                                 ifelse(lag(year,5) == year - 5,
                                                        lag(keeper,5),
                                                        NA))))),
         lag_keep6 = ifelse(lag(year,1) == year - 6,
                            lag(keeper,1),
                            ifelse(lag(year,2) == year - 6,
                                   lag(keeper,2),
                                   ifelse(lag(year,3) == year - 6,
                                          lag(keeper,3),
                                          ifelse(lag(year,4) == year -  6,
                                                 lag(keeper,4),
                                                 ifelse(lag(year,5) == year - 6,
                                                        lag(keeper,5),
                                                        ifelse(lag(year,6) == year - 6,
                                                               lag(keeper,6),
                                                               NA))))))) %>%
  ungroup() %>%
  arrange(player_code,year) %>%
  group_by(player_code) %>%
  mutate(lag_oa_adp1 = ifelse(lag(year,1) == year - 1,lag(overall_adp,1), NA),
         lag_oa_adp2 = ifelse(lag(year,1) == year - 2,
                              lag(overall_adp,1),
                              ifelse(lag(year,2) == year - 2, 
                                     lag(overall_adp,2),
                                     NA)),
         lag_oa_adp3 = ifelse(lag(year,1) == year - 3, 
                              lag(overall_adp,1),
                              ifelse(lag(year,2) == year - 3,
                                     lag(overall_adp,2),
                                     ifelse(lag(year,3) == year - 3,
                                            lag(overall_adp,3),
                                            NA))),
         lag_oa_adp4 = ifelse(lag(year,1) == year - 4,
                              lag(overall_adp,1),
                              ifelse(lag(year,2) == year - 4,
                                     lag(overall_adp,2),
                                     ifelse(lag(year,3) == year - 4,
                                            lag(overall_adp,3),
                                            ifelse(lag(year,4) == year -  4,
                                                   lag(overall_adp,4),
                                                   NA)))),
         lag_oa_adp5 = ifelse(lag(year,1) == year - 5,
                              lag(overall_adp,1),
                              ifelse(lag(year,2) == year - 5,
                                     lag(overall_adp,2),
                                     ifelse(lag(year,3) == year - 5,
                                            lag(overall_adp,3),
                                            ifelse(lag(year,4) == year -  5,
                                                   lag(overall_adp,4),
                                                   ifelse(lag(year,5) == year - 5,
                                                          lag(overall_adp,5),
                                                          NA))))),
         lag_oa_adp6 = ifelse(lag(year,1) == year - 6,
                              lag(overall_adp,1),
                              ifelse(lag(year,2) == year - 6,
                                     lag(overall_adp,2),
                                     ifelse(lag(year,3) == year - 6,
                                            lag(overall_adp,3),
                                            ifelse(lag(year,4) == year -  6,
                                                   lag(overall_adp,4),
                                                   ifelse(lag(year,5) == year - 6,
                                                          lag(overall_adp,5),
                                                          ifelse(lag(year,6) == year - 6,
                                                                 lag(overall_adp,6),
                                                                 NA)))))),
         lag_oa_adp7 = ifelse(lag(year,1) == year - 7,
                              lag(overall_adp,1),
                              ifelse(lag(year,2) == year - 7,
                                     lag(overall_adp,2),
                                     ifelse(lag(year,3) == year - 7,
                                            lag(overall_adp,3),
                                            ifelse(lag(year,4) == year -  7,
                                                   lag(overall_adp,4),
                                                   ifelse(lag(year,5) == year - 7,
                                                          lag(overall_adp,5),
                                                          ifelse(lag(year,6) == year - 7,
                                                                 lag(overall_adp,6),
                                                                 ifelse(lag(year,7) == year - 7,
                                                                        lag(overall_adp,7),
                                                                        NA))))))),
         lag_oa_adp8 = ifelse(lag(year,1) == year - 8,
                              lag(overall_adp,1),
                              ifelse(lag(year,2) == year - 8,
                                     lag(overall_adp,2),
                                     ifelse(lag(year,3) == year - 8,
                                            lag(overall_adp,3),
                                            ifelse(lag(year,4) == year -  8,
                                                   lag(overall_adp,4),
                                                   ifelse(lag(year,5) == year - 8,
                                                          lag(overall_adp,5),
                                                          ifelse(lag(year,6) == year - 8,
                                                                 lag(overall_adp,6),
                                                                 ifelse(lag(year,7) == year - 8,
                                                                        lag(overall_adp,7),
                                                                        ifelse(lag(year,8) == year - 8,
                                                                               lag(overall_adp,8),
                                                                               NA)))))))),
         lag_oa_adp9 = ifelse(lag(year,1) == year - 9,
                              lag(overall_adp,1),
                              ifelse(lag(year,2) == year - 9,
                                     lag(overall_adp,2),
                                     ifelse(lag(year,3) == year - 9,
                                            lag(overall_adp,3),
                                            ifelse(lag(year,4) == year -  9,
                                                   lag(overall_adp,4),
                                                   ifelse(lag(year,5) == year - 9,
                                                          lag(overall_adp,5),
                                                          ifelse(lag(year,6) == year - 9,
                                                                 lag(overall_adp,6),
                                                                 ifelse(lag(year,7) == year - 9,
                                                                        lag(overall_adp,7),
                                                                        ifelse(lag(year,8) == year - 9,
                                                                               lag(overall_adp,8),
                                                                               ifelse(lag(year,9) == year - 9,
                                                                                      lag(overall_adp,9),
                                                                                      NA))))))))),
         lag_oa_adp10 = ifelse(lag(year,1) == year - 10,
                               lag(overall_adp,1),
                               ifelse(lag(year,2) == year - 10,
                                      lag(overall_adp,2),
                                      ifelse(lag(year,3) == year - 10,
                                             lag(overall_adp,3),
                                             ifelse(lag(year,4) == year -  10,
                                                    lag(overall_adp,4),
                                                    ifelse(lag(year,5) == year - 10,
                                                           lag(overall_adp,5),
                                                           ifelse(lag(year,6) == year - 10,
                                                                  lag(overall_adp,6),
                                                                  ifelse(lag(year,7) == year - 10,
                                                                         lag(overall_adp,7),
                                                                         ifelse(lag(year,8) == year - 10,
                                                                                lag(overall_adp,8),
                                                                                ifelse(lag(year,9) == year - 10,
                                                                                       lag(overall_adp,9),
                                                                                       ifelse(lag(year,10) == year -10,
                                                                                              lag(overall_adp,10),
                                                                                              NA))))))))))) %>%
  mutate(lag_pos_adp1 = ifelse(lag(year,1) == year - 1,lag(position_adp,1), NA),
         lag_pos_adp2 = ifelse(lag(year,1) == year - 2,
                               lag(position_adp,1),
                               ifelse(lag(year,2) == year - 2, 
                                      lag(position_adp,2),
                                      NA)),
         lag_pos_adp3 = ifelse(lag(year,1) == year - 3, 
                               lag(position_adp,1),
                               ifelse(lag(year,2) == year - 3,
                                      lag(position_adp,2),
                                      ifelse(lag(year,3) == year - 3,
                                             lag(position_adp,3),
                                             NA))),
         lag_pos_adp4 = ifelse(lag(year,1) == year - 4,
                               lag(position_adp,1),
                               ifelse(lag(year,2) == year - 4,
                                      lag(position_adp,2),
                                      ifelse(lag(year,3) == year - 4,
                                             lag(position_adp,3),
                                             ifelse(lag(year,4) == year -  4,
                                                    lag(position_adp,4),
                                                    NA)))),
         lag_pos_adp5 = ifelse(lag(year,1) == year - 5,
                               lag(position_adp,1),
                               ifelse(lag(year,2) == year - 5,
                                      lag(position_adp,2),
                                      ifelse(lag(year,3) == year - 5,
                                             lag(position_adp,3),
                                             ifelse(lag(year,4) == year -  5,
                                                    lag(position_adp,4),
                                                    ifelse(lag(year,5) == year - 5,
                                                           lag(position_adp,5),
                                                           NA))))),
         lag_pos_adp6 = ifelse(lag(year,1) == year - 6,
                               lag(position_adp,1),
                               ifelse(lag(year,2) == year - 6,
                                      lag(position_adp,2),
                                      ifelse(lag(year,3) == year - 6,
                                             lag(position_adp,3),
                                             ifelse(lag(year,4) == year -  6,
                                                    lag(position_adp,4),
                                                    ifelse(lag(year,5) == year - 6,
                                                           lag(position_adp,5),
                                                           ifelse(lag(year,6) == year - 6,
                                                                  lag(position_adp,6),
                                                                  NA)))))),
         lag_pos_adp7 = ifelse(lag(year,1) == year - 7,
                               lag(position_adp,1),
                               ifelse(lag(year,2) == year - 7,
                                      lag(position_adp,2),
                                      ifelse(lag(year,3) == year - 7,
                                             lag(position_adp,3),
                                             ifelse(lag(year,4) == year -  7,
                                                    lag(position_adp,4),
                                                    ifelse(lag(year,5) == year - 7,
                                                           lag(position_adp,5),
                                                           ifelse(lag(year,6) == year - 7,
                                                                  lag(position_adp,6),
                                                                  ifelse(lag(year,7) == year - 7,
                                                                         lag(position_adp,7),
                                                                         NA))))))),
         lag_pos_adp8 = ifelse(lag(year,1) == year - 8,
                               lag(position_adp,1),
                               ifelse(lag(year,2) == year - 8,
                                      lag(position_adp,2),
                                      ifelse(lag(year,3) == year - 8,
                                             lag(position_adp,3),
                                             ifelse(lag(year,4) == year -  8,
                                                    lag(position_adp,4),
                                                    ifelse(lag(year,5) == year - 8,
                                                           lag(position_adp,5),
                                                           ifelse(lag(year,6) == year - 8,
                                                                  lag(position_adp,6),
                                                                  ifelse(lag(year,7) == year - 8,
                                                                         lag(position_adp,7),
                                                                         ifelse(lag(year,8) == year - 8,
                                                                                lag(position_adp,8),
                                                                                NA)))))))),
         lag_pos_adp9 = ifelse(lag(year,1) == year - 9,
                               lag(position_adp,1),
                               ifelse(lag(year,2) == year - 9,
                                      lag(position_adp,2),
                                      ifelse(lag(year,3) == year - 9,
                                             lag(position_adp,3),
                                             ifelse(lag(year,4) == year -  9,
                                                    lag(position_adp,4),
                                                    ifelse(lag(year,5) == year - 9,
                                                           lag(position_adp,5),
                                                           ifelse(lag(year,6) == year - 9,
                                                                  lag(position_adp,6),
                                                                  ifelse(lag(year,7) == year - 9,
                                                                         lag(position_adp,7),
                                                                         ifelse(lag(year,8) == year - 9,
                                                                                lag(position_adp,8),
                                                                                ifelse(lag(year,9) == year - 9,
                                                                                       lag(position_adp,9),
                                                                                       NA))))))))),
         lag_pos_adp10 = ifelse(lag(year,1) == year - 10,
                                lag(position_adp,1),
                                ifelse(lag(year,2) == year - 10,
                                       lag(position_adp,2),
                                       ifelse(lag(year,3) == year - 10,
                                              lag(position_adp,3),
                                              ifelse(lag(year,4) == year -  10,
                                                     lag(position_adp,4),
                                                     ifelse(lag(year,5) == year - 10,
                                                            lag(position_adp,5),
                                                            ifelse(lag(year,6) == year - 10,
                                                                   lag(position_adp,6),
                                                                   ifelse(lag(year,7) == year - 10,
                                                                          lag(position_adp,7),
                                                                          ifelse(lag(year,8) == year - 10,
                                                                                 lag(position_adp,8),
                                                                                 ifelse(lag(year,9) == year - 10,
                                                                                        lag(position_adp,9),
                                                                                        ifelse(lag(year,10) == year -10,
                                                                                               lag(position_adp,10),
                                                                                               NA))))))))))) %>%
  ungroup() %>%
  arrange(player_code,year) %>%
  group_by(player_code) %>%
  mutate(lag_ppg1 = ifelse(lag(year,1) == year - 1,lag(ppg,1), NA),
         lag_ppg2 = ifelse(lag(year,1) == year - 2,
                           lag(ppg,1),
                           ifelse(lag(year,2) == year - 2, 
                                  lag(ppg,2),
                                  NA)),
         lag_ppg3 = ifelse(lag(year,1) == year - 3,
                           lag(ppg,1),
                           ifelse(lag(year,2) == year - 3,
                                  lag(ppg,2),
                                  ifelse(lag(year,3) == year -3,
                                         lag(ppg,3),
                                         NA))),
         lag_ppg4 = ifelse(lag(year,1) == year - 4,
                           lag(ppg,1),
                           ifelse(lag(year,2) == year - 4,
                                  lag(ppg,2),
                                  ifelse(lag(year,3) == year - 4,
                                         lag(ppg,3),
                                         ifelse(lag(year,4) == year - 4,
                                                lag(ppg,4),
                                                NA)))),
         lag_ppg5 = ifelse(lag(year,1) == year - 5,
                           lag(ppg,1),
                           ifelse(lag(year,2) == year - 5,
                                  lag(ppg,2),
                                  ifelse(lag(year,3) == year - 5,
                                         lag(ppg,3),
                                         ifelse(lag(year,4) == year - 5,
                                                lag(ppg,4),
                                                ifelse(lag(year,5) == year - 5,
                                                       lag(ppg,5),
                                                       NA))))),
         lag_ppg6 = ifelse(lag(year,1) == year - 6,
                           lag(ppg,1),
                           ifelse(lag(year,2) == year - 6,
                                  lag(ppg,2),
                                  ifelse(lag(year,3) == year - 6,
                                         lag(ppg,3),
                                         ifelse(lag(year,4) == year - 6,
                                                lag(ppg,4),
                                                ifelse(lag(year,5) == year - 6,
                                                       lag(ppg,5),
                                                       ifelse(lag(year,6) == year - 6,
                                                              lag(ppg,6),
                                                              NA)))))),
         lag_ppg7 = ifelse(lag(year,1) == year - 7,
                           lag(ppg,1),
                           ifelse(lag(year,2) == year - 7,
                                  lag(ppg,2),
                                  ifelse(lag(year,3) == year - 7,
                                         lag(ppg,3),
                                         ifelse(lag(year,4) == year - 7,
                                                lag(ppg,4),
                                                ifelse(lag(year,5) == year - 7,
                                                       lag(ppg,5),
                                                       ifelse(lag(year,6) == year - 7,
                                                              lag(ppg,6),
                                                              ifelse(lag(year,7) == year -7,
                                                                     lag(ppg,7),
                                                                     NA))))))),
         lag_ppg8 = ifelse(lag(year,1) == year - 8,
                           lag(ppg,1),
                           ifelse(lag(year,2) == year - 8,
                                  lag(ppg,2),
                                  ifelse(lag(year,3) == year - 8,
                                         lag(ppg,3),
                                         ifelse(lag(year,4) == year - 8,
                                                lag(ppg,4),
                                                ifelse(lag(year,5) == year - 8,
                                                       lag(ppg,5),
                                                       ifelse(lag(year,6) == year - 8,
                                                              lag(ppg,6),
                                                              ifelse(lag(year,7) == year - 8,
                                                                     lag(ppg,7),
                                                                     ifelse(lag(year,8) == year - 8,
                                                                            lag(ppg,8),
                                                                            NA)))))))),
         lag_ppg9 = ifelse(lag(year,1) == year - 9,
                           lag(ppg,1),
                           ifelse(lag(year,2) == year - 9,
                                  lag(ppg,2),
                                  ifelse(lag(year,3) == year - 9,
                                         lag(ppg,3),
                                         ifelse(lag(year,4) == year - 9,
                                                lag(ppg,4),
                                                ifelse(lag(year,5) == year - 9,
                                                       lag(ppg,5),
                                                       ifelse(lag(year,6) == year - 9,
                                                              lag(ppg,6),
                                                              ifelse(lag(year,7) == year - 9,
                                                                     lag(ppg,7),
                                                                     ifelse(lag(year,8) == year - 9,
                                                                            lag(ppg,8),
                                                                            ifelse(lag(year,9) == year - 9,
                                                                                   lag(ppg,9),
                                                                                   NA))))))))),
         lag_ppg10 = ifelse(lag(year,1) == year - 10,
                            lag(ppg,1),
                            ifelse(lag(year,2) == year - 10,
                                   lag(ppg,2),
                                   ifelse(lag(year,3) == year - 10,
                                          lag(ppg,3),
                                          ifelse(lag(year,4) == year - 10,
                                                 lag(ppg,4),
                                                 ifelse(lag(year,5) == year - 10,
                                                        lag(ppg,5),
                                                        ifelse(lag(year,6) == year - 10,
                                                               lag(ppg,6),
                                                               ifelse(lag(year,7) == year - 10,
                                                                      lag(ppg,7),
                                                                      ifelse(lag(year,8) == year - 10,
                                                                             lag(ppg,8),
                                                                             ifelse(lag(year,9) == year - 10,
                                                                                    lag(ppg,9),
                                                                                    ifelse(lag(year, 10) == year - 10,
                                                                                           lag(ppg,10),
                                                                                           NA))))))))))
  ) %>%
  mutate(lag_ttl1 = ifelse(lag(year,1) == year - 1,lag(ttl,1), NA),
         lag_ttl2 = ifelse(lag(year,1) == year - 2,
                           lag(ttl,1),
                           ifelse(lag(year,2) == year - 2, 
                                  lag(ttl,2),
                                  NA)),
         lag_ttl3 = ifelse(lag(year,1) == year - 3,
                           lag(ttl,1),
                           ifelse(lag(year,2) == year - 3,
                                  lag(ttl,2),
                                  ifelse(lag(year,3) == year -3,
                                         lag(ttl,3),
                                         NA))),
         lag_ttl4 = ifelse(lag(year,1) == year - 4,
                           lag(ttl,1),
                           ifelse(lag(year,2) == year - 4,
                                  lag(ttl,2),
                                  ifelse(lag(year,3) == year - 4,
                                         lag(ttl,3),
                                         ifelse(lag(year,4) == year - 4,
                                                lag(ttl,4),
                                                NA)))),
         lag_ttl5 = ifelse(lag(year,1) == year - 5,
                           lag(ttl,1),
                           ifelse(lag(year,2) == year - 5,
                                  lag(ttl,2),
                                  ifelse(lag(year,3) == year - 5,
                                         lag(ttl,3),
                                         ifelse(lag(year,4) == year - 5,
                                                lag(ttl,4),
                                                ifelse(lag(year,5) == year - 5,
                                                       lag(ttl,5),
                                                       NA))))),
         lag_ttl6 = ifelse(lag(year,1) == year - 6,
                           lag(ttl,1),
                           ifelse(lag(year,2) == year - 6,
                                  lag(ttl,2),
                                  ifelse(lag(year,3) == year - 6,
                                         lag(ttl,3),
                                         ifelse(lag(year,4) == year - 6,
                                                lag(ttl,4),
                                                ifelse(lag(year,5) == year - 6,
                                                       lag(ttl,5),
                                                       ifelse(lag(year,6) == year - 6,
                                                              lag(ttl,6),
                                                              NA)))))),
         lag_ttl7 = ifelse(lag(year,1) == year - 7,
                           lag(ttl,1),
                           ifelse(lag(year,2) == year - 7,
                                  lag(ttl,2),
                                  ifelse(lag(year,3) == year - 7,
                                         lag(ttl,3),
                                         ifelse(lag(year,4) == year - 7,
                                                lag(ttl,4),
                                                ifelse(lag(year,5) == year - 7,
                                                       lag(ttl,5),
                                                       ifelse(lag(year,6) == year - 7,
                                                              lag(ttl,6),
                                                              ifelse(lag(year,7) == year -7,
                                                                     lag(ttl,7),
                                                                     NA))))))),
         lag_ttl8 = ifelse(lag(year,1) == year - 8,
                           lag(ttl,1),
                           ifelse(lag(year,2) == year - 8,
                                  lag(ttl,2),
                                  ifelse(lag(year,3) == year - 8,
                                         lag(ttl,3),
                                         ifelse(lag(year,4) == year - 8,
                                                lag(ttl,4),
                                                ifelse(lag(year,5) == year - 8,
                                                       lag(ttl,5),
                                                       ifelse(lag(year,6) == year - 8,
                                                              lag(ttl,6),
                                                              ifelse(lag(year,7) == year - 8,
                                                                     lag(ttl,7),
                                                                     ifelse(lag(year,8) == year - 8,
                                                                            lag(ttl,8),
                                                                            NA)))))))),
         lag_ttl9 = ifelse(lag(year,1) == year - 9,
                           lag(ttl,1),
                           ifelse(lag(year,2) == year - 9,
                                  lag(ttl,2),
                                  ifelse(lag(year,3) == year - 9,
                                         lag(ttl,3),
                                         ifelse(lag(year,4) == year - 9,
                                                lag(ttl,4),
                                                ifelse(lag(year,5) == year - 9,
                                                       lag(ttl,5),
                                                       ifelse(lag(year,6) == year - 9,
                                                              lag(ttl,6),
                                                              ifelse(lag(year,7) == year - 9,
                                                                     lag(ttl,7),
                                                                     ifelse(lag(year,8) == year - 9,
                                                                            lag(ttl,8),
                                                                            ifelse(lag(year,9) == year - 9,
                                                                                   lag(ttl,9),
                                                                                   NA))))))))),
         lag_ttl10 = ifelse(lag(year,1) == year - 10,
                            lag(ttl,1),
                            ifelse(lag(year,2) == year - 10,
                                   lag(ttl,2),
                                   ifelse(lag(year,3) == year - 10,
                                          lag(ttl,3),
                                          ifelse(lag(year,4) == year - 10,
                                                 lag(ttl,4),
                                                 ifelse(lag(year,5) == year - 10,
                                                        lag(ttl,5),
                                                        ifelse(lag(year,6) == year - 10,
                                                               lag(ttl,6),
                                                               ifelse(lag(year,7) == year - 10,
                                                                      lag(ttl,7),
                                                                      ifelse(lag(year,8) == year - 10,
                                                                             lag(ttl,8),
                                                                             ifelse(lag(year,9) == year - 10,
                                                                                    lag(ttl,9),
                                                                                    ifelse(lag(year, 10) == year - 10,
                                                                                           lag(ttl,10),
                                                                                           NA))))))))))) %>%
           mutate(age = year-yob,
                  experience = year-start)


w_lags %>% filter(grepl("DJ",str_sub(player_code,1,2)),year==2017)


################

prediction_data <-
  w_lags %>%
  ungroup() %>%
  filter(year == 2017) %>%
  select(-pick,-adj_value,-year,-first_name,-last_name,-player_code,-keeper,-games,-ppg,-ttl,-yob,-start,-times_drafted)

players <- prediction_data$player
teams <- prediction_data$team
prediction_data %<>% select(-player,-team)

prediction_data %<>% mutate(pos = factor(pos,levels = c("D/ST","HC","K","QB","RB","TE","WR")))

str(prediction_data)

#players <- prediction_data$player
#prediction_data  %<>% select(-player)

prediction_data$HC_kept = 0
prediction_data$QB_kept = 8
prediction_data$RB_kept = 7
prediction_data$WR_kept = 8
prediction_data$TE_kept = 0

colnames(train_df)
colnames(prediction_data)
sum(colnames(prediction_data) %in% colnames(train_df))/length(colnames(prediction_data))


## Generate new predictions...

colnames(data.matrix(prediction_data)) == colnames(data.matrix(train_df[,-2]))

prediction_data2 <- prediction_data[,colnames(train_df[,-2])]
sum(colnames(prediction_data2) != colnames(train_df[,-2]))
library(xgboost)
pmatrix <- xgb.DMatrix(data=data.matrix(prediction_data2))

prediction_data$pred <- predict(object = model2017,newdata=data.matrix(prediction_data2))

prediction_data$pred <- round(prediction_data$pred)

prediction_data$age
pred_df <- data.frame(pred = prediction_data$pred,
                      player = players,
                      pos = prediction_data$pos,
                      team = teams,
                      age = prediction_data$age,
                      stringsAsFactors = F)

pred_df %>%
  group_by(pos) %>%
  summarise(min_pred = min(pred),
            max_pred = max(pred),
            avg_pred = mean(pred))

team_byes <-data.frame(team = c("ATL","DEN","NO","WAS","BUF","CIN","DAL","SEA","DET","HOU","ARI","GB","JAC","LAR","NYG",
                                 "TEN","CHI","CLE","LAC","MIN","NE","PIT","BAL","KC","OAK","PHI","CAR","IND","MIA","NYJ",
                                 "SF","TB"),
                       bye = c(rep(5,4),rep(6,4),rep(7,2),rep(8,6),rep(9,6),rep(10,4),rep(11,6)),
                       stringsAsFactors = FALSE)

team_byes$bye <- ifelse(team_byes$team %in% c("TB","MIA"),1,team_byes$bye)

pred_df2 <- pred_df %>%
  left_join(.,team_byes, by = "team")


load(file="/home/john/stats_corner/2017/predict-cost-model/adp_w_proj2017.Rda")                                                                          

str(adp_w_proj2017)
str(pred_df2)

pred_df3 <-
  pred_df2 %>%
  mutate(pos = as.character(pos)) %>%
  inner_join(adp_w_proj2017 %>% select(player,pos,team,ptsGame), by = c("player","pos","team")) %>%
  rename(ppg = ptsGame)

projections2017 <- pred_df3
save(projections2017,file = "/home/john/stats_corner/2017/predict-cost-model/projections2017.Rda")
