# No ages for 2022...need to reformat data to use nfl-verse instead. 
## Use just adp and create clusters
load("/home/john/projects/fantasy-football/data/adp-data/adp_2023_raw.Rda")

adp_w_rankings <-
  adp %>%
  select(player,pos,team,overall) %>%
  group_by(pos) %>%
  arrange(overall) %>%
  mutate(pos_adp = row_number())

adp_cluster <-
  adp_w_rankings %>%
  mutate(cluster = case_when(pos == 'QB' & between(pos_adp,1,2) ~ 1,
                             pos == 'QB' & between(pos_adp,3,4) ~ 2,
                             pos == 'QB' & between(pos_adp,5,8) ~ 3,
                             pos == 'QB' & between(pos_adp,9,16) ~ 4,
                             pos == 'QB' & between(pos_adp,17,19) ~ 5,
                             pos == 'QB' & between(pos_adp,20,25) ~ 6,
                             pos == 'RB' & between(pos_adp,1,3) ~ 1,
                             pos == 'RB' & between(pos_adp,4,6) ~ 2,
                             pos == 'RB' & between(pos_adp,7,10) ~ 3,
                             pos == 'RB' & between(pos_adp,11,14) ~ 4,
                             pos == 'RB' & between(pos_adp,15,18) ~ 5,
                             pos == 'RB' & between(pos_adp,19,22) ~ 6,
                             pos == 'RB' & between(pos_adp,23,28) ~ 7,
                             pos == 'RB' & between(pos_adp,29,35) ~ 8,
                             pos == 'RB' & between(pos_adp,36,45) ~ 9,
                             pos == 'RB' & between(pos_adp,46,60) ~ 10,
                             pos == 'WR' & between(pos_adp,1,2) ~ 1,
                             pos == 'WR' & between(pos_adp,3,5) ~ 2,
                             pos == 'WR' & between(pos_adp,6,8) ~ 3,
                             pos == 'WR' & between(pos_adp,9,11) ~ 4,
                             pos == 'WR' & between(pos_adp,12,14) ~ 5,
                             pos == 'WR' & between(pos_adp,15,18) ~ 6,
                             pos == 'WR' & between(pos_adp,19,24) ~ 7,
                             pos == 'WR' & between(pos_adp,25,28) ~ 8,
                             pos == 'WR' & between(pos_adp,29,33) ~ 9,
                             pos == 'WR' & between(pos_adp,34,38) ~ 10,
                             pos == 'WR' & between(pos_adp,39,50) ~ 11,
                             pos == 'WR' & between(pos_adp,51,60) ~ 12,
                             pos == 'TE' & between(pos_adp,1,2) ~ 1,
                             pos == 'TE' & between(pos_adp,3,4) ~ 2,
                             pos == 'TE' & between(pos_adp,5,13) ~ 3,
                             pos == 'TE' & between(pos_adp,14,17) ~ 4,
                             pos == 'TE' & between(pos_adp,18,20) ~ 5,
                             TRUE ~ as.numeric(NA)))

# bring in cluster cost and prod

load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/qb_single_obs_per_cluster_last5.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/rb_single_obs_per_cluster_last5.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/wr_single_obs_per_cluster_last5.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/te_single_obs_per_cluster_last5.Rda")

df <- bind_rows(qb_single_obs_per_cluster %>% mutate(position = 'QB',
                                                     avg_cost = round(avg_cost),
                                                     avg_ppg = round(avg_ppg,1)),
                rb_single_obs_per_cluster %>% mutate(position = 'RB',
                                                     avg_cost = round(avg_cost),
                                                     avg_ppg = round(avg_ppg,1)),
                wr_single_obs_per_cluster %>% mutate(position = 'WR',
                                                     avg_cost = round(avg_cost),
                                                     avg_ppg = round(avg_ppg,1)),
                te_single_obs_per_cluster %>% mutate(position = 'TE',
                                                     avg_cost = round(avg_cost),
                                                     avg_ppg = round(avg_ppg,1)))

draft_data <-
  adp_cluster %>%
  left_join(.,
            df,
            by = c("cluster","pos" = "position"))

glimpse(draft_data)

draft_data_sorted <- 
draft_data %>%
  group_by(pos) %>%
  arrange(pos,pos_adp) %>% 
  select(cluster,pos,pos_adp,player,avg_ppg,avg_cost)

readr::write_csv(x = draft_data_sorted,
                 file = "/home/john/projects/fantasy-football/analysis/2023/auction-app-guide.csv")
