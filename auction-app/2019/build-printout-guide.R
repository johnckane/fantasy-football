load("/home/john/projects/fantasy-football/data/adp-data/adp_2019_ranjed_w_age_bye.Rda")

colnames(adp_2019_ranked_w_age_bye)

adp_2019_ranked_w_age_bye_cluster <-
  adp_2019_ranked_w_age_bye %>%
  mutate(cluster = case_when(pos == 'QB' & between(pos_adp,1,3) ~ 1,
                             pos == 'QB' & between(pos_adp,4,7) ~ 2,
                             pos == 'QB' & between(pos_adp,8,14) ~ 3,
                             pos == 'QB' & between(pos_adp,15,20) ~ 4,
                             pos == 'QB' & between(pos_adp,21,28) ~ 5,
                             pos == 'RB' & between(pos_adp,1,3) ~ 1,
                             pos == 'RB' & between(pos_adp,4,6) ~ 2,
                             pos == 'RB' & between(pos_adp,7,10) ~ 3,
                             pos == 'RB' & between(pos_adp,11,14) ~ 4,
                             pos == 'RB' & between(pos_adp,15,18) ~ 5,
                             pos == 'RB' & between(pos_adp,19,22) ~ 6,
                             pos == 'RB' & between(pos_adp,23,27) ~ 7,
                             pos == 'RB' & between(pos_adp,28,34) ~ 8,
                             pos == 'RB' & between(pos_adp,35,44) ~ 9,
                             pos == 'RB' & between(pos_adp,45,60) ~ 10,
                             pos == 'WR' & between(pos_adp,1,3) ~ 1,
                             pos == 'WR' & between(pos_adp,4,6) ~ 2,
                             pos == 'WR' & between(pos_adp,7,10) ~ 3,
                             pos == 'WR' & between(pos_adp,11,14) ~ 4,
                             pos == 'WR' & between(pos_adp,15,18) ~ 5,
                             pos == 'WR' & between(pos_adp,19,22) ~ 6,
                             pos == 'WR' & between(pos_adp,23,27) ~ 7,
                             pos == 'WR' & between(pos_adp,28,34) ~ 8,
                             pos == 'WR' & between(pos_adp,35,44) ~ 9,
                             pos == 'WR' & between(pos_adp,45,60) ~ 10,
                             pos == 'TE' & between(pos_adp,1,2) ~ 1,
                             pos == 'TE' & between(pos_adp,3,5) ~ 2,
                             pos == 'TE' & between(pos_adp,6,12) ~ 3,
                             pos == 'TE' & between(pos_adp,13,16) ~ 4,
                             pos == 'TE' & between(pos_adp,17,20) ~ 5,
                             TRUE ~ as.numeric(NA)))

# bring in cluster cost and prod

load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/qb_single_obs_per_cluster.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/rb_single_obs_per_cluster.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/wr_single_obs_per_cluster.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/te_single_obs_per_cluster.Rda")

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
  adp_2019_ranked_w_age_bye_cluster %>%
  left_join(.,
            df,
            by = c("cluster","pos" = "position"))

glimpse(draft_data)

draft_data_sorted <- 
draft_data %>%
  group_by(pos) %>%
  arrange(pos,pos_adp) %>% 
  select(cluster,pos,pos_adp,player,age,bye,avg_ppg,avg_cost)

readr::write_csv(x = draft_data_sorted,
                 path = "/home/john/projects/fantasy-football/analysis/2019/auction-app-guide.csv")
