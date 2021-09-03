load("/home/john/projects/fantasy-football/data/adp-data/adp_2020_ranked_w_age_bye.Rda")

adp_w_clusters <-
adp_2020_ranked_w_age_bye %>%
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


adp_w_clusters %>%
  filter(pos == 'QB') %>%
  arrange(pos_adp) %>%
  select(pos_adp, cluster, player, team, age, bye) %>%
  print(n = 25)


adp_w_clusters %>%
     filter(pos == 'RB') %>%
     arrange(pos_adp) %>%
     select(pos_adp, cluster, player, team, age, bye) %>%
     print(n = 68)

adp_w_clusters %>%
  filter(pos == 'WR') %>%
  arrange(pos_adp) %>%
  select(pos_adp, cluster, player, team, age, bye) %>%
  print(n = 68)

adp_w_clusters %>%
  filter(pos == 'TE') %>%
  arrange(pos_adp) %>%
  select(pos_adp, cluster, player, team, age, bye) %>%
  print(n = 68)

adp_w_clusters %>%
  filter(pos == 'K') %>%
  arrange(pos_adp) %>%
  select(pos_adp, cluster, player, team, age, bye)

adp_w_clusters %>%
  filter(pos == 'D/ST') %>%
  arrange(pos_adp) %>%
  select(pos_adp, cluster, player, team, age, bye)
