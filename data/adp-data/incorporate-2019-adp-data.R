library(tidyverse)
load("~/projects/fantasy-football/data/adp-data/adp.Rda")
load("~/projects/fantasy-football/data/adp-data/adp_2019_ranjed_w_age_bye.Rda")

head(adp_2019_ranked_w_age_bye)
colnames(adp)
colnames(adp_2019_ranked_w_age_bye)


## Connect to the database
library(pool)
pool <- dbPool(drv = RMySQL::MySQL(), 
               dbname = "armchair_analysis", 
               host = "localhost", 
               username = "john", 
               port = 3306, 
               unix.sock = "/var/run/mysqld/mysqld.sock")
players <- dbGetQuery(con = pool,
                      "SELECT player, fname, lname, pos1
                       FROM player")

#We need to find all "rookies" from 2019 
adp_2019_ranked_w_age_bye$coded <- 
  ifelse(
    grepl(pattern = "-",
          x = adp_2019_ranked_w_age_bye$player_code
    ) | (adp_2019_ranked_w_age_bye$pos == 'D/ST'),
    1,
    0
  )

adp_2019_ranked_w_age_bye %>% filter(coded == 0)

coded <- adp_2019_ranked_w_age_bye %>% filter(coded == 1)
uncoded <- adp_2019_ranked_w_age_bye %>% filter(coded == 0)

uncoded %>%
  select(player, player_code) %>%
  arrange(player)
  print(n = 20)

  
uncoded2 <-
  uncoded %>%
  mutate(player_code = case_when(player == 'Alexander Mattison' ~ 'AM-0825',
                                 player == 'D.K. Metcalf' ~ 'DM-2275',
                                 player == 'Damien Harris' ~ 'DH-1362',
                                 player == 'Darrell Henderson' ~ 'DH-2325',
                                 player == 'Darwin Thompson' ~ 'DT-1231',
                                 player == 'David Montgomery' ~ 'DM-2787',
                                 player == 'Deebo Samuel' ~ 'DS-0150',
                                 player == 'Devin Singletary' ~ 'DS-2177',
                                 player == 'Josh Jacobs' ~ 'JJ-0587',
                                 player == 'Justice Hill' ~ 'JH-4085',
                                 player == 'Kyler Murray' ~ 'KM-3500',
                                 player == 'Marquise Brown' ~ 'MB-4075',
                                 player == 'Mecole Hardman' ~ 'MH-0875',
                                 player == 'Miles Boykin' ~ 'MB-3325',
                                 player == 'Miles Sanders' ~ 'MS-0125',
                                 player == "N'Keal Harry" ~ 'NH-0825',
                                 player == 'T.J. Hockenson' ~ 'TH-1875',
                                 player == 'Tony Pollard' ~ 'TP-1650',
                                 TRUE ~ player_code))

all_coded <-
  bind_rows(coded,uncoded2) %>%
  ungroup() %>%
  rename(player_name = player) %>%
  mutate(overall_adp = overall) %>%
  select(year,pick,player_name,pos,overall,std_dev,high,low,player_code,pos_adp,overall_adp)

colnames(all_coded)
colnames(adp)
##############################################
adp <- adp %>% rename(player_name = player.x)

adp <- bind_rows(adp,adp2019)


save(adp,file = "/home/john/projects/fantasy-football/data/adp-data/adp.Rda")
