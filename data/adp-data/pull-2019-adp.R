library(tidyverse)
library(rvest)
library(RMySQL)
library(magrittr)
load("/home/john/projects/fantasy-football/team_df.Rda")
url_base <- "https://fantasyfootballcalculator.com/adp?format=standard&year="
url_coda <- "&teams=12&view=graph&pos=all"
year <- 2019
dim <- 195

for(j in 1:1){
  
  webpage <- read_html(paste0(url_base,year,url_coda))
  
  adp <- html_nodes(webpage,'td , .adp-player-name a') %>%
    html_text() %>%
    matrix(.,nrow = dim,ncol = 13, byrow = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    `colnames<-`(c("obs","pick","player","player2","pos","team","bye","overall","std_dev","high","low","times_drafted","blank")) %>%
    mutate(year = year) %>%
    select(year,pick,player,pos,team,overall,std_dev,high,low,times_drafted) %>%
    mutate(overall = as.numeric(overall),
           std_dev = as.numeric(std_dev),
           high = as.numeric(high),
           low = as.numeric(low),
           times_drafted = as.numeric(times_drafted))
  
  rm(webpage)
}

adp

pool <- dbPool(drv = RMySQL::MySQL(), 
               dbname = "armchair_analysis", 
               host = "localhost", 
               username = "john", 
               port = 3306, 
               unix.sock = "/var/run/mysqld/mysqld.sock")
players <- dbGetQuery(con = pool,
                      "select player as player_code, fname, lname, pos1, dob
                      from player")

adp$first_name <- sapply(str_split(adp$player,' '), '[[',1)
adp$last_name  <- sapply(str_split(adp$player,' '), '[[',2)

adp %>%
  group_by(player) %>%
  summarise(count = n()) %>%
  filter(count > 1)


duplicate_entries <- 
  adp %>%
  left_join(.,players, by = c("first_name" = "fname","last_name" = "lname")) %>%
  group_by(player) %>%
  summarise(count = n()) %>%
  filter(count > 1)

adp_players <- adp %>% group_by(player, first_name, last_name) %>% slice(1) %>% select(player, first_name, last_name)

single_entries <- 
  adp_players %>% 
  anti_join(.,duplicate_entries, by = c("player"))

single_entries_coded <-
  single_entries %>%
  left_join(.,players,by = c("first_name" = "fname","last_name" = "lname")) 

adp_w_code <-
  adp %>%
  left_join(.,single_entries_coded %>% ungroup() %>% select(player,player_code), by = c("player"))

adp_w_code %<>%
  mutate(player_code = ifelse(pos %in% c("DEF"), player,player_code)) %>%
  mutate(pos = ifelse(pos == "DEF","D/ST",
                      ifelse(pos == "PK","K",pos)))

uncoded <- 
  adp_w_code %>%
  filter(is.na(player_code == TRUE))



filter(uncoded,is.na(player_code)) %>% arrange(player) %>% pull(player) %>% cbind()

players %>%
  filter(lname %in% c('Mattison',
                      'Moore',
                      'Metcalf',
                      'Thompson',
                      'Montgomery',
                      'Samuel',
                      'Singletary',
                      'Jacobs',
                      'Murray',
                      'Sanders',
                      'Harry',
                      'Mahomes',
                      'Hockenson',
                      'Pollard')) %>%
  arrange(lname)


uncoded_w_code <-
    uncoded %>%
    mutate(player_code = case_when(player == "Adrian Peterson"    ~ 'AP-0700',
                                   player == "Alexander Mattison" ~ 'Matti01',
                                   player == "Antonio Brown"      ~ 'AB-3500',
                                   player == "Cam Newton"         ~ 'CN-0500',
                                   player == "CJ Anderson"        ~ 'CA-0750',
                                   player == "D.J. Moore"         ~ 'DM-2850',
                                   player == "D.K. Metcalf"       ~ 'Metca01',
                                   player == "Damien Harris"      ~ 'Harri01',
                                   player == "Darrell Henderson"  ~ 'Hende01',
                                   player == "Darwin Thompson"    ~ 'Thomp01',
                                   player == "David Johnson"      ~ 'DJ-1325',
                                   player == "David Montgomery"   ~ 'Montg01',
                                   player == "Deebo Samuel"       ~ 'Samue01',
                                   player == "Devin Singletary"   ~ 'Singl01',
                                   player == "Josh Allen"         ~ 'JA-1087', 
                                   player == "Josh Jacobs"        ~ 'Jacob01',
                                   player == "JuJu Smith-Schuster"~ 'JS-4750',
                                   player == "Justice Hill"       ~ 'HillJ01',
                                   player == "Justin Jackson"     ~ 'JJ-0575',
                                   player == "Kaimi Fairbairn"    ~ 'KF-0075',
                                   player == "Kyler Murray"       ~ 'Murra01',
                                   player == "LeVeon Bell"        ~ 'LB-0250',
                                   player == "Marvin Jones"       ~ 'MJ-2250',
                                   player == "Mecole Hardman"     ~ 'Hardm01',
                                   player == "Michael Thomas"     ~ 'MT-0875',
                                   player == "Mike Williams"      ~ 'MW-2825',
                                   player == "Miles Sanders"      ~ 'Sande01',
                                   player == "Mitch Trubisky"     ~ 'MT-1800',
                                   player == "N'Keal Harry"       ~ 'Harry01',
                                   player == "Odell Beckham Jr"   ~ 'OB-0075',
                                   player == "Parris Campbell"    ~ 'Campb01',
                                   player == "Pat Mahomes"        ~ 'PM-0025',
                                   player == "T.J. Hockenson"     ~ 'Hocke01',
                                   player == "Tony Pollard"       ~ 'Polla01',
                                   player == "Marquise Brown"     ~ 'Brown01',
                                   player == "Miles Boykin"       ~ 'Boyki01',     
                                   TRUE ~ as.character(NA)))
                                 

filter(uncoded_w_code,is.na(player_code)) %>% arrange(player)                                                                                 


adp_final <- bind_rows(adp_w_code %>% filter(is.na(player_code) == FALSE),uncoded_w_code)

adp_final %<>% 
  left_join(.,team_df %>% select(abbr,team_name), by = c("team" = "abbr")) %>%
  mutate(player_code =ifelse(pos == 'D/ST',paste0(team_name," ","D/ST"),player_code)) %>%
  select(-team_name)

adp_2019 <-
  adp_final %>%
  left_join(team_df,by = c("team" = "abbr")) %>%
  mutate(player_code2 = ifelse(pos == 'D/ST',paste0(team_name," D/ST"),player_code)) %>%
  mutate(player_code = player_code2) %>%
  select(-player_code2,team_name,city)


adp_2019 %>% distinct() %>% dim()
any(is.na(adp_2019))
which(is.na(adp_20))

head(adp_2019)

adp_2019_ranked <- 
  adp_2019 %>%
  arrange(overall) %>%
  mutate(overall = row_number()) %>%
  group_by(pos) %>%
  mutate(pos_adp = row_number()) 

## Incorporate age

head(players)

adp_2019_ranked_w_age <-
  adp_2019_ranked %>%
  left_join(players, by = "player_code") %>%
  mutate(age = 2019 - year(as_date(dob)))

# Also get BYE weeks
bye_weeks <-
  data.frame(team = c('NYJ','SF',
                      'DET','MIA',
                      'BUF','CHI','IND','OAK',
                      'CAR','CLE','PIT','TB',
                      'BAL','DAL',
                      'ATL','CIN','LAR','NO',
                      'DEN','HOU','JAX','NE','PHI','WAS',
                      'GB','NYG','SEA','TEN',
                      'ARI','KC','LAC','MIN'),
             bye = c(4,4,
                     5,5,
                     6,6,6,6,
                     7,7,7,7,
                     8,8,
                     9,9,9,9,
                     10,10,10,10,10,10,
                     11,11,11,11,
                     12,12,12,12),
             stringsAsFactors = F)

adp_2019_ranked_w_age_bye <-
  adp_2019_ranked_w_age %>%
  left_join(bye_weeks, by = 'team')

any(is.na(adp_2019_ranked_w_age_bye$bye))
#which(is.na(adp_2019_ranked_w_age_bye$bye))
#adp_2019_ranked_w_age_bye$team[c(23,83,121)]


save(adp_2019_ranked_w_age_bye, file = "/home/john/projects/fantasy-football/data/adp-data/adp_2019_ranjed_w_age_bye.Rda")

