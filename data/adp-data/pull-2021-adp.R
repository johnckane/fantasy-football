library(tidyverse)
library(rvest)
library(RMySQL)
library(magrittr)
library(pool)
load("/home/john/projects/fantasy-football/data/team_df.Rda")
#link: https://fantasyfootballcalculator.com/adp?format=standard&year=&teams=12&view=graph&pos=all
url_base <- "https://fantasyfootballcalculator.com/adp?format=standard&year="
url_coda <- "&teams=12&view=graph&pos=all"
year <- 2021
dim <- 223

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

player_info <- function(player){
  uncoded %>%
    filter(player == player) %>%
    inner_join(players, by = c('last_name' = 'lname'))
}

players %>% filter(lname == 'Dillon')

uncoded_w_code <-
    uncoded %>%
    mutate(player_code = case_when(player == "Adam Trautman"        ~ "Trautm01",
                                   player == "Amon-Ra St. Brown"    ~ "StBrow01",
                                   player == "Antonio Brown"        ~ "BrownA01",
                                   player == "A.J. Dillon"          ~ "Dillon01",
                                   player == "Adrian Peterson"      ~ "AP-0700",
                                   player == "Antonio Gibson"       ~ "Gibson01",
                                   player == "Benny Snell Jr."      ~ "SnellB01",
                                   player == "Brandon Aiyuk"        ~ "AiyukB01",
                                   player == "Bryan Edwards"        ~ "Edward01",
                                   player == "Cam Akers"            ~ "AkersC01",
                                   player == "Cam Newton"           ~ "CN-0500",
                                   player == "CeeDee Lamb"          ~ "LambCe01",
                                   player == "Chase Claypool"       ~ "Claypo01",
                                   player == "Chris Thompson"       ~ "CT-1220",
                                   player == "Chuba Hubbard"        ~ "Hubbar01",
                                   player == "Clyde Edwards-Helaire"~ "Edward02",
                                   player == "Darnell Mooney"       ~ "Mooney01",
                                   player == "D.J. Moore"           ~ "DM-2850",
                                   player == "D'Andre Swift"        ~ "Swift01",
                                   player == "Darrynton Evans"      ~ "EvansD01",
                                   player == "David Johnson"        ~ "DJ-1325",
                                   player == "DeVonta Smith"        ~ "SmithD01",
                                   player == "Elijah Moore"         ~ "MooreE01",
                                   player == "Gabriel Davis"        ~ "DavisG01",
                                   player == "Henry Ruggs III"      ~ "RuggsH01",
                                   player == "Irv Smith Jr."        ~ "SmithI01",
                                   player == "Ja'Marr Chase"        ~ "ChaseJ01",
                                   player == "J.K. Dobbins"         ~ "Dobbin01",
                                   player == "Jalen Hurts"          ~ "HurtsJ01",
                                   player == "Jalen Reagor"         ~ "Reagor01",
                                   player == "James Robinson"       ~ "Robins01",
                                   player == "Jerry Jeudy"          ~ "JeudyJ01",
                                   player == "Javonte Williams"     ~ "Willia01",
                                   player == "J.J. Taylor"          ~ "Taylor01",
                                   player == "Joe Burrow"           ~ "Burrow01",
                                   player == "Jonathan Taylor"      ~ "Taylor01",
                                   player == "Josh Allen"           ~ "JA-1087",
                                   player == "Joshua Kelley"        ~ "Kelley01",
                                   player == "JuJu Smith-Schuster"  ~ "JS-4750",
                                   player == "Justin Fields"        ~ "Fields01",
                                   player == "Justin Herbert"       ~ "Herber01",
                                   player == "Justin Jackson"       ~ "JJ-0575",
                                   player == "Justin Jefferson"     ~ "Jeffer01",
                                   player == "Jaylen Waddle"        ~ "Waddle01",
                                   player == "Kaimi Fairbairn"      ~ "KF-0075",
                                   player == "Ke'Shawn Vaughn"      ~ "Vaughn",
                                   player == "Kenneth Gainwell"     ~ "Gainwe01",
                                   player == "Kyle Pitts"           ~ "PittsK01",
                                   player == "Laviska Shenault Jr." ~ "Shenau01",
                                   player == "LeVeon Bell"          ~ "LB-0250",
                                   player == "Marquez Callaway"     ~ "Callaw01",
                                   player == "Marvin Jones"         ~ "MJ-2250",
                                   player == "Mac Jones"            ~ "JonesMa01",
                                   player == "Michael Thomas"       ~ "MT-0875",
                                   player == "Michael Carter"       ~ "Carter01",
                                   player == "Mike Williams"        ~ "MW-2825",
                                   player == "Odell Beckham Jr"     ~ "OB-0075",
                                   player == "Pat Freiermuth"       ~ "Freier01",
                                   player == "Pat Mahomes"          ~ "PM-0025",
                                   player == "Rhamondre Stevenson"  ~ "Steven01",
                                   player == "Robert Tonyan Jr."    ~ "Tonyan01",
                                   player == "Rodrigo Blankenship"  ~ "Blanke01",
                                   player == "Rondale Moore"        ~ "MooreR01",
                                   player == "Steven Hauschka"      ~ "SH-0400",
                                   player == "Tee Higgins"          ~ "Higgin01",
                                   player == "Terrace Marshall Jr." ~ "Marsha01",
                                   player == "Tony Jones Jr."       ~ "JonesT01",
                                   player == "Trey Sermon"          ~ "Sermon01",
                                   player == "Trevor Lawrence"      ~ "Lawren01",
                                   player == "Trey Lance"           ~ "LanceT01",
                                   player == "Tua Tagovailoa"       ~ "Tagova01",
                                   player == "Tyler Bass"           ~ "BassTy01",
                                   player == "Xavier Jones"         ~ "JonesX01",
                                   player == "Zack Moss"            ~ "MossZa01",    
                                   player == "Zach Wilson"          ~ "Wilson01",
                                   TRUE ~ as.character(NA)))


filter(uncoded_w_code,is.na(player_code)) %>% arrange(player)                                                                                 



adp_final <- bind_rows(adp_w_code %>% filter(is.na(player_code) == FALSE),uncoded_w_code)

adp_final %<>% 
  left_join(.,team_df %>% select(abbr,team_name), by = c("team" = "abbr")) %>%
  mutate(player_code =ifelse(pos == 'D/ST',paste0(team_name," ","D/ST"),player_code)) %>%
  select(-team_name)


## Need to fix differentiating team names here, for examples JAX and JAC
adp_final <-
  adp_final %>%
  mutate(team = ifelse(team == 'JAX','JAC',team))


adp_2021 <-
  adp_final %>%
  left_join(team_df,by = c("team" = "abbr")) %>%
  mutate(player_code2 = ifelse(pos == 'D/ST',paste0(team_name," D/ST"),player_code)) %>%
  mutate(player_code = player_code2) %>%
  select(-player_code2,team_name,city)


adp_2021 %>% distinct() %>% dim()
any(is.na(adp_2021))
which(is.na(adp_2021))

adp_2021_ranked <- 
  adp_2021 %>%
  arrange(overall) %>%
  mutate(overall = row_number()) %>%
  group_by(pos) %>%
  mutate(pos_adp = row_number()) 

## Incorporate age

head(players)

adp_2021_ranked_w_age <-
  adp_2021_ranked %>%
  left_join(players, by = "player_code") %>%
  mutate(age = 2021 - lubridate::year(lubridate::as_date(dob)))

# Also get BYE weeks
bye_weeks <-
  data.frame(team = c('ATL','NO','NYJ','SF',
                      'BUF','DAL','JAC','LAC','MIN','PIT',
                      'BAL','LV',
                      'DET','SEA','TB','WAS',
                      'CHI','CIN','HOU','NYG',
                      'LAR','DEN',
                      'ARI','KC',
                      'CAR','CLE','GB','TEN',
                      'IND','MIA','NE','PHI'),
             bye = c(6,6,6,6,
                     7,7,7,7,7,7,
                     8,8,
                     9,9,9,9,
                     10,10,10,10,
                     11,11,
                     12,12,
                     13,13,13,13,
                     14,14,14,14),
             stringsAsFactors=F)

adp_2021_ranked_w_age_bye <-
  adp_2021_ranked_w_age %>%
  left_join(bye_weeks, by = 'team')

any(is.na(adp_2021_ranked_w_age_bye$bye))
save(adp_2021_ranked_w_age_bye, file = "/home/john/projects/fantasy-football/data/adp-data/adp_2021_ranked_w_age_bye.Rda")
