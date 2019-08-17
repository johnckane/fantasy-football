library(tidyverse)
library(rvest)
library(RMySQL)
library(magrittr)
load("/home/john/fantasy-football/team_df.Rda")
url_base <- "https://fantasyfootballcalculator.com/adp?format=standard&year="
url_coda <- "&teams=12&view=graph&pos=all"
year <- 2018
dim <- 211

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


mydb = dbConnect(MySQL(), user='root', password='cz14F4b12', dbname='armchair_analysis', host='localhost')
players <- dbGetQuery(con = mydb,
                      "select player as player_code, fname, lname, pos1
                      from player")

adp$first_name <- sapply(str_split(adp$player,' '), '[[',1)
adp$last_name  <- sapply(str_split(adp$player,' '), '[[',2)


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



filter(uncoded,is.na(player_code)) %>% arrange(player)


 dbGetQuery(con = mydb,
           "select * from player where 
            #fname = 'Christian' 
            #and 
            #pos1 = 'WR'
            #and
            lname = 'Yeldon'
           ")

# Lots of rookies in here
uncoded %<>%
  mutate(player_code = case_when(player == 'Alex Smith' & pos == 'QB'     ~ 'AS-1600',
                                 player == 'Anthony Miller' & pos == 'WR' ~ 'Mille01',
                                 player == 'Antonio Brown'                ~ 'AB-3500',
                                 player == 'Antonio Callaway'             ~ 'Calla01',
                                 player == 'Calvin Ridley'                ~ 'Ridle01',
                                 player == 'Cam Newton'                   ~ 'CN-0500',
                                 player == 'Chris Thompson'               ~ 'CT-1220',
                                 player == 'CJ Anderson'                  ~ 'CA-0750',
                                 player == 'Daniel Carlson'               ~ 'Carls01',
                                 player == 'David Johnson'                ~ 'DJ-1325',
                                 player == 'Donta Foreman'                ~ 'DF-1362',
                                 player == 'Hayden Hurst'                 ~ 'Hurst01',
                                 player == 'Jordan Wilkins'               ~ 'Wilki01',
                                 player == 'JuJu Smith-Schuster'          ~ 'JS-4750',
                                 player == 'Kalen Ballage'                ~ 'Balla01',
                                 player == 'Kerryon Johnson'              ~ 'Johns01',
                                 player == 'LeVeon Bell'                  ~ 'LB-0250',
                                 player == 'Marvin Jones'                 ~ 'MJ-2250',
                                 player == 'Michael Gallup'               ~ 'Gallu01',
                                 player == 'Michael Thomas'               ~ 'MT-0875',
                                 player == 'Mike Williams'                ~ 'MW-2825',
                                 player == 'Mitch Trubisky'               ~ 'MT-1800',
                                 player == 'Nick Chubb'                   ~ 'Chubb01',
                                 player == 'Nyheim Hines'                 ~ 'Hines01',
                                 player == 'Odell Beckham Jr'             ~ 'OB-0075',
                                 player == 'Pat Mahomes'                  ~ 'PM-0025',
                                 player == 'Rashaad Penny'                ~ 'Penny01',
                                 player == 'Ronald Jones II'              ~ 'Jones01',
                                 player == 'Royce Freeman'                ~ 'Freem01',
                                 player == 'Saquon Barkley'               ~ 'Barkl01',
                                 player == 'Sony Michel'                  ~ 'Miche01',
                                 player == 'T.Y. Hilton'                  ~ 'TH-1850',
                                 player == 'Wil Lutz'                     ~ 'WL-0300',
                                 player == 'Adrian Peterson'              ~ 'AP-0700',
                                 player == 'Christian Kirk'               ~ 'KirkC01',
                                 player == 'Courtland Sutton'             ~ 'Sutto01',
                                 player == 'Mike Gesicki'                 ~ 'Gesic01',
                                 player == 'TJ Yeldon'                    ~ 'TY-0150',
                                 TRUE ~ as.character(NA)))
                                                                                                                       

filter(uncoded,is.na(player_code)) %>% arrange(player)                                                                                 

colnames(adp_w_code)
colnames(uncoded)


adp_final <- bind_rows(adp_w_code %>% filter(is.na(player_code) == FALSE),uncoded)

adp_final %<>% 
  left_join(.,team_df %>% select(abbr,team_name), by = c("team" = "abbr")) %>%
  mutate(player_code =ifelse(pos == 'D/ST',paste0(team_name," ","D/ST"),player_code)) %>%
  select(-team_name)

adp_2018 <-
  adp_final %>%
  left_join(team_df,by = c("team" = "abbr")) %>%
  mutate(player_code2 = ifelse(pos == 'D/ST',paste0(team_name," D/ST"),player_code)) %>%
  mutate(player_code = player_code2) %>%
  select(-player_code2,team_name,city)


adp_2018 %>% distinct() %>% dim()


save(adp_2018, file = "/home/john/fantasy-football/data/adp-data/adp_2018.Rda")
