url_base <- "https://fantasyfootballcalculator.com/adp?format=standard&year="
url_coda <- "&teams=12&view=graph&pos=all"
year <- 2017
dim <- 211

for(j in 1:1){
  
  webpage <- read_html(paste0(url_base,year,url_coda))
  
  adp2017 <- html_nodes(webpage,'td , .adp-player-name a') %>%
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

adp2017$first_name <- sapply(str_split(adp2017$player,' '), '[[',1)
adp2017$last_name  <- sapply(str_split(adp2017$player,' '), '[[',2)


duplicate_entries <- 
  adp2017 %>%
  left_join(.,players, by = c("first_name" = "fname","last_name" = "lname")) %>%
  group_by(player) %>%
  summarise(count = n()) %>%
  filter(count > 1)

adp2017_players <- adp2017 %>% group_by(player, first_name, last_name) %>% slice(1) %>% select(player, first_name, last_name)

single_entries <- 
  adp2017_players %>% 
  anti_join(.,duplicate_entries, by = c("player"))

single_entries_coded <-
  single_entries %>%
  left_join(.,players,by = c("first_name" = "fname","last_name" = "lname")) 
adp2017_w_code <-
  adp2017 %>%
  left_join(.,single_entries_coded %>% ungroup() %>% select(player,player_code), by = c("player"))

adp2017_w_code %<>%
  mutate(player_code = ifelse(pos %in% c("DEF"), player,player_code)) %>%
  mutate(pos = ifelse(pos == "DEF","D/ST",
                      ifelse(pos == "PK","K",pos)))

uncoded2017 <- 
  adp2017_w_code %>%
  filter(is.na(player_code == TRUE))



filter(uncoded2017,is.na(player_code)) %>% arrange(player)

# Lots of rookies in here
uncoded2017 %<>%
  mutate(player_code = ifelse(player == 'Adrian Peterson' & team == 'NO','AP-0700',
                       ifelse(player == 'Alex Smith' & pos == 'QB','AS-1600',
                       ifelse(player == 'Antonio Brown','AB-3500',
                       ifelse(player == 'Brandon Marshall','BM-0300',
                       ifelse(player == 'Cam Newton','CN-0500',
                       ifelse(player == 'CJ Anderson','CA-0750',
                       ifelse(player == 'David Johnson','DJ-1325',
                       ifelse(player == 'Jonathan Stewart','JS-6700',
                       ifelse(player == 'Kevin White','KW-0887',
                       ifelse(player == 'LeVeon Bell','LB-0250',
                       ifelse(player == 'Marvin Jones','MJ-2250',
                       ifelse(player == 'Michael Thomas','MT-0875',
                       ifelse(player == 'Odell Beckham Jr','OB-0075',
                       ifelse(player == 'T.Y. Hilton','TH-1850',
                       ifelse(player == 'Chris Brown', 'CB-3600',player_code
                       ))))))))))))))))
                                             
filter(uncoded2017,is.na(player_code)) %>% arrange(player)                                                                                 


# Hard code these values
# Solution is to list first five letters of last_name,first_name then a number 01,02,etc.
uncoded2017 %<>%
  mutate(player_code = ifelse(player == 'Alvin Kamara','Kamar01',
                       ifelse(player == 'Christian McCaffrey','McCaf01',
                       ifelse(player == 'CJ Prosise','Prosi01',
                       ifelse(player == 'Cooper Kupp','KuppC01',
                       ifelse(player == 'Corey Davis','Davis01',
                       ifelse(player == 'Dalvin Cook','CookD01',
                       ifelse(player == 'Deshaun Watson','Watso01',
                       ifelse(player == 'Donta Foreman','Freem01',
                       ifelse(player == 'Evan Engram','Engra01',
                       ifelse(player == 'Jamaal Williams','Willi01',
                       ifelse(player == 'James Conner','Conne01',
                       ifelse(player == 'Joe Mixon','Mixon01',
                       ifelse(player == 'Joe Williams','Willi02',
                       ifelse(player == 'John Ross','RossJ01',
                       ifelse(player == 'Kareem Hunt','HuntK01',
                       ifelse(player == 'Kenny Golladay','Golla01',
                       ifelse(player == 'Leonard Fournette','Fourn01',
                       ifelse(player == 'Marlon Mack','MackM01',
                       ifelse(player == 'O.J. Howard','Howar01',
                       ifelse(player == 'Samaje Perine','Perin01',
                       ifelse(player == 'Zay Jones','Jones01',player_code
                       ))))))))))))))))))))))
filter(uncoded2017,is.na(player_code)) %>% arrange(player)  


colnames(adp2017_w_code)
colnames(uncoded2017)


adp2017_final <- bind_rows(adp2017_w_code %>% filter(is.na(player_code) == FALSE),uncoded2017)

adp2017_final %<>% 
  left_join(.,team_df %>% select(abbr,team_name), by = c("team" = "abbr")) %>%
  mutate(player_code =ifelse(pos == 'D/ST',paste0(team_name," ","D/ST"),player_code)) %>%
  select(-team_name)

adp2017_final2 <-
  adp2017_final %>%
  left_join(team_df,by = c("team" = "abbr")) %>%
  mutate(player_code2 = ifelse(pos == 'D/ST',paste0(team_name," D/ST"),player_code)) %>%
  mutate(player_code = player_code2) %>%
  select(-player_code2,team_name,city)


adp2017_final2 %>% distinct() %>% dim()


save(adp2017_final2, file = "/home/john/stats_corner/2017/adp2017_final.Rda")
#filter(adp2017_final, player == 'Antonio Brown')
#filter(adp2017_w_code, player == 'Antonio Brown')
