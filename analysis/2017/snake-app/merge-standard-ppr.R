library(tidyverse)
library(stringr)
library(httr)
library(rvest)
library(magrittr)
standard <- read.csv("/home/john/stats_corner/2017/snake-app/ffa_standard20170829.csv",stringsAsFactors = FALSE)
ppr <- read.csv("/home/john/stats_corner/2017/snake-app/ffa_customrankings2017-1 (1) - ppr.csv", stringsAsFactors = FALSE)
colnames(standard)
colnames(ppr)

# Need ADP for PPR #
url_base <- "https://fantasyfootballcalculator.com/adp?format=ppr&year=2017&teams=12&view=graph&pos=all"
url_coda <- "&teams=12&view=graph&pos=all"
year <- 2017
dim <- 184

for(j in 1:1){
  
  webpage <- read_html(paste0(url_base))
  
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

ppr_defense <- filter(ppr,position == "DST")
ppr %<>% filter(position != "DST")
colnames(adp2017)
table(adp2017$pos)
adp2017_defense <- filter(adp2017, pos == "DEF")
adp2017 %<>% filter(pos != "DEF")

adp2017 %<>% select(player,pos,team,overall)

ppr %>%
  mutate(player = str_trim(player)) %>%
  filter(grepl("Prosise",player)==T)

adp2017 %<>%
  mutate(player = str_trim(player))

adp2017
adp2017 %>% filter(grepl("Prosise",player) == T)

adp2017 %<>%
  mutate(player = ifelse(player == "Odell Beckham Jr","Odell Beckham",
                         ifelse(player == "CJ Anderson","C.J. Anderson",
                                ifelse(player == "Ted Ginn Jr","Ted Ginn",
                                       ifelse(player == "CJ Prosise","C.J. Prosise",player)))))


adp2017 %>% filter(grepl("Beckham",player) == T)

ppr2 <- 
adp2017 %>%
  left_join(.,ppr,by = c("player","team"))

ppr2 %>% filter(is.na(points))

ppr2 %>% filter(grepl("Prosise",player) == T)

head(adp2017_defense)
head(ppr_defense)
ppr_defense <- 
  adp2017_defense %>%
  left_join(.,ppr_defense,by="team")

colnames(ppr2)
colnames(ppr_defense)

ppr_defense %<>% rename(player=player.x)
ppr_defense <- ppr_defense[,c(colnames(ppr2))]


ppr_defense

ppr3

ppr3 <-
  bind_rows(ppr2,ppr_defense) %>%
  arrange(overall) %>%
  mutate(adp=row_number(),
         pointsptsGame = points/16)

standard %<>%
  rename(standard_adp = adp,
         standard_points = ptsGame) %>%
  mutate(player_team = str_trim(paste0(player," - ",team))) %>%
  select(player_team,position,age,bye,standard_adp,standard_points,team) %>%
  distinct()


ppr3 %<>%
  rename(ppr_adp = adp,
         ppr_points = pointsptsGame) %>%
  mutate(player_team = str_trim(paste0(player," - ",team))) %>%
  select(player_team,position,ppr_adp,ppr_points,team) %>%
  distinct()
  


ppr3 %>%
  filter(position == "DST")

standard %>%
  filter(position == "DST")


ppr3 %<>% mutate(player_team = ifelse(position == "DST",paste0(team," D/ST"),player_team))
standard %<>% mutate(player_team = ifelse(position == "DST",paste0(team," D/ST"),player_team))
  

combined_data <- inner_join(standard,ppr3,by = c("player_team","position"))




combined_data %<>%
  mutate(position = ifelse(position == 'DST','D/ST',position)) %>%
  filter(position %in% c('QB','RB','WR','TE','D/ST','K')) %>%
  arrange(ppr_adp) %>%
  mutate(ppr_adp = ifelse(is.na(ppr_adp),999,ppr_adp),
         standard_adp = ifelse(is.na(standard_adp),999,standard_adp)) %>%
  select(-team.x,-team.y)
  


write_csv(combined_data,"/home/john/stats_corner/2017/snake-app/ffa_data.csv")

