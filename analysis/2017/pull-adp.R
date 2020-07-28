library(rvest)

url_base <- "https://fantasyfootballcalculator.com/adp?format=standard&year="
url_coda <- "&teams=12&view=graph&pos=all"

# url2007 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2007&teams=12&view=graph&pos=all"
# url2008 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2008&teams=12&view=graph&pos=all"
# url2009 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2009&teams=12&view=graph&pos=all"
# url2010 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2010&teams=12&view=graph&pos=all"
# url2011 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2011&teams=12&view=graph&pos=all"
# url2012 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2012&teams=12&view=graph&pos=all"
# url2013 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2013&teams=12&view=graph&pos=all"
# url2014 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2014&teams=12&view=graph&pos=all"
# url2015 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2015&teams=12&view=graph&pos=all"
# url2016 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2016&teams=12&view=graph&pos=all"
# url2017 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2017&teams=12&view=graph&pos=all"

data_list <- list()
dims <- c(215,180,202,210,183,145,194,163,194,168,217)

for(i in 1:10){
  
  webpage <- read_html(paste0(url_base,2006+i,url_coda))
  
  data_list[[i]] <- html_nodes(webpage,'td , .adp-player-name a') %>%
    html_text() %>%
    matrix(.,nrow = dims[i],ncol = 12, byrow = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    `colnames<-`(c("obs","pick","player","player2","pos","team","overall","std_dev","high","low","times_drafted","blank")) %>%
    mutate(year = i + 2006) %>%
    select(year,pick,player,pos,team,overall,std_dev,high,low,times_drafted) %>%
    mutate(overall = as.numeric(overall),
           std_dev = as.numeric(std_dev),
           high = as.numeric(high),
           low = as.numeric(low),
           times_drafted = as.numeric(times_drafted))
  
  rm(webpage)
}

data_list[[1]]

adp_data <- do.call(rbind,data_list)

save(adp_data, file = "/home/john/stats_corner/fantasy_football/adp_data.Rda")






# Pull in player code from the database. 
adp_data$first_name <- sapply(str_split(adp_data$player,' '), '[[',1)
adp_data$last_name  <- sapply(str_split(adp_data$player,' '), '[[',2)

adp_data_players <- adp_data %>% group_by(player, first_name, last_name) %>% slice(1) %>% select(player, first_name, last_name)

mydb = dbConnect(MySQL(), user='root', password='cz14F4b12', dbname='armchair_analysis', host='localhost')
players <- dbGetQuery(con = mydb,
                      "select player as player_code, fname, lname, pos1
                       from player")

duplicate_entries <- 
 adp_data_players %>%
  left_join(.,players, by = c("first_name" = "fname","last_name" = "lname")) %>%
  group_by(player) %>%
  summarise(count = n()) %>%
  filter(count > 1)


single_entries <- 
  adp_data_players %>% 
  anti_join(.,duplicate_entries, by = c("player"))

single_entries_coded <-
  single_entries %>%
  left_join(.,players,by = c("first_name" = "fname","last_name" = "lname")) 
single_entries_coded %>% head()


adp_data_w_code <-
  adp_data %>%
  left_join(.,single_entries_coded %>% ungroup() %>% select(player,player_code), by = c("player"))
adp_data_w_code %<>%
  mutate(player_code = ifelse(pos %in% c("DEF"), player,player_code)) %>%
  mutate(pos = ifelse(pos == "DEF","D/ST",
                      ifelse(pos == "PK","K",pos)))

with(adp_data_w_code,table(pos))

uncoded <- 
  adp_data_w_code %>%
  filter(is.na(player_code == TRUE))

# These teams are based on 2017 rosters
uncoded %>% group_by(player) %>% slice(1) %>% tally
uncoded %>% arrange(player,year) %>% slice(71:n())



uncoded %<>%
  mutate(player_code = ifelse(player == 'Adrian Peterson' & team == 'NO','AP-0700',
                       ifelse(player == 'Adrian Peterson' & team == 'FA','AP-0800',
                       ifelse(player == 'Alex Smith' & pos == 'QB','AS-1600',
                       ifelse(player == 'Antonio Brown','AB-3500',
                       ifelse(player == 'Beanie Wells','CW-1400',
                       ifelse(player == 'Benjamin Watson','BW-0700',
                       ifelse(player == 'Brandon Marshall','BM-0300',
                       ifelse(player == 'Cam Newton','CN-0500',
                       ifelse(player == 'Carnell Williams','CW-200',
                       ifelse(player == 'Charles Johnson','CJ-1450',
                       ifelse(player == 'Chris Johnson','CJ-1700',
                       ifelse(player == 'CJ Anderson','CA-0750',
                       ifelse(player == 'David Johnson','DJ-1325',
                       ifelse(player == 'Deshaun Foster','DF-1400',
                       ifelse(player == 'Donte Stallworth','DS-3600',
                       ifelse(player == 'EJ Manuel','EM-0250',
                       ifelse(player == 'Eric Johnson','EJ-0900',
                       ifelse(player == 'James Davis','D-0600',
                       ifelse(player == 'James Jones','JJ-4200',
                       ifelse(player == 'Jonathan Stewart','JS-6700',
                       ifelse(player == 'Joshua Cribbs','JC-5600',
                       ifelse(player == 'Kevin Smith','KS-1700',
                       ifelse(player == 'Kevin White','KW-0887',
                       ifelse(player == 'LeRon McClain','LM-0700',
                       ifelse(player == 'LeVeon Bell','LB-0250',
                       ifelse(player == 'Marvin Jones','MJ-2250',
                       ifelse(player == 'Matt Jones' & pos == 'WR','MJ-2300',
                       ifelse(player == 'Matt Jones' & pos == 'RB','MJ-2275',
                       ifelse(player == 'Michael Thomas','MT-0875',
                       ifelse(player == 'Mike Sims-Walker','MW-0400',
                       ifelse(player == 'Mike Williams' & team == 'FA', 'MW-2700',
                       ifelse(player == 'Mike Williams' & team == 'None', 'MW-2800',
                       ifelse(player == 'Odell Beckham Jr','OB-0075',
                       ifelse(player == 'Reggie Brown','RB-3700',
                       ifelse(player == 'Ricky Williams','RW-2600',
                       ifelse(player == 'Roy Williams','RW-3100',
                       ifelse(player == 'Ryan Grant','RG-1600',
                       ifelse(player == 'Steve Smith' & team == 'BAL','SS-2100',
                       ifelse(player == 'Steve Smith' & team == 'TB', 'SS-2200',
                       ifelse(player == 'TJ Yeldon','TY-0150',
                       ifelse(player == 'T.Y. Hilton','TH-1850',
                       ifelse(player == 'Zach Miller' & year %in% c(2009,2010,2011), 'ZM-0200',
                       ifelse(player == 'Zach Miller' & year %in% c(2016), 'ZM-0250',
                       ifelse(player == 'Chris Brown', 'CB-3600',
                       ifelse(player == 'Chris Henry' & pos == 'RB','CH-2700',
                       ifelse(player == 'Chris Henry' & pos == 'WR','CH-2800',
                       ifelse(player == 'Chris Givens','CG-1060',player_code
                       ))))))))))))))))))))))))))))))))))))))))))))))))

filter(uncoded,is.na(player_code))
dbGetQuery(con = mydb,"select * from player where fname = 'Chris' and lname = 'Givens'")


coded_adp_data <- 
  bind_rows(adp_data_w_code %>% filter(is.na(player_code) == FALSE),
            uncoded)

lapply(coded_adp_data, function(x) sum(is.na(x)))

coded_adp_data %>% filter(is.na(player_code))

head(team_df)

coded_adp_data2 <-
  coded_adp_data %>%
  left_join(.,team_df %>% select(abbr,team_name), by = c("team" = "abbr")) %>%
  mutate(player_code =ifelse(pos == 'D/ST',paste0(team_name," ","D/ST"),player_code)) %>%
  select(-team_name)
                             

save(coded_adp_data2,file = "/home/john/stats_corner/fantasy_football/coded_adp_data_2010_2016.Rda")
                       