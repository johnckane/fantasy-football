data_file <- "/home/john/stats_corner/2017/snake-app/ffa_customrankings2017-1-std.csv"

library(tidyverse)
library(DBI)
library(stringr)
library(RMySQL)
library(magrittr)
ffa <- read_csv(data_file)

with(ffa,table(position))
colnames(ffa)
ffa %<>% filter(position %in% c("DST","K","QB","RB","TE","WR"))
ffa %<>% mutate(position = ifelse(position == 'DST','D/ST',position))
# Defenses don't have last names

ffa_dst <- filter(ffa, position == "D/ST")
ffa_no_dst <- filter(ffa, position != 'D/ST')


ffa_no_dst$first_name <- sapply(str_split(ffa_no_dst$player,' '), '[[',1)
ffa_no_dst$last_name <- sapply(str_split(ffa_no_dst$player,' '), '[[',2)


mydb = dbConnect(MySQL(), user='root', password='cz14F4b12', dbname='armchair_analysis', host='localhost')
players <- dbGetQuery(con = mydb,
                      "select player as player_code, fname, lname, pos1, yob
                       from player")

colnames(ffa_no_dst) 


duplicate_entries <- 
  ffa_no_dst %>%
  left_join(.,players, by = c("first_name" = "fname","last_name" = "lname")) %>%
  group_by(player,first_name,last_name) %>%
  summarise(count = n()) %>%
  filter(count > 1)

duplicate_entries
library(dplyr)

ffa_no_dst_players <- ffa_no_dst %>% ungroup() %>% group_by(player, first_name, last_name) %>% slice(1) %>% select(player, first_name, last_name)

single_entries <- 
  ffa_no_dst_players %>% 
  anti_join(.,duplicate_entries, by = c("player"))

single_entries_coded <-
  single_entries %>%
  left_join(.,players,by = c("first_name" = "fname","last_name" = "lname")) 
ffa_no_dst_w_code <-
  ffa_no_dst %>%
  left_join(.,single_entries_coded %>% ungroup() %>% select(player,player_code), by = c("player"))


uncoded_ffa <- 
  ffa_no_dst_w_code %>%
  filter(is.na(player_code == TRUE))


colnames(ffa_no_dst_w_code)
colnames(duplicate_entries)

uncoded_ffa_duplicates <- 
  uncoded_ffa %>%
  inner_join(.,duplicate_entries, by = c("first_name","last_name")) %>%
  arrange(first_name,last_name) %>%
  distinct()

uncoded_ffa_single_entries <-
  uncoded_ffa %>%
  anti_join(.,duplicate_entries, by = c("first_name","last_name")) %>%
  distinct()

# 172, 47, 125

# So there are 172 uncoded 

uncoded_ffa_duplicates %>% arrange(first_name,last_name) %>% print(n = 47)

filter(players, fname == "Charles",lname=="Johnson")

# Remove Alan Cross as RB
# Remove Byron Marshall as a WR
# Remove Dan Vitale as TE
# Remove Dexter McCluster at WR
# Remove Kasen Williams as FA
# Remove Rhett Elison at RB
uncoded_ffa_duplicates <- uncoded_ffa_duplicates[-which(uncoded_ffa_duplicates$player.x == 'Alan Cross' & uncoded_ffa_duplicates$position == 'RB'),]
uncoded_ffa_duplicates <- uncoded_ffa_duplicates[-which(uncoded_ffa_duplicates$player.x == 'Byron Marshall' & uncoded_ffa_duplicates$position == 'WR'),]
uncoded_ffa_duplicates <- uncoded_ffa_duplicates[-which(uncoded_ffa_duplicates$player.x == 'Dan Vitale' & uncoded_ffa_duplicates$position == 'TE'),]
uncoded_ffa_duplicates <- uncoded_ffa_duplicates[-which(uncoded_ffa_duplicates$player.x == 'Dexter McCluster' & uncoded_ffa_duplicates$position == 'WR'),]
uncoded_ffa_duplicates <- uncoded_ffa_duplicates[-which(uncoded_ffa_duplicates$player.x == 'Kasen Williams' & uncoded_ffa_duplicates$team == 'FA'),]
uncoded_ffa_duplicates <- uncoded_ffa_duplicates[-which(uncoded_ffa_duplicates$player.x == 'Rhett Ellison' & uncoded_ffa_duplicates$position == 'RB'),]


uncoded_ffa_duplicates %>% arrange(first_name,last_name) %>% print(n = 45)
filter(players, fname == 'Tony', lname == 'Washington')

colnames(uncoded_ffa_duplicates)
uncoded_ffa_duplicates %<>%
  mutate(player_code = ifelse(player.x == 'Adrian Peterson' & team == 'NO','AP-0700',
                       ifelse(player.x == 'Alan Cross', 'AC-2450',
                       ifelse(player.x == 'Alex Smith' & position == 'QB','AS-1600',
                       ifelse(player.x == 'Alex Smith' & position == 'TE','AS-1700',
                       ifelse(player.x == 'Antonio Brown','AB-3500',
                       ifelse(player.x == 'Austin Johnson','AJ-1125',
                       ifelse(player.x == 'Brandon Marshall','BM-0300',
                       ifelse(player.x == 'Brandon Williams','BW-2030',
                       ifelse(player.x == 'Byron Marshall','BM-0337',
                       ifelse(player.x == 'Cam Newton','CN-0500',        
                       ifelse(player.x == 'Charles Johnson','CJ-1450',
                       ifelse(player.x == 'Chris Harper','CH-1290',
                       ifelse(player.x == 'Chris Johnson','CJ-1700',
                       ifelse(player.x == 'Chris Thompson','CT-1220',
                       ifelse(player.x == 'Corey Fuller','CF-1550',
                       ifelse(player.x == 'Dan Vitale','DB-0550',
                       ifelse(player.x == 'David Johnson' & position == 'RB', 'DJ-1325',
                       ifelse(player.x == 'David Johnson' & position == 'TE', 'DJ-1300',
                       ifelse(player.x == 'Devin Smith','DS-3031',
                       ifelse(player.x == 'Dexter McCluster','DM-1200',        
                       ifelse(player.x == 'Jonathan Stewart','JS-6700',        
                       ifelse(player.x == 'Josh Johnson','JJ-3500',
                       ifelse(player.x == 'Kasen Williams','KW-1175',
                       ifelse(player.x == 'Keith Smith' & position == 'RB', 'KS-1450',
                       ifelse(player.x == 'Kevin Smith' & position == 'RB', 'KS-1700',
                       ifelse(player.x == 'Kevin Smith' & position == 'WR', 'KS-1750',
                       ifelse(player.x == 'Kevin White' & position == 'WR', 'KW-0887',
                       ifelse(player.x == 'Malcolm Johnson' & position == 'RB','MJ-1175',
                       ifelse(player.x == 'Marcus Johnson' & position == 'WR', 'MJ-1450',
                       ifelse(player.x == 'Marvin Jones','MJ-2250',
                       ifelse(player.x == 'Matt Jones','MJ-2275',
                       ifelse(player.x == 'Michael Thomas','MT-0875',
                       ifelse(player.x == 'Mike Williams','Willi03',
                       ifelse(player.x == 'Rod Smith','RS-2100',
                       ifelse(player.x == 'Rhett Ellison','RE-0400',
                       ifelse(player.x == 'Ryan Grant','RG-1650',
                       ifelse(player.x == 'Ryan Griffin' & position == 'TE','RG-1870',
                       ifelse(player.x == 'Ryan Griffin' & position == 'QB','RG-1885',
                       ifelse(player.x == 'Taiwan Jones' & position == 'RB','TJ-2400',
                       ifelse(player.x == 'Tony Washington','TW-1075',
                       ifelse(player.x == 'Will Johnson','WJ-0300',
                       ifelse(player.x == 'Zach Miller','ZM-0200',player_code
                      )))))))))))))))))))))))))))))))))))))))))))




filter(uncoded_ffa_duplicates,is.na(player_code))



# Lots of rookies in here
uncoded_ffa_single_entries %<>% distinct()

filter(uncoded_ffa_single_entries,is.na(player_code)) %>% arrange(last_name,first_name)


filter(players,fname == 'Aaron',lname == 'Jones')

# Hard code these values
# Solution is to list first five letters of last_name,first_name then a number 01,02,etc.

colnames(uncoded_ffa_single_entries)
uncoded_ffa_single_entries %<>% mutate(pos = position)
uncoded_ffa_single_entries %<>%
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

  
  
  
    mutate(player_code = ifelse(player.x == 'AJ McCarron','AM-1150') 
           
                      ifelse(player == 'Alvin Kamara','Kamar01',
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
filter(uncoded_ffa,is.na(player_code)) %>% arrange(player) 



#### Code the defenses
ffa_dst
ffa_dst2  <- ffa_dst %>% mutate(player_code = paste0(player," D/ST"))
ffa_dst2 %>% View()
