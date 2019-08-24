data_file <- "/home/john/projects/fantasy-football/data/FFA/ffa_customrankings2018-NPPR.csv"

library(tidyverse)
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


pool <- dbPool(drv = RMySQL::MySQL(), 
               dbname = "armchair_analysis", 
               host = "localhost", 
               username = "john", 
               port = 3306, 
               unix.sock = "/var/run/mysqld/mysqld.sock")
players <- dbGetQuery(con = pool,
                      "select player as player_code, fname, lname, pos1, dob, nflid
                      from player")

ffa_no_dst_players <- ffa_no_dst %>% ungroup() %>% group_by(player, first_name, last_name, playerId) %>% slice(1) %>% select(player, first_name, last_name, playerId)

duplicate_entries <- 
  ffa_no_dst %>%
  #left_join(.,players, by = c("first_name" = "fname","last_name" = "lname")) %>%
  left_join(.,players, by = c("playerId" = "nflid")) %>%
  group_by(player,first_name,last_name,playerId) %>%
  summarise(count = n()) %>%
  filter(count > 1)

single_entries <- 
  ffa_no_dst %>% 
  anti_join(.,duplicate_entries, by = c("playerId"))



coded_single_entries <-
  single_entries %>%
  left_join(.,players,by = c("playerId" = "nflid")) %>%
  filter(is.na(player_code) == FALSE)


uncoded_single_entries <-
  single_entries %>%
  left_join(.,players,by = c("playerId" = "nflid")) %>%
  filter(is.na(player_code) == TRUE)


dbGetQuery(con = pool, "select player, fname, lname, nflid from player where lname = 'Zylstra'")

uncoded_single_entries %<>%
  mutate(player_code = case_when(player == 'AJ McCarron' ~ 'AM-1150',
                                 player == 'Donnel Pumphrey' ~ 'DP-2400',
                                 player == 'Joel Bouagnon' ~ 'JB-4575',
                                 player == 'River Cracraft' ~ 'Cracr01',
                                 player == 'Brandon Zylstra' ~ 'Zylst01',
                                 TRUE ~ paste0(str_sub(paste0(last_name,first_name),1,5),'01')))

sum(is.na(uncoded_single_entries$player_code))


# Get the position from players in AA

duplicate_entries_w_pos <-
duplicate_entries %>%
  select(playerId) %>%
  inner_join(players, by = c("playerId" = "nflid")) %>%
  inner_join(ffa_no_dst %>% select("playerId","position"), by = "playerId") %>%
  filter(pos1 == position) %>%
  distinct()



coded_duplicate_entries <-
  duplicate_entries_w_pos %>%
  inner_join(ffa_no_dst, by = c("playerId","position")) %>% 
  inner_join(players, by = c("playerId" = "nflid")) %>%
  rename(player_code = player_code.x,
         player = player.x)
  

ffa_no_dst_w_code <- bind_rows(coded_single_entries    %>% ungroup() %>% select(player_code,playerId,player,team,position,age,bye,points),
                               uncoded_single_entries  %>% ungroup() %>% select(player_code,playerId,player,team,position,age,bye,points),
                               coded_duplicate_entries %>% ungroup() %>% select(player_code,playerId,player,team,position,age,bye,points))
                                                                                                                                                                                                                               
#### Code the defenses
ffa_dst_w_code <- ffa_dst %>% mutate(player_code = paste0(player," D/ST")) %>% select(player_code,playerId,player,team,position,age,bye,points)

ffa_nppr <- bind_rows(ffa_no_dst_w_code, ffa_dst_w_code)

ffa_nppr %<>%
  mutate(age = case_when(player == 'Kenyan Drake' ~ 24,
                         player == 'Marlon Mack' ~ 22,
                         player == 'Jamaal Williams' ~ 23,
                         player == 'Sterling Shepard' ~ 24,
                         player == 'Patrick Mahomes' ~ 22,
                         player == 'Josh Doctson' ~ 25,
                         player == 'Tyrell Williams' ~ 26,
                         player == 'Cameron Meredith' ~ 25,
                         player == 'Austin Hooper' ~ 23,
                         player == 'Steven Hauschka' ~ 33,
                         TRUE ~ as.numeric(age)))

ffa_nppr %>% filter(is.na(age)) %>% mutate(ppg = points/16) %>% arrange(desc(ppg)) %>% filter(!(position %in% c('TE','K','HC','D/ST')))

save(ffa_nppr, file = "/home/john/fantasy-football/data/FFA/ffa_nppr.Rda")
