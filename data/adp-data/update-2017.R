dlibrary(RMySQL)
library(tidyverse)

load("/home/john/everything/2017/adp2017_final.Rda")
load("/home/john/fantasy-football/coded_adp_data_2010_2016.Rda")

adp <- bind_rows(coded_adp_data2,adp2017_final2) %>% select(-team_name, -city)

mydb = dbConnect(MySQL(), user='root', password='cz14F4b12', dbname='armchair_analysis', host='localhost')
players <- dbGetQuery(con = mydb,
                      "select player, fname, lname, pos1
                       from player")


head(players)
players %>% filter(lname == 'Kamara')

## Which players don't have an AA code? And are not D/ST

no_aa_code <- filter(adp, grepl("-",player_code) == FALSE) %>% filter(.,grepl("D/ST",player_code) == FALSE)
aa_code <- filter(adp,grepl("-",player_code) == TRUE)
dst_code <- filter(adp,grepl("D/ST",player_code))

no_aa_code$firstname <- sapply(str_split(no_aa_code$player,' '), '[[', 1)      
no_aa_code$lastname  <- sapply(str_split(no_aa_code$player,' '), '[[', 2)      

View(no_aa_code)

no_aa_code %<>%
  left_join(players %>% select(fname,lname,player), by = c("firstname" = "fname",
                                                          "lastname" = "lname"))
coded <- filter(no_aa_code, is.na(player.y)==FALSE)
uncoded <- filter(no_aa_code, is.na(player.y))
uncoded
players %>% filter(lname == 'Prosise')
players %>% filter(lname == 'Carson')
players %>% filter(lname == 'Lutz')
## Steps:
## 1) code players
## 2) join uncoded with coded with aa_code 
## 2a) make sure D/ST is in there
## 3) join all adp data together in one dataset

?dplyr::case_when
uncoded %<>% mutate(player_code = case_when(player.x == 'CJ Prosise' ~ 'CP-2550',
                                            player.x == 'Donta Foreman' ~ 'DF-1362',
                                            player.x == 'Christopher Carson' ~ 'CC-0750',
                                            player.x == 'Wil Lutz' ~ 'WL-0300'))

adp <- bind_rows(uncoded,coded,aa_code,dst_code)
dim(adp)[1] == dim(adp2017)[1]  
adp %<>% select(1,2,3,4,6,7,8,9,13)

# We need values that capture overal rank as well as position_adp
adp %<>%
  arrange(year,overall) %>%
  group_by(year,pos) %>%
  mutate(pos_adp = row_number()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(overall_adp = row_number())


save(adp, file = "/home/john/fantasy-football/data/adp-data/adp.Rda")
  
