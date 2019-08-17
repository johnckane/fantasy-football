library(tidyverse)
library(lubridate)
library(DBI)
library(pool)


pool <- dbPool(drv = RMySQL::MySQL(), 
               dbname = "armchair_analysis", 
               host = "localhost", 
               username = "john", 
               port = 3306, 
               unix.sock = "/var/run/mysqld/mysqld.sock")

load("/home/john/projects/fantasy-football/data/adp-data/adp.Rda")
load("/home/john/projects/fantasy-football/data/adp-data/adp_2018.Rda")

head(adp)
head(adp_2018)


players <- dbGetQuery(con = pool, "SELECT player, fname, lname, pos1 FROM player")

head(adp)

head(adp_2018)

# which values of adp_2018$player DON'T have a "-" in the third position?
adp_2018 %>%
  filter(str_sub(player_code,3,3) != '-' & pos != 'D/ST')

# Find these values

recode <-
  adp_2018 %>%
  filter(str_sub(player_code,3,3) != '-' & pos != 'D/ST')

dont_recode <- 
  adp_2018 %>%
  filter(str_sub(player_code,3,3) == '-' | pos == 'D/ST')

211 == (194 + 17)

recode$firstname <- sapply(str_split(recode$player,' '), '[[', 1)      
recode$lastname  <- sapply(str_split(recode$player,' '), '[[', 2)     

recode

recode2 <-
  recode %>%
  left_join(players, by = c("firstname" = "fname", "lastname" = "lname")) %>%
  select(-player_code) %>%
  rename(player_code = player.y)

colnames(recode2)

recoded <-
  recode2 %>%
  select(1:9,17) %>%
  bind_rows(.,
            dont_recode %>%
              select(1:9,13))



recoded2 <-
  recoded %>%
  arrange(overall) %>%
  mutate(overall_adp = row_number()) %>%
  group_by(pos) %>%
  arrange(overall) %>%
  mutate(pos_adp = row_number())

adp_2018_cleaned <- recoded2

save(adp_2018_cleaned, 
     file = "/home/john/projects/fantasy-football/data/adp-data/adp_2018_cleaned.Rda")

head(adp)
head(adp_2018_cleaned)

new_adp <- 
  bind_rows(adp,
            adp_2018_cleaned %>%
              select(1,2,3,4,6,7,8,9,10,12,13))

dim(adp)
dim(adp_2018_cleaned)
dim(new_adp)

2276 == (211 + 2065)

sum(is.na(new_adp$player_code))
sum(is.na(new_adp$overall_adp))
sum(is.na(new_adp$pos_adp))

View(new_adp)

adp <- new_adp
save(adp, 
     file = "/home/john/projects/fantasy-football/data/adp-data/adp.Rda")
