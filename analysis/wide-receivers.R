#library(RMySQL)
library(tidyverse)
library(lubridate)
#mydb = dbConnect(MySQL(), user='john', dbname='armchair_analysis', host='localhost')
library(DBI)
library(pool)
pool <- dbPool(drv = RMySQL::MySQL(), 
               dbname = "armchair_analysis", 
               host = "localhost", 
               username = "john", 
               port = 3306, 
               unix.sock = "/var/run/mysqld/mysqld.sock")
team_name_abbr_lookup <- data.frame(abbr = c('ARI','ATL','BAL','BUF','CAR','CHI','CIN','CLE','DAL','DEN',
                                             'DET','GB' ,'HOU','IND','JAC','KC' ,'LA' ,'MIA','MIN','NE',
                                             'NO' ,'NYG','NYJ','OAK','PHI','PIT','SD' ,'SEA','SF' , 'STL',
                                             'TB','TEN','WAS'),
                                    name = c('Cardinals','Falcons','Ravens','Bills','Panthers','Bears','Bengals','Browns',
                                             'Cowboys','Broncos','Lions','Packers','Texans','Colts','Jaguars','Chiefs','Rams',
                                             'Dolphins','Vikings','Patriots','Saints','Giants','Jets','Raiders','Eagles',
                                             'Steelers','Chargers','Seahawks','49ers','Rams','Buccaneers','Titans','Redskins'),
                                    stringsAsFactors = FALSE)

offense <- dbGetQuery(con = pool,
                      "SELECT 
                      a.player
                      , b.wk as week
                      , a.year
                      , a.team
                      , 0.04*py + -2*ints + 4*tdp + 0.1*ry + 0.1*recy + 6*tdr + 6*tdrec -2*fuml +6*tdret as fp
                      FROM offense as a
                      JOIN game as b on a.gid = b.gid") 


two_point <- dbGetQuery(con = pool,
                        "select 
                        a.*
                        , b.gid
                        , c.seas as year
                        , c.wk as week 
                        from conv as a 
                        join play as b on a.pid = b.pid
                        join game as c on b.gid = c.gid where conv = 1")


rush_two_point <- two_point %>%
  filter(bc != "")
pass_two_point <- two_point %>%
  filter(psr != "")
two_point %>%
  filter(bc == "", psr == "") # forget this one


offense2 <-
  offense %>%
  left_join(., 
            pass_two_point %>% group_by(week,year,psr) %>% summarise(count = n()), 
            by = c("year","week","player" = "psr")) %>% 
  mutate(fp = ifelse(is.na(count) == FALSE, fp + 2*count, fp)) %>%
  select(-count)

offense3 <-
  offense2 %>%
  left_join(.,
            pass_two_point %>% group_by(week,year,trg) %>% summarise(count= n()),
            by = c("year","week","player" = "trg")) %>%
  mutate(fp = ifelse(is.na(count) == FALSE, fp+2*count, fp)) %>%
  select(-count)

offense4 <-
  offense3 %>%
  left_join(.,
            rush_two_point %>% group_by(week,year,bc) %>% summarise(count = n()),
            by = c("year","week","player" = "bc")) %>%
  mutate(fp = ifelse(is.na(count) == FALSE, fp+2*count, fp)) %>%
  select(-count)

offense5 <- offense4 %>%
  select(player,year,week,team,fp) %>%
  rename(points = fp)

# Need names, teams, positions
wrs <- dbGetQuery(con = pool,
                  "select 
                      a.player
                      , a.fname
                      , a.lname
                      , a.dob
                      , a.start
                      from player as a
                      where pos1 = 'WR'")

wr_offense <- inner_join(wrs,offense5,by='player') %>% mutate(name = paste0(fname," ",lname),
                                                              experience = year - start,
                                                              age = year - year(as_date(dob)))
save(wr_offense, file = "/home/john/projects/fantasy-football/data/analysis-data/wr_offense.Rda")
head(wr_offense)


wr_first8_ranks <-
wr_offense %>%
  filter(week <= 8) %>%
  group_by(player,name,year,team,experience,age) %>%
  summarise(total_points = sum(points)) %>%
  ungroup() %>%
  group_by(team,year) %>%
  arrange(desc(total_points)) %>%
  mutate(team_rank_first8 = row_number()) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(desc(total_points)) %>%
  mutate(league_rank_first8 = row_number()) %>%
  select(-total_points)

wr_last8_ranks <-
  wr_offense %>%
  filter(week >  8,
         week <= 16) %>%
  group_by(player,name,year,team,experience,age) %>%
  summarise(total_points = sum(points)) %>%
  ungroup() %>%
  group_by(team,year) %>%
  arrange(desc(total_points)) %>%
  mutate(team_rank_last8 = row_number()) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(desc(total_points)) %>%
  mutate(league_rank_last8 = row_number()) %>%
  select(-total_points)




###
wr <-
wr_offense %>%
  filter(week <= 16) %>%
  group_by(player,name,year,team,experience,age) %>%
  summarise(total_points = sum(points)) %>%
  ungroup() %>%
  group_by(team,year) %>%
  arrange(desc(total_points)) %>%
  mutate(team_rank = row_number()) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(desc(total_points)) %>%
  mutate(league_rank = row_number()) %>%
  left_join(wr_first8_ranks, by = c('player','name','year','team','experience','age')) %>%
  left_join(wr_last8_ranks, by = c('player','name','year','team','experience','age'))

head(wr)

save(wr, file = "/home/john/projects/fantasy-football/data/analysis-data/wr.Rda")

head(wr)
