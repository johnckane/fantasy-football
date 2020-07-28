library(RMySQL)
library(tidyverse)
mydb = dbConnect(MySQL(), user='root', password='cz14F4b12', dbname='armchair_analysis', host='localhost')
team_name_abbr_lookup <- data.frame(abbr = c('ARI','ATL','BAL','BUF','CAR','CHI','CIN','CLE','DAL','DEN',
                                             'DET','GB' ,'HOU','IND','JAC','KC' ,'LA' ,'MIA','MIN','NE',
                                             'NO' ,'NYG','NYJ','OAK','PHI','PIT','SD' ,'SEA','SF' , 'STL',
                                             'TB','TEN','WAS'),
                                    name = c('Cardinals','Falcons','Ravens','Bills','Panthers','Bears','Bengals','Browns',
                                             'Cowboys','Broncos','Lions','Packers','Texans','Colts','Jaguars','Chiefs','Rams',
                                             'Dolphins','Vikings','Patriots','Saints','Giants','Jets','Raiders','Eagles',
                                             'Steelers','Chargers','Seahawks','49ers','Rams','Buccaneers','Titans','Redskins'),
                                    stringsAsFactors = FALSE)
offense <- dbGetQuery(con = mydb,
                      "select 
              a.player
            , b.wk as week
            , a.year
            , a.team
            , 0.04*py + -2*ints + 4*tdp + 0.1*ry + 0.1*recy + 6*tdr + 6*tdrec -2*fuml +6*tdret as fp
            from offense as a
            join game as b on a.gid = b.gid") 
two_point <- dbGetQuery(con = mydb,
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

kicking <- dbGetQuery(con = mydb,
                      "
                      select
                       a.fkicker as player
                      ,b.seas as year
                      ,b.wk as week
                      ,sum(a.pts) as points
                      from
                      (select 
                        a1.pid
                      , b1.gid
                      , a1.fkicker
                      , case when a1.fgxp = 'XP' and a1.good = 1 then 1 
                             when a1.fgxp = 'FG' and a1.good = 1 and a1.dist <= 39 then 3
                             when a1.fgxp = 'FG' and a1.good = 1 and a1.dist >= 40 and dist <= 49 then 4
                             when a1.fgxp = 'FG' and a1.good = 1 and a1.dist >= 50 then 5
                             when a1.fgxp = 'FG' and a1.good = 0 then -1
                             else 0
                        end as pts
                        from fgxp as a1
                        join play as b1 on a1.pid = b1.pid) as a
                     join game as b on a.gid = b.gid
                     group by 1,2,3
                      ")
defense <- dbGetQuery(con = mydb,
                      "
                      select
                       a.gid
                      ,a.team
                      ,a.year
                      ,sum(a.pts) as fp
                      from
                      (select
                       a1.gid
                      ,a1.team
                      ,a1.year
                      ,a1.sck + 6*a1.tdret + 2*a1.ints + 2*a1.saf +2*a1.blk +2*a1.frcv +6*a1.tdd as pts
                      from defense as a1) as a
                      group by 1,2,3")
all_points <- dbGetQuery(con = mydb,
                         "select seas, wk, gid, h, ptsv as pa_h, v, ptsh as pa_v
                         from game")

pa <-
  all_points %>%
  select(seas,wk,gid,h,pa_h) %>%
  rename(team = h, pa = pa_h) %>%
  union(.,all_points %>%
          select(seas,wk,gid,v,pa_v) %>%
          rename(team = v, pa = pa_v)
  ) %>%
  mutate(pts = ifelse(pa == 0, 5,
                      ifelse(pa >=1 & pa <=6, 4,
                             ifelse(pa >= 7 & pa <= 13,3,
                                    ifelse(pa >= 14 & pa <= 17,1,
                                           ifelse(pa >= 28 & pa <= 34, -1,
                                                  ifelse(pa >= 35 & pa <= 45, -3,
                                                         ifelse(pa >= 46, -5, 0))))))))
ty <- dbGetQuery(con = mydb,
                 "select
                    gid
                  , tid
                  , tname
                  , ry + py as ty
                  from team")
tya <- 
  ty %>%
  group_by(gid) %>%
  mutate(tya = ifelse(row_number() == 1, lead(ty), lag(ty))) %>%
  select(gid,tname,tya) %>%
  mutate(fpts = ifelse(tya < 100, 5,
                       ifelse(tya >= 100 & tya <= 199, 3,
                              ifelse(tya >= 200 & tya <= 299, 2,
                                     ifelse(tya >= 350 & tya <= 399,-1,
                                            ifelse(tya >= 400 & tya <= 449, -3,
                                                   ifelse(tya >= 450 & tya <= 499,-5,
                                                          ifelse(tya >= 500 & tya <= 549, -6,
                                                                 ifelse(tya >= 550, -7,0)))))))))
total_defense <- 
  defense %>%
  select(gid,team,fp) %>%
  inner_join(pa %>% select(gid,team,pts),
             by = c("gid","team")) %>%
  inner_join(tya %>% select(-tya),
             by = c("gid","team" = "tname")) %>%
  ungroup() %>%
  mutate(points = fp + pts + fpts) %>%
  select(-fp,-pts,-fpts)
coach <- dbGetQuery(con = mydb,
                    "select
                      gid
                    , seas
                    , wk
                    , h as coach
                    , case when (ptsh - ptsv) >= 25 then 3
                           when (ptsh - ptsv) between  20 and  24 then 1
                           when (ptsh - ptsv) between  15 and  19 then 1
                           when (ptsh - ptsv) between -19 and -15 then -1
                           when (ptsh - ptsv) between -24 and -20 then -1
                           when (ptsh - ptsv) <= -25 then -3 
                           else 0 end as pts
                    from game
                    union
                    select
                      gid
                    , seas
                    , wk
                    , v as coach
                    , case when (ptsv - ptsh) >= 25 then 3
                           when (ptsv - ptsh) between  20 and  24 then 1
                           when (ptsv - ptsh) between  15 and  19 then 1
                           when (ptsv - ptsh) between -19 and -15 then -1
                           when (ptsv - ptsh) between -24 and -20 then -1
                           when (ptsv - ptsh) <= -25 then -3 
                           else 0 end as pts
                    from game")
offense5 <- offense4 %>%
  select(player,year,week,fp) %>%
  rename(points = fp)
total_defense2 <- total_defense %>%
  left_join(.,
            dbGetQuery(con = mydb,
                       "select gid, seas as year, wk as week from game"),
            by = c("gid")) %>%
  inner_join(.,team_name_abbr_lookup, by = c("team" = "abbr")) %>%
  mutate(player = paste0(name," D/ST")) %>%
  select(player,year,week,points)
coach2 <-
  coach %>%
  rename(year = seas,
         week = wk,
         points = pts) %>%
  inner_join(.,team_name_abbr_lookup, by = c("coach" = "abbr")) %>%
  mutate(player = paste0(name," HC")) %>%
  select(player, year, week, points)
points_week_data <- rbind(offense5,
                          kicking,
                          total_defense2,
                          coach2) %>%
  group_by(player,week,year) %>%
  summarise(points = sum(points)) %>%
  ungroup()

# Need to join this with position data
dbListFields(conn = mydb, "player")

player_pos <- dbGetQuery(conn = mydb,
                         "select player, pos1, yob from player")

points_week_pos <-
  left_join(points_week_data,player_pos, by = c("player")) %>%
  rename(pos = pos1) %>%
  mutate(age = year - yob) %>%
  filter(pos %in% c("QB","RB","WR","TE","K"))

head(points_week_pos)

points_year_pos <- 
  points_week_pos %>%
  filter(week %in% c(1:16)) %>% # only care about fantasy season!
  group_by(player,pos,age,year) %>%
  summarise(total_points = sum(points)) %>%
  ungroup() %>%
  group_by(pos,year) %>%
  arrange(desc(total_points)) #%>%
  slice(1:40)
  
  
  points_year_pos_top40 <- 
    points_week_pos %>%
    filter(week %in% c(1:16)) %>% # only care about fantasy season!
    group_by(player,pos,age,year) %>%
    summarise(total_points = sum(points)) %>%
    ungroup() %>%
    group_by(pos,year) %>%
    arrange(desc(total_points)) %>%
    slice(1:40)

head(points_year_pos)
library(ggplot2)

plot <- ggplot(data = points_year_pos,
               aes(x = age, y = total_points)) 
plot +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~pos,ncol = 5) +
  geom_vline(xintercept = c(25,30,35), colour = 'red') +
  ggtitle("Total Fantasy Points by Position and Age",subtitle = "Since 2000")



plot2 <- ggplot(data = points_year_pos %>% filter(year >= 2010),
                aes(x = age, y = total_points)) 
plot2 +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~pos,ncol = 5) +
  geom_vline(xintercept = c(25,30,35), colour = 'red') +
  geom_point(data = points_year_pos_top40 %>% filter(year >= 2010),
             aes(x = age,y = total_points), colour = 'green', alpha = 0.2) +
  geom_smooth(data = points_year_pos_top40 %>% filter(year >= 2010),
              aes(x = age,y = total_points), colour = 'green') +
  geom_hline(data = data.frame(pos = c("K","RB","WR","TE","QB"),
                               total_points = c(128,200,192,160,280)),
             aes(yintercept = total_points)) +
  ggtitle("Total Fantasy Points by Position and Age",subtitle = "Since 2010") 
