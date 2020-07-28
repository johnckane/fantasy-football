players_only <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_0/FFA-CustomRankings_KANE_PPR.csv", 
               stringsAsFactors = FALSE, 
               header = TRUE)
players_only$player_team <- paste0(players_only$playername, " - ",players_only$team)
str(players_only)

players_only <- select(players_only,player_team,position) %>% filter(position %in% c("QB","RB","WR","TE","D/ST","K"))

df1 <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_0/FFA-CustomRankings_KANE_PPR.csv", 
               stringsAsFactors = FALSE, 
               header = TRUE)

df2 <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_0/FFA-CustomRankings_standard.csv", 
               stringsAsFactors = FALSE, 
               header = TRUE)

table(df1$position)
table(df2$position)
library(dplyr)
colnames(df1)
df1 <- df1 %>%
  filter(position %in% c("QB","RB","WR","TE","DST","K")) %>%
  select(1,2,3,4,8,16)
colnames(df2)
df2 <- df2 %>%
  filter(position %in% c("QB","RB","WR","TE","DST","K")) %>%
  select(1,2,3,4,5,9,17)
library(sqldf)
joined <- sqldf("
  select
    a.playerId
,   a.playername
,   a.team
,   a.position
,   a.points/13 as ppr_points
,   a.adp as ppr_adp
,   b.points/13 as standard_points
,   b.adp as standard_adp
from
  df1 as a
, df2 as b
where
  a.playerId = b.playerId
                ")

joined %>% arrange(playerId) %>% group_by(playerId) %>% filter(row_number() == 2)
joined %>% filter(playerId %in% c(2531332,2541429))

#remove both these guys
joined <- joined %>% filter(!playerId %in% c(2531332,2541429))

colnames(joined)
joined$player_team <- paste0(joined$playername, " - ", joined$team)

head(joined$player_team)

colnames(joined)
joined <- joined %>% select(9,4,5,6,7,8)
head(joined)

write.csv(joined, "/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_0/faa_projection_data.csv",row.names = FALSE)

df <- df %>% 
  mutate(adp = ifelse(ppr_adp == "null", 999, as.numeric(ppr_adp))) %>%
  select(1,2,3,7) %>% 
  data.frame() %>% 
  `colnames<-`(c("player_team","position","ppg","adp")) 
  