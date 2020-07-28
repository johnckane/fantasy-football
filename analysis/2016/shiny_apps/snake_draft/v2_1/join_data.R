players_only <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_1/kane_ppr_20160904.csv", 
               stringsAsFactors = FALSE, 
               header = TRUE)
players_only$player_team <- paste0(players_only$playername, " - ",players_only$team)
str(players_only)

players_only <- select(players_only,player_team,position) %>% filter(position %in% c("QB","RB","WR","TE","D/ST","K"))

df1 <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_1/kane_ppr_20160904.csv", 
               stringsAsFactors = FALSE, 
               header = TRUE)

df2 <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_1/espn_standard_20160904.csv", 
               stringsAsFactors = FALSE, 
               header = TRUE)

table(df1$position)
table(df2$position)
library(dplyr)
colnames(df1)
df1 <- df1 %>%
  filter(position %in% c("QB","RB","WR","TE","DST","K")) %>%
  select(1,2,3,4,7,15)
colnames(df2)
df2 <- df2 %>%
  filter(position %in% c("QB","RB","WR","TE","DST","K")) %>%
  select(1,2,3,4,5,7,15)
library(sqldf)
joined <- sqldf("
  select
    a.playername
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
  a.playername = b.playername
and a.team = b.team
and a.position = b.position
                ")
#remove both these guys


colnames(joined)
joined$player_team <- paste0(joined$playername, " - ", joined$team)

joined %>% group_by(player_team) %>% filter(row_number() >=  2)

joined <- joined %>%
  filter(!player_team %in% c("Terrelle Pryor - CLE",
                             "Mike Thomas - LA",
                             "Chris Harper - FA",
                             "Kasen Williams - FA",
                             "B.J. Daniels - FA"))

head(joined$player_team)

colnames(joined)
joined <- joined %>% select(8,3,4,5,6,7)
head(joined)

write.csv(joined, "/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_1/faa_projection_data.csv",row.names = FALSE)

df <- df %>% 
  mutate(adp = ifelse(ppr_adp == "null", 999, as.numeric(ppr_adp))) %>%
  select(1,2,3,7) %>% 
  data.frame() %>% 
  `colnames<-`(c("player_team","position","ppg","adp")) 
  