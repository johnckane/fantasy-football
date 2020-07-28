library(dplyr)
library(tidyr)
# This is PPR data
data <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/FFA-CustomRankings.csv", 
                 stringsAsFactors = FALSE, 
                 header = TRUE)
head(data)
str(data)

# we don't need all these variables

data <- select(data, 1,2,3,4,5,8,16,19,20,21)
str(data)

data <- data %>% filter(position %in% c("QB","RB","WR","TE","DST","K"))
data$ppg <- data$points/13
data$adp <- ifelse(data$adp == "null",999,as.numeric(data$adp))
table(data$adp)

# Let's create some players who have been drafted.
# Assume top 45 picks go as indicated
drafted_n0 <- filter(data, adp < 8)
drafted_n1 <- filter(data, adp < 17)
drafted_n2 <- filter(data, adp < 32)

available_n0 <- anti_join(data,drafted_n0, by = "playerId")
available_n1 <- anti_join(data,drafted_n1, by = "playerId")
available_n2 <- anti_join(data,drafted_n2, by = "playerId")

#split by position
n3 <- rbind(
available_n0 %>%
  group_by(position) %>%
  arrange(adp) %>%
  slice(1),
available_n1 %>%
  group_by(position) %>%
  arrange(adp) %>%
  slice(1),
available_n2 %>%
  group_by(position) %>%
  arrange(adp) %>%
  slice(1)
) %>%
  arrange(position,adp) %>%
  mutate(pct_drop = round(100*(ppg - lag(ppg))/ppg,2),
         raw_drop = (ppg-lag(ppg))) %>%
  select (-adp, -playerId,-playerposition,-points,-upper,-lower,-risk) %>%
  group_by(position) %>%
  mutate(record = row_number(),
         player_team = paste0(playername," - ",team))

# get the metrics
recs <-
n3 %>% 
  select(position,ppg,record) %>%
  spread(key = record, value = ppg)  %>%
#  `colnames<-`(c("POS","BA","PPG Next Pick","PPG Two Picks")) %>%
#  select(-BA) %>%
  inner_join(
    n3 %>%
    select(position,pct_drop, record) %>%
    spread(key = record, value = pct_drop), #%>%
#    `colnames<-`(c("POS","BA","% Drop Next Pick","% Drop Two Picks")) %>%
#    select(-BA),
    by = "position") %>%
  inner_join(
    n3 %>%
    select(position,raw_drop,record) %>%
    spread(key = record, value = raw_drop), # %>%
#    `colnames<-`(c("POS","BA","Raw Drop Next Pick","Raw Drop Two Picks")) %>%
#    select(-BA), 
    by = "position") %>%
  inner_join(
    n3 %>%
    select(position,player_team,record) %>%
    spread(key = record, value = player_team),  #%>%
#    `colnames<-`(c("POS","BA","BA - 1 Pick","BA 2 Picks")) %>%
#      select(-BA),
    by = "position")

recs


recs_formatted <- recs %>%
  select(1,11,2,12,3,6,9) %>%
  `colnames<-`(c("POS","Best Available","PPG","Best Available Next Pick","PPG","% Drop","Drop"))
recs_formatted