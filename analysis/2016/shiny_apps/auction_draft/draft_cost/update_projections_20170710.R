#' This is updating old data for the sake of making the app.
faa_update <- read.csv("/home/john/Downloads/FFA-CustomRankings.csv",
                       stringsAsFactors = F,
                       header = T,
                       row.names = NULL)

faa_update
str(faa_update)
head(faa_update)
library(tidyverse)

data <- faa_update %>% select(player,playername,position,vor) %>%
  rename(position = playername,
         team = position,
         points = vor) 
data$pred_cost = rnorm(913,40,20)
data$pred_cost <- ifelse(data$pred_cost < 1,1,data$pred_cost)
data$pred_cost <- floor(data$pred_cost)
hist(data$pred_cost)

data %<>% mutate(pred_cost2 = pred_cost,
                 ppg = points/13) %>%
          rename(pos = position) %>%
          select(player,pos,pred_cost,pred_cost2,ppg)

write.csv()

fa2 <- faa_update %>%
  filter(position %in% c("QB","RB","WR","TE")) %>%
  mutate(ppg = vor/13) %>%
  rename(player = player,
         pos = team) %>%
  select(-points)

fa2

fa2$player <- ifelse(fa2$player == "C.J. Prosise",
                     "CJ Prosise",
                     fa2$player)
fa2$player <- ifelse(fa2$player == "LeVeon Bell",
                     "Le'Veon Bell",
                     fa2$player)
fa2$player <- ifelse(fa2$player == "Odell Beckham Jr ",
                     "Odell Beckham Jr",
                     fa2$player)
fa2$player <- ifelse(fa2$player == "Steve Smith",
                     "Steve Smith Sr.",
                     fa2$player)
fa2$player <- ifelse(fa2$player == "Robert Griffin",
                     "Robert Griffin III",
                     fa2$player)   
fa2$player <- ifelse(fa2$player == "Ted Ginn",
                     "Ted Ginn Jr",
                     fa2$player)   

library(sqldf)
install.packages("sqldf")

full_data3 <- sqldf("select
                        a.player
                     ,  a.pos
                     ,  a.pred_cost
                     ,  a.pred_cost2
                     ,  b.ppg
                     from
                      full_data2 as a
                     ,fa2 as b
                     where
                      a.player = b.player
                    and a.pos = b.pos")
full_data3 <- full_data3 %>% filter(is.na(ppg) == F)

write.csv(data, "/home/john/stats_corner/2016/shiny_apps/auction_draft/app/data2017test.csv",
          row.names = F)
