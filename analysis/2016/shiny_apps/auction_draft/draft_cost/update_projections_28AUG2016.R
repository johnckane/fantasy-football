faa_update <- read.csv("/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/faa_projections_upate_25AUG2016.csv",
                       stringsAsFactors = F,
                       header = T)


fa2 <- faa_update %>%
  filter(position %in% c("QB","RB","WR","TE")) %>%
  mutate(ppg = points/13) %>%
  rename(player = playername,
         pos = position) %>%
  select(-points)

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

write.csv(full_data3, "/home/john/stats_corner/2016/shiny_apps/auction_draft/app/full_data3.csv",
          row.names = F)
