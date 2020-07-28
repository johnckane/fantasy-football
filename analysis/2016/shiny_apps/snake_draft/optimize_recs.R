library(stringr)
library(dplyr)
df <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_1/faa_projection_data.csv",
               #df <- read.csv("/srv/shiny-server/stats-corner/2016/snake-assistant/faa_projection_data.csv",
               stringsAsFactors = FALSE,
               header = TRUE)
df_team <- str_split(df$player_team, " - ")
df_team2 <- sapply(df_team,"[[",2)
df$team <- df_team2

team <- c("GB","PHI","JAC","KC","NO","SEA","MIN","TB","CAR","DAL","BAL",
          "LA","MIA","NYG","PIT","SF","ARI","CHI","CIN","HOU","NE","WAS",
          "BUF","DET","IND","OAK","ATL","DEN","NYJ","SD","CLE","TEN")
bye <- c(4,4,5,5,5,5,6,6,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,11,11,11,11,13,13)

team_byes <- data.frame(team,bye,stringsAsFactors = F)


df <- left_join(df,team_byes, by = "team") %>% select(-team)

# Needed for the lineup optimzier:
week <- c(1:13)
week_df <- data.frame(week, dummy = 1)

#' These are functions used to do the lineup optimizer:
lo <- function(pos,num,data){
  data %>%
    filter(position == pos) %>%
    group_by(week) %>%
    arrange(desc(ppg)) %>%
    slice(1:num)          
}

lo_flex_op <- function(pos,num,data){
  data %>%
    filter(position == pos) %>%
    group_by(week) %>%
    arrange(desc(ppg)) %>%
    slice(num)
}


flex_op_pos <- function(nqb,nrb,nwr,nte,nk,ndst,data){
  bind_rows(lo_flex_op("QB",  nqb, data),
            lo_flex_op("RB",  nrb, data),
            lo_flex_op("WR",  nwr, data),
            lo_flex_op("TE",  nte, data),
            lo_flex_op("K",    nk, data),
            lo_flex_op("DST",ndst, data)) %>%
    group_by(week) %>%
    filter(ppg == max(ppg))
}

#' This function calculates season per game value added:
added_value <- function(data,new_row,compare_to,nqb,nrb,nwr,nte,nk,ndst){
  hypothetical_df <- bind_rows(data,# %>%
                               #                              rename(ppg = standard_points),# %>%
                               #                               full_join(.,week_df, by = "dummy"),
                               new_row %>%
                                 rename(ppg = standard_points) %>% #
                                 mutate(dummy = 1) %>%
                                 # right_join(.,week_df, by = "dummy") %>%
                                 full_join(.,week_df, by = "dummy") %>%
                                 mutate(ppg = ifelse(bye == week, 0, ppg)) %>% 
                                 data.frame()
  )
  hypothetical_lineup <- bind_rows(lo("QB",   nqb, hypothetical_df),
                                   lo("RB",   nrb, hypothetical_df),
                                   lo("WR",   nwr, hypothetical_df),
                                   lo("TE",   nte, hypothetical_df),
                                   lo("K",     nk, hypothetical_df),
                                   lo("DST", ndst, hypothetical_df),
                                   flex_op_pos(nqb + 1,
                                               nrb + 1,
                                               nwr + 1,
                                               nte + 1,
                                               nk  + 1,
                                               ndst + 1,
                                               hypothetical_df)) %>%
    group_by(week) %>%
    summarise(ttl_points = sum(ppg)) 
  
  points_added <- (sum(hypothetical_lineup$ttl_points) - sum(compare_to$ttl_points))/13
  
  return(points_added)
}
drafted <- df[c(5,10,15,20,50),]
colnames(drafted)
drafted_players_w_weeks <- 
    drafted %>%
    mutate(dummy = 1) %>%
    rename(ppg = standard_points) %>%
    full_join(., week_df, by = "dummy") %>%
    mutate(ppg = ifelse(bye == week, 0, ppg))


lineup_optimizer <-
    bind_rows(lo("QB",1,drafted_players_w_weeks),
              lo("RB",2,drafted_players_w_weeks),
              lo("WR",3,drafted_players_w_weeks),
              lo("TE",1,drafted_players_w_weeks),
              lo("K", 1,drafted_players_w_weeks),
              lo("DST",1,drafted_players_w_weeks),
              flex_op_pos(0,
                          2,
                          3,
                          1,
                          1,
                          1,
                          drafted_players_w_weeks)) %>%
      group_by(week) %>%
      summarise(ttl_points = sum(ppg)) %>%
      arrange(week)
  
lineup_optimizer
  
  

recs <- df %>%
  group_by(position) %>%
  arrange(-standard_points) %>%
  slice(1)


recs_alt <- recs %>% data.frame()
  
recs_alt$VALUE_ADDED <- rep(0,dim(recs_alt)[1])
recs_alt  

for(i in 1:dim(recs_alt)[1]){
 recs_alt$VALUE_ADDED[i] <- added_value(drafted_players_w_weeks,
                                         recs_alt[i,] %>% data.frame(),
                                         lineup_optimizer %>%
                                           data.frame() %>%
                                           mutate(ttl_points = round(ttl_points,1)) %>%
                                         data.frame(),
                                       1,2,3,1,1,1
  )
}

