df

df <- df %>% rename(ppg = standard_points)

drafted_players <- df %>%
  filter(player_team %in% 
           c("Antonio Brown - PIT",
             "Alshon Jeffery - CHI",
             "LeSean McCoy - BUF")) %>%
  select(player_team, position, ppg,bye)

drafted_players
summary(df$bye)

drafted_players <- df[c(3,19,24,30,45,90,27),]

drafted_players
# We only need to do the Lineup Optimizer for weeks 4 - 13, the weeks with BYEs

# we need to expand the drafted players list to include that player
#anti_join ?

library(dplyr)

week <- c(4:13)

week_df <- data.frame(week, dummy = 1)

drafted_players$dummy <- 1

drafted_players_w_weeks <- inner_join(drafted_players, week_df, by = "dummy") %>%
  filter(bye != week)

drafted_players_w_weeks %>%
  group_by(week,position)
#' This function will take the first N at each position each week
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


flex_op_pos <- function(nqb,nrb,nwr,nte,data){
  bind_rows(lo_flex_op("QB",nqb,data),
            lo_flex_op("RB",nrb,data),
            lo_flex_op("WR",nwr,data),
            lo_flex_op("TE",nte,data)) %>%
    group_by(week) %>%
    filter(ppg == max(ppg))
}

library(dplyr)
weekly_lineups <- bind_rows(lo("QB",1,drafted_players_w_weeks),
                            lo("RB",2,drafted_players_w_weeks),
                            lo("WR",2,drafted_players_w_weeks),
                            lo("TE",1,drafted_players_w_weeks),
                            flex_op_pos(0,3,3,2,drafted_players_w_weeks)) %>%
  group_by(week) %>%
  summarise(ttl_points = sum(ppg)) %>%
  arrange(week)

as_is <- weekly_lineups

weekly_lineups

##' This is now a function for adding how many points in a given week this player would add to your lineup,
##' based on the current projected starters (as determined by ppg) 
##' 

added_value <- function(data,new_row,compare_to){
  hypothetical_df <- bind_rows(data %>%
#                                 rename(ppg = standard_points) %>%
                                 full_join(.,week_df, by = "dummy"),
                               new_row %>%
#                                 rename(ppg = standard_points) %>%
                                 mutate(dummy = 1) %>%
                                 full_join(.,week_df, by = "dummy")) %>% 
    data.frame()
  hypothetical_lineup <- bind_rows(lo("QB",1,hypothetical_df),
                                   lo("RB",2,hypothetical_df),
                                   lo("WR",2,hypothetical_df),
                                   lo("TE",1,hypothetical_df),
                                   flex_op_pos(2,3,3,2,hypothetical_df)) %>%
    group_by(week) %>%
    summarise(ttl_points = sum(ppg)) 

    points_added <- (sum(hypothetical_lineup$ttl_points) - sum(compare_to$ttl_points))/13

    return(points_added)
}


added_value(drafted_players, df[250,], weekly_lineups)

recs2 <- df[c(1,10,100),]
recs2$value_added <- rep(0,3)
for(i in 1:3){
recs2$value_added[i] <- added_value(drafted_players,df[i,],weekly_lineups)
}
recs2







