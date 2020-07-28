library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(pryr) # for debugging

df <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_1/faa_projection_data.csv",
               #df <- read.csv("/srv/shiny-server/stats-corner/2016/snake-assistant/faa_projection_data.csv",
               stringsAsFactors = FALSE,
               header = TRUE)
df$ppr_adp      <- ifelse(df$ppr_adp == "null", 999, as.numeric(df$ppr_adp))
df$standard_adp <- ifelse(df$standard_adp == "null",999, as.numeric(df$standard_adp))                       
df_team <- str_split(df$player_team, " - ")
df_team2 <- sapply(df_team,"[[",2)
df$team <- df_team2 %>% str_trim()

team <- c("GB","PHI","JAC","KC","NO","SEA","MIN","TB","CAR","DAL","BAL",
          "LA","MIA","NYG","PIT","SF","ARI","CHI","CIN","HOU","NE","WAS",
          "BUF","DET","IND","OAK","ATL","DEN","NYJ","SD","CLE","TEN")
bye <- c(4,4,5,5,5,5,6,6,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,11,11,11,11,13,13)

team_byes <- data.frame(team,bye,stringsAsFactors = F)


df <- left_join(df,team_byes, by = "team") #%>% select(-team)
colnames(df)
df <- df %>% 
  mutate(adp = ppr_adp) %>%
  select(1,2,3,9,8,7) %>% 
  data.frame() %>% 
  `colnames<-`(c("player_team","position","ppg","adp","bye","team")) 
# Needed for the lineup optimzier:
week <- c(1:13)
week_df <- data.frame(week, dummy = 1)

input <- list()
input$your_team <- NULL
input$num_qb <- 1
input$num_rb <- 2
input$num_wr <- 2
input$num_te <- 1
input$num_k <- 1
input$num_dst <- 1
lo_df <- data.frame(position = c("QB","RB","WR","TE","K","DST"),
                    lim = c(input$num_qb,
                            input$num_rb,
                            input$num_wr,
                            input$num_te,
                            input$num_k,
                            input$num_dst),
                    stringsAsFactors = FALSE)
lo_flex_op_df <- data.frame(position = c("QB","RB","WR","TE"),
                            lim_flex_op = c(input$num_qb + 1,
                                    input$num_rb + 1,
                                    input$num_wr + 1,
                                    input$num_te + 1),
                            stringsAsFactors = FALSE)

limits_df <- inner_join(lo_df,lo_flex_op_df, by = "position")
limits_df

colnames(df)
df_w_weeks <- df %>%
  mutate(dummy = 1) %>%
  full_join(.,week_df, by = "dummy") %>%
  mutate(ppg = ifelse(bye == week, 0, ppg),
         adp = NA) %>%
  select(1,2,3,4,5,6,8)


drafted_w_weeks <- df_w_weeks %>%
  filter(player_team %in% input$your_team)

lineup_optimzier

colnames(drafted_players_w_weeks)
colnames(df_w_weeks)

df_w_weeks2 <- df_w_weeks[,c(2,3,7)]

drafted_w_weeks2 <- drafted_players_w_weeks %>% select(position,ppg,week)

added_value <- function(data,player_num,compare_to){
  
  hypothetical_df <- bind_rows(data, # current team
                               df_w_weeks2[(player_num - 1)*13 + 1 : player_num*13,]
  ) %>%
    group_by(week,position) %>%
    arrange(desc(ppg)) %>%
    mutate(obs = row_number())
  
  hypothetical_lineup <- hypothetical_df %>%
    inner_join(.,limits_df, by = c("position")) %>%
    group_by(week,position) %>%
    arrange(desc(ppg)) %>%
    mutate(obs = row_number(),
           flex_op_max = ifelse(obs== lim_flex_op & ppg == max(ppg),1,0)) %>%
    filter(obs <= lim || flex_op_max == 1) %>%
    summarise(ttl_points = sum(ppg))
  

  points_added <- (hypothetical_lineup$ttl_points - compare_to)/13
  
  return(round(points_added,1))
  
}

drafted_players_w_weeks <- 
  dfr %>%
    filter(player_team %in% input$your_team) %>%
    mutate(dummy = 1) %>%
    full_join(., week_df, by = "dummy") %>%
    mutate(ppg = ifelse(bye == week, 0, ppg)) %>%
    arrange(position) %>%
    mutate(VALUE_ADDED = 0)

lineup_optimizer <-
  drafted_players_w_weeks %>%
  filter(player_team %in% input$your_team) %>%
  arrange(week,position,desc(ppg)) %>%
  group_by(week,position) %>%
  mutate(obs = row_number()) %>%
  inner_join(.,lo_df, by = c("position")) %>%
  filter(obs <= lim) %>%
  bind_rows(., 
            drafted_players_w_weeks %>%
              filter(player_team %in% input$your_team) %>%
              arrange(week,position,desc(ppg)) %>%
              group_by(week, position) %>%
              mutate(obs = row_number()) %>%
              inner_join(.,lo_flex_op_df, by = c("position")) %>%
              filter(obs == lim) %>%
              ungroup() %>%
              group_by(week) %>%
              filter(ppg == max(ppg))) %>%
  group_by(week) %>%
  summarise(ttl_points = sum(ppg))

  
  dfr_alt <- dfr %>% data.frame()
  print(str(dfr_alt))
  dfr_alt$VALUE_ADDED = 0
  dfr_list <- list(dfr_alt)
  dpww <- drafted_players_w_weeks
  thelineup <- lineup_optimizer %>% data.frame() %>% mutate(ttl_points = round(ttl_points,1)) %>% data.frame() 
  # ctp = sum(thelineup$ttl_pts)
  start = Sys.time()
  ctp = 0

  
  av <- rep(0,dim(dfr_alt)[1])
  start = Sys.time()
  # for(i in 1:length(av)){
  for(i in 1:900){
  # for(i in 1:2){
    av[i] <- added_value(drafted_w_weeks2,i,ctp)
  }
  end = Sys.time()
  print(end - start)
  
  # I think the warnings are due to NA's in the data. But I made great progress tonight. Down from 5 seconds to 3.2,
  # and overall, down from 30 seconds to 3 !!
  