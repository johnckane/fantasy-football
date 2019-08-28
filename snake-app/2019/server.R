library(shiny)
library(dplyr)
library(tidyr)
library(stringr)


shinyServer(function(input, output) {
  
#############################################
#### These are all non-reactive elements ####
#############################################

  df <- read.csv("/home/john/projects/fantasy-football/data/FFA/2019/snake_app_data.csv",
  #df <- read.csv("/srv/shiny-server/stats-corner/2017/snake-app/ffa_data.csv",
                 stringsAsFactors = FALSE,
                 header = TRUE)

  # Needed for the lineup optimzier:
  week <- c(1:13)
  week_df <- data.frame(week, dummy = 1)
  
  #' This function calculates season per game value added:
  
  added_value <- function(data,
                          player_num,
                          week_data,
                          compare_to,
                          player_list){
    hypothetical_df <- bind_rows(data, # current team
                                 # df_w_weeks2()[((player_num - 1)*13 + 1):(player_num*13),]
                                 week_data[((player_num - 1)*13 + 1):(player_num*13),]
    ) %>%
      group_by(week,position) %>%
      arrange(desc(ppg)) %>%
      mutate(obs = row_number()) %>%
      ungroup() %>%
      inner_join(.,limits_df(), by = c("position"))

    hypothetical_lineup <-
      bind_rows(hypothetical_df %>% filter(obs <= lim),
                hypothetical_df %>% filter(obs == lim_flex_op) %>% group_by(week) %>% arrange(desc(ppg)) %>% slice(1)
      ) %>%
      ungroup() %>%
      summarise(ttl_points = sum(ppg))
    
    points_added <- (hypothetical_lineup$ttl_points - compare_to)/13
    
    return(round(points_added,1))
    
  }

  
###############################################
###############################################  
############################################### 
  picks_made <- reactive({length(input$your_team)})
  
  lo_df <- reactive({
    data.frame(position = c("QB","RB","WR","TE","K","DST"),
                      lim = c(input$num_qb,
                              input$num_rb,
                              input$num_wr,
                              input$num_te,
                              input$num_k,
                              input$num_dst),
                      stringsAsFactors = FALSE) 
  })
  
  
  dfr <- reactive({
  if(input$league == "Catalina"){
      df %>% 
      select(1,2,3,4,7,8) %>% 
      data.frame() %>% 
      `colnames<-`(c("player_team","position","age","bye","adp","ppg")) 
    }
  else if(input$league == "Chicago"){
      df %>% 
      select(1,2,3,4,9,10) %>% 
      data.frame() %>% 
      `colnames<-`(c("player_team","position","age","bye","adp","ppg"))
  }
   else if(input$league == "Kane"){
     df %>%
       select(1,2,3,4,5,6) %>%
       data.frame() %>%
       `colnames<-`(c("player_team","position","age","bye","adp","ppg"))
   } 
  })
  

  limits_df <- reactive ({
    if(input$extra_pos == "FLEX"){
      left_join(lo_df(),
                 data.frame(position = c("RB","WR","TE"),
                            lim_flex_op = c(input$num_rb + 1,
                                            input$num_wr + 1,
                                            input$num_te + 1),
                            stringsAsFactors = FALSE),
                 by = "position")
    }
    else if(input$extra_pos == "OP"){
      left_join(lo_df(),
                 data.frame(position = c("QB","RB","WR","TE"),
                            lim_flex_op = c(input$num_qb + 1,
                                            input$num_rb + 1,
                                            input$num_wr + 1,
                                            input$num_te + 1),
                            stringsAsFactors = FALSE),
                 by = "position")
    }
    else if(input$extra_pos == "None"){
      left_join(lo_df(),
                data.frame(position = c("QB","RB","WR","TE"),
                           lim_flex_op = c(0,
                                           0,
                                           0,
                                           0),
                           stringsAsFactors = FALSE),
                by = "position")
    }
  })
  
  df_w_weeks <- reactive({
    dfr() %>%
    mutate(dummy = 1) %>%
    full_join(.,week_df, by = "dummy") %>%
    mutate(ppg = replace(ppg,bye==week,0)) %>%
    arrange(player_team,week)
  })
  
  df_w_weeks2 <- reactive({
   df_w_weeks() %>%
      filter(!(player_team %in% input$your_team))
  })
  

  my_team_w_weeks <- reactive({
    df_w_weeks() %>%
      filter(player_team %in% input$your_team)
  })
  
  
  lineup_optimizer <- reactive({
    my_team_w_weeks()%>%
      arrange(week,position,desc(ppg)) %>%
      group_by(week,position) %>%
      mutate(obs = row_number()) %>%
      inner_join(.,limits_df(), by = c("position")) %>%
      filter(obs <= lim) %>%
      bind_rows(., 
                my_team_w_weeks() %>%
                  arrange(week,position,desc(ppg)) %>%
                  group_by(week, position) %>%
                  mutate(obs = row_number()) %>%
                  inner_join(.,limits_df(), by = c("position")) %>%
                  filter(obs == lim_flex_op) %>%
                  ungroup() %>%
                  group_by(week) %>%
                  filter(ppg == max(ppg))) %>%
      group_by(week) %>%
      summarise(ttl_points = sum(ppg))
  })
  
  
  
  dfr2 <- reactive({
    dfr_2 <- dfr() %>% filter(.,
                             !(player_team %in% input$your_team)) %>%
      arrange(player_team)

    players <- dfr_2$player_team
    
    av <- rep(0,dim(dfr_2)[1])
    ct <- lineup_optimizer() %>% ungroup() %>% summarise(total = sum(ttl_points)) %>% select(total)
    for(i in 1:length(av)){
      av[i] <- added_value(data = my_team_w_weeks(),
                           player_num = i,
                           week_data = df_w_weeks2(),
                           compare_to = ct,
                           player_list  = players)
    }
    
    
    return(dfr_2 %>% inner_join(.,
                               data.frame(player_team = players,
                                          VALUE_ADDED = unlist(av),
                                          stringsAsFactors = FALSE), 
                               by  = "player_team") %>% 
             mutate(VALUE_ADDED = ifelse(player_team %in% input$dnr,-999,VALUE_ADDED)) %>%
             ungroup()
           )
    
  })
  
  next_pick <- reactive({
    ifelse(picks_made() %% 2 == 0,
           picks_made()* input$league_teams + input$first_pick,
           picks_made() * input$league_teams + input$league_teams - input$first_pick + 1)
  })
  
  # The pick after the next
  next_pick1 <- reactive({
    ifelse(picks_made() %%2 == 0,
           next_pick() + 2 * (input$league_teams - input$first_pick) + 1,
           (picks_made() + 1) * input$league_teams + input$first_pick
    )
  })
  
  # Two picks after the next
  next_pick2 <- reactive({
                       next_pick() + 2 * input$league_teams
  })
    
  # here dp stands for drafted players. 
  # ap stands for available players
  # 0 : current drafted players
  # 1 : anticipated drafted players by the next time you pick
  # 2 : anticipated drafted players by the time after the next time you pick  
  dp0 <- reactive({
    bind_rows(data.frame(drafted_players = input$drafted_players,
                         stringsAsFactors = FALSE),
              data.frame(drafted_players = input$your_team,
                         stringsAsFactors = FALSE)) %>%
      distinct(drafted_players, .keep_all = TRUE)
  })
  
  ap1 <- reactive({
    dfr2() %>%
          filter(!player_team %in% input$drafted_players, 
                 !player_team %in% input$your_team#,
                 ) %>%
          arrange(adp) 
  })
  
  ap2 <- reactive({
    dfr2() %>%
          filter(!player_team %in% input$drafted_players,
                 !player_team %in% input$your_team#,
                 ) %>%
          arrange(adp) %>%
          slice((next_pick1() - length(dp0())): dim(dfr2())[1])
  })
  ap3 <- reactive({
    dfr2() %>%
          filter(!player_team %in% input$drafted_players,
                 !player_team %in% input$your_team#,
                 ) %>%
          arrange(adp) %>%
          slice((next_pick2() - length(dp0())): dim(dfr2())[1])
  })
  n3 <- reactive({
    
    rbind(
    ap1() %>%
      group_by(position) %>%
      arrange(desc(VALUE_ADDED)) %>%
      slice(1),
    ap2() %>%
      group_by(position) %>%
      arrange(desc(VALUE_ADDED)) %>%
      slice(1),
    ap3() %>%
      group_by(position) %>%
      arrange(desc(VALUE_ADDED)) %>%
      slice(1)
  ) %>% 
     group_by(position) %>%
     arrange(position,desc(VALUE_ADDED)) %>%
     mutate(pct_drop = round(100*((VALUE_ADDED - dplyr::lead(VALUE_ADDED))/VALUE_ADDED),1),
             raw_drop = (VALUE_ADDED - dplyr::lead(VALUE_ADDED)),
             record   = row_number())
  })

  output$N3 <- renderDataTable({
    n3() %>%
      data.frame(.,stringsAsFactors = FALSE)
  },
  options = list(paging = FALSE, searching = FALSE))
  

  
  # get the metrics
  recs <- reactive({
    if(input$one_or_two == 1){
    n3() %>% 
    select(position,ppg,record) %>%
    spread(key = record, value = ppg)  %>%
    inner_join(
      n3() %>%
        select(position,VALUE_ADDED,record) %>%
        spread(key = record, value = VALUE_ADDED), 
      by = "position") %>%    
    inner_join(
      n3() %>%
        select(position,pct_drop, record) %>%
        spread(key = record, value = pct_drop), 
      by = "position") %>%
    inner_join(
      n3() %>%
        select(position,raw_drop,record) %>%
        spread(key = record, value = raw_drop), 
      by = "position") %>%
    inner_join(
      n3() %>%
        select(position,player_team,record) %>%
        spread(key = record, value = player_team),   
      by = "position") %>%
    inner_join(
      n3() %>%
        select(position,age,record) %>%
        spread(key = record, value = age),
      by = "position") %>%
      select(1,14,17,5,15,6,11) %>%
      data.frame(.,stringsAsFactors = FALSE) %>%
      `colnames<-`(c("POS","BEST_AVAILABLE","Age","VA","BANT","VA_BANT","DROP")) %>%
    arrange(desc(DROP))
    }
    else if(input$one_or_two == 2){
      n3() %>% 
        select(position,ppg,record) %>%
        spread(key = record, value = ppg)  %>%
        inner_join(
          n3() %>%
            select(position,VALUE_ADDED,record) %>%
            spread(key = record, value = VALUE_ADDED), 
          by = "position") %>%
        inner_join(
          n3() %>%
            select(position,pct_drop, record) %>%
            spread(key = record, value = pct_drop),
          by = "position") %>%
        inner_join(
          n3() %>%
            select(position,raw_drop,record) %>%
            spread(key = record, value = raw_drop),
          by = "position") %>%
        inner_join(
          n3() %>%
            select(position,player_team,record) %>%
            spread(key = record, value = player_team),  
          by = "position") %>%
        inner_join(
          n3() %>%
            select(position,age,record) %>%
            spread(key = record, value = age),
          by = "position") %>%
        select(1,14,17,5,16,7,13) %>%
        data.frame(.,stringsAsFactors = FALSE) %>%
        `colnames<-`(c("POS","BEST_AVAILABLE","Age","VA","BANT2","VA_BANT2","DROP")) %>%
        mutate(DROP = VA - VA_BANT2) %>%
        arrange(desc(DROP))
    }
  })
  

  output$next_pick  <- renderText(next_pick())
  output$next_pick1 <- renderText(next_pick1())
  output$next_pick2 <- renderText(next_pick2())
  
  output$pos_recs <- renderText(paste(as.character(recs()$POS[order(-recs()$DROP)]), collapse = ", "))
  
  output$available_players <- renderDataTable({
      dfr2() %>%
        filter(!player_team %in% input$drafted_players,
               !player_team %in% input$your_team) %>%
        arrange(adp) %>%
        data.frame(.,stringsAsFactors = FALSE) %>%
        `colnames<-`(c("Player","Position","Age","BYE","ADP","PPG","VA"))
  },
  options = list())
  output$optimized_lineup <- renderDataTable({
    
    lineup_optimizer() %>%  
      data.frame(.,stringsAsFactors = FALSE) %>%
      mutate(ttl_points = round(ttl_points,1)) %>%
     `colnames<-`(c("Week","Expected Points"))
  },
  options = list(paging = FALSE, searching = FALSE))
 
  output$rec_table <- renderDataTable({
     recs() %>%
     data.frame(.,stringsAsFactors = FALSE)
   },
   options = list(paging = FALSE, searching = FALSE))
})
