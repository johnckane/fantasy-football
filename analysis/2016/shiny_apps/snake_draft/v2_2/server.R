library(shiny)
library(dplyr)
library(tidyr)
library(stringr)

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


flex_op_pos <- function(nqb,nrb,nwr,nte,data){
  bind_rows(lo_flex_op("QB",nqb,data),
            lo_flex_op("RB",nrb,data),
            lo_flex_op("WR",nwr,data),
            lo_flex_op("TE",nte,data)) %>%
    group_by(week) %>%
    filter(ppg == max(ppg))
}

#' This function calculates season per game value added:
added_value <- function(data,new_row,compare_to,nqb,nrb,nwr,nte){
hypothetical_df <- bind_rows(data, #%>%
#                              rename(ppg = standard_points) %>%
#                               full_join(.,week_df, by = "dummy"),
                             new_row %>%
#                              rename(ppg = standard_points) %>%
                               mutate(dummy = 1) %>%
                               full_join(.,week_df, by = "dummy")) %>% 
  data.frame()
hypothetical_lineup <- bind_rows(lo("QB", nqb, hypothetical_df),
                                 lo("RB", nrb, hypothetical_df),
                                 lo("WR", nwr, hypothetical_df),
                                 lo("TE", nte, hypothetical_df),
                                 flex_op_pos(nqb + 1,
                                             nrb + 1,
                                             nwr + 1,
                                             nte + 1,
                                             hypothetical_df)) %>%
  group_by(week) %>%
  summarise(ttl_points = sum(ppg)) 

points_added <- (sum(hypothetical_lineup$ttl_points) - sum(compare_to$ttl_points))/13

return(points_added)
}


shinyServer(function(input, output) {

  dfr <-     reactive({
  if(input$scoring_format == "PPR"){
      df %>% 
      mutate(adp = ifelse(ppr_adp == "null", 999, as.numeric(ppr_adp))) %>%
      select(1,2,3,8,7) %>% 
      data.frame() %>% 
      `colnames<-`(c("player_team","position","ppg","adp","bye")) 
    }
  else if(input$scoring_format == "Standard"){
      df %>% 
      mutate(adp = ifelse(standard_adp == "null",999,as.numeric(standard_adp))) %>%
      select(1,2,5,8,7) %>% 
      data.frame() %>% 
      `colnames<-`(c("player_team","position","ppg","adp","bye"))
    }
  })
  
  
  dfr2 <- reactive ({
  if(input$extra_pos == "FLEX"){
    bind_rows(dfr(),
               dfr() %>% mutate(position = ifelse(position %in% c("RB","WR","TE"),
                                                  "FLEX",
                                                  position)) %>%
                         filter(position == "FLEX")
               ) %>%
                 data.frame() %>%
                 `colnames<-`(c("player_team","position","ppg","adp","bye"))
   }  
   else if(input$extra_pos == "OP"){
     bind_rows(dfr(),
               dfr() %>% mutate(position = ifelse(position %in% c("QB","RB","WR","TE"),
                                                  "OP",
                                                  position)) %>%
                         filter(position == "OP")
               ) %>%
               data.frame() %>%
               `colnames<-`(c("player_team","position","ppg","adp","bye"))
   }
  })
  


  next_pick <- reactive({
    ifelse(input$picks_made %% 2 == 0,
           input$picks_made * input$league_teams + input$first_pick,
           input$picks_made * input$league_teams + input$league_teams - input$first_pick + 1)
  })
  
  # The pick after the next
  next_pick1 <- reactive({
    ifelse(input$picks_made %%2 == 0,
           next_pick() + 2 * (input$league_teams - input$first_pick) + 1,
           (input$picks_made + 1) * input$league_teams + input$first_pick
    )
  })
  
  # Two picks after the next
  next_pick2 <- reactive({
                       next_pick() + 2 * input$league_teams
  })
    
  # here dp stands for drafted players. 
  # 0 : current drafted players
  # 1 : anticipated drafted players by the next time you pick
  # 2 : anticipated drafted players by the time after the next time you pick  
  dp0 <- reactive({
    input$drafted_players
  })
  ap1 <- reactive({
    dfr2() %>%
          filter(!player_team %in% input$drafted_players, 
                 position %in% input$pos_to_rec, 
                 !bye %in% as.numeric(input$byes_to_filter)) %>%
          arrange(adp)
  })
  ap2 <- reactive({
    dfr2() %>%
          filter(!player_team %in% input$drafted_players, 
                 position %in% input$pos_to_rec, 
                 !bye %in% as.numeric(input$byes_to_filter)) %>%
          arrange(adp) %>%
          slice((next_pick1() - length(dp0())): dim(dfr2())[1])
  })
  ap3 <- reactive({
    dfr2() %>%
          filter(!player_team %in% input$drafted_players, 
                 position %in% input$pos_to_rec, 
                 !bye %in% as.numeric(input$byes_to_filter)) %>%
          arrange(adp) %>%
          slice((next_pick2() - length(dp0())): dim(dfr2())[1])
  })
  n3 <- reactive({
    rbind(
    ap1() %>%
      group_by(position) %>%
      arrange(desc(ppg)) %>%
      slice(1),
    ap2() %>%
      group_by(position) %>%
      arrange(desc(ppg)) %>%
      slice(1),
    ap3() %>%
      group_by(position) %>%
      arrange(desc(ppg)) %>%
      slice(1)
  ) %>%
    arrange(position,adp) %>%
    mutate(pct_drop = round(100*(ppg - lag(ppg))/ppg,2),
           raw_drop = (ppg-lag(ppg))) %>%
    #select (-points) %>%
    group_by(position) %>%
    mutate(record = row_number())
  })
  
  # get the metrics
  recs <- reactive({
    if(input$one_or_two == 1){
    n3() %>% 
    select(position,ppg,record) %>%
    spread(key = record, value = ppg)  %>%
    inner_join(
      n3() %>%
        select(position,pct_drop, record) %>%
        spread(key = record, value = pct_drop), #%>%
      by = "position") %>%
    inner_join(
      n3() %>%
        select(position,raw_drop,record) %>%
        spread(key = record, value = raw_drop), # %>%
      by = "position") %>%
    inner_join(
      n3() %>%
        select(position,player_team,record) %>%
        spread(key = record, value = player_team),  #%>%
      by = "position") %>%
      select(1,11,2,12,3,6,9) %>%
      data.frame() %>%
      `colnames<-`(c("POS","BEST_AVAILABLE","PPG","BANT","PPG_BANT","PCT_DROP","RAW_DROP"))
    }
    else if(input$one_or_two == 2){
      n3() %>% 
        select(position,ppg,record) %>%
        spread(key = record, value = ppg)  %>%
        inner_join(
          n3() %>%
            select(position,pct_drop, record) %>%
            spread(key = record, value = pct_drop), #%>%
          by = "position") %>%
        inner_join(
          n3() %>%
            select(position,raw_drop,record) %>%
            spread(key = record, value = raw_drop), # %>%
          by = "position") %>%
        inner_join(
          n3() %>%
            select(position,player_team,record) %>%
            spread(key = record, value = player_team),  #%>%
          by = "position") %>%
        select(1,11,2,13,4,7,10) %>%
        data.frame() %>%
        `colnames<-`(c("POS","BEST_AVAILABLE","PPG","BANT2","PPG_BANT2","PCT_DROP","RAW_DROP")) %>%
        mutate(PCT_DROP = round(100*(PPG_BANT2 - PPG) / PPG,1),
               RAW_DROP = (PPG_BANT2 - PPG))
    }
  })
  
  

  output$next_pick <- renderText(next_pick())
  output$next_pick1 <- renderText(next_pick1())
  output$next_pick2 <- renderText(next_pick2())
  
  output$pos_recs <- renderText(paste(as.character(recs()$POS[order(recs()$PCT_DROP)]), collapse = ", "))
  
  output$available_players <- renderDataTable({
      dfr2() %>%
        filter(!player_team %in% input$drafted_players,
               !position %in% c("FLEX","OP")) %>%
        arrange(adp) %>%
        select(1,2,4,3,5) %>%
        data.frame() %>%
        `colnames<-`(c("Player","POS","ADP","PPG","BYE"))
  })
  
  drafted_players_w_weeks <- reactive({
    dfr2() %>%
      filter(player_team %in% input$your_team) %>%
      mutate(dummy = 1) %>%
      full_join(., week_df, by = "dummy") %>%
      mutate(ppg = ifelse(bye == week, 0, ppg))
  })
  
  lineup_optimizer <- reactive({
    if(input$extra_pos == "FLEX"){
      bind_rows(lo("QB",input$num_qb,drafted_players_w_weeks()),
                lo("RB",input$num_rb,drafted_players_w_weeks()),
                lo("WR",input$num_wr,drafted_players_w_weeks()),
                lo("TE",input$num_te,drafted_players_w_weeks()),
                flex_op_pos(0,
                            input$num_rb + 1,
                            input$num_wr + 1,
                            input$num_te + 1,
                            drafted_players_w_weeks())) %>%
        group_by(week) %>%
        summarise(ttl_points = sum(ppg)) %>%
        arrange(week)
    }
    else if(input$extra_pos == "OP"){
      bind_rows(lo("QB",input$num_qb,drafted_players_w_weeks()),
                lo("RB",input$num_rb,drafted_players_w_weeks()),
                lo("WR",input$num_wr,drafted_players_w_weeks()),
                lo("TE",input$num_te,drafted_players_w_weeks()),
                flex_op_pos(input$num_qb + 1,
                            input$num_rb + 1,
                            input$num_wr + 1,
                            input$num_te + 1,
                            drafted_players_w_weeks())) %>%
        group_by(week) %>%
        summarise(ttl_points = sum(ppg)) %>%
        arrange(week)
    }
    
    
    
    
    
  })
  
  output$optimized_lineup <- renderDataTable({
    
    lineup_optimizer() %>%  
      data.frame() %>%
      mutate(ttl_points = round(ttl_points,1)) %>%
      `colnames<-`(c("Week","Expected Points"))
  
  },
  options = list(paging = FALSE, searching = FALSE))
  
  
  # recs2 <- reactive({
  #     recs_alt <- recs()
  #     recs_alt$VALUE_ADDED <- rep(0,dim(recs_alt)[1])
  #     for(i in 1:dim(recs_alt)[1]){
  # 
  #        recs_alt$VALUE_ADDED[i] <- added_value(drafted_players_w_weeks() %>% data.frame(),
  #                                            recs_alt[i,] %>% data.frame(),
  #                                            lineup_optimizer() %>%  
  #                                              data.frame() %>%
  #                                              mutate(ttl_points = round(ttl_points,1)) %>%
  #                                              data.frame(),
  #                                            input$num_qb,
  #                                            input$num_rb,
  #                                            input$num_wr,
  #                                            input$num_te
  #                                            )
  #     }
  #   
  #      return(recs_alt)
  # })
  # 
  #  output$rec_table <- renderDataTable({
  #    recs2() %>%
  #      arrange(PCT_DROP) #%>%
  #    #data.frame()
  #  },
  #  options = list(paging = FALSE, searching = FALSE))
  
   output$rec_table <- renderDataTable({
     recs() %>%
       arrange(PCT_DROP) #%>%
     #data.frame()
   },
   options = list(paging = FALSE, searching = FALSE))
   
   
})