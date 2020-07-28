library(shiny)
library(dplyr)
library(tidyr)
library(stringr)

df <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_1/faa_projection_data.csv",
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

#df$position2 <- ifelse(df$position %in% c("RB","WR","TE"),
#                       "FLEX",
#                       df$position)
#df$position3 <- ifelse(df$position %in% c("QB","RB","WR","TE"),
#                       "OP",
#                       df$position)


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
  #output$next_pick2 <- renderText(next_pick2())
  
  output$pos_recs <- renderText(paste(as.character(recs()$POS[order(recs()$PCT_DROP)]), collapse = ", "))
  
  output$rec_table <- renderDataTable({
    recs() %>%
      arrange(PCT_DROP) #%>%
      #data.frame()
  },
  options = list(paging = FALSE, searching = FALSE))
  
  output$available_players <- renderDataTable({
      dfr2() %>%
        filter(!player_team %in% input$drafted_players,
               !position %in% c("FLEX","OP")) %>%
        arrange(adp) %>%
        select(1,2,4,3,5) %>%
        data.frame() %>%
        `colnames<-`(c("Player","POS","ADP","PPG","BYE"))
  })
  
})