library(shiny)
library(dplyr)
library(tidyr)

df <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_0/faa_projection_data.csv",
               stringsAsFactors = FALSE,
               header = TRUE)



shinyServer(function(input, output) {

dfr <-     reactive({
  if(input$scoring_format == "PPR"){
      df %>% 
      mutate(adp = ifelse(ppr_adp == "null", 999, as.numeric(ppr_adp))) %>%
      select(1,2,3,7) %>% 
      data.frame() %>% 
      `colnames<-`(c("player_team","position","ppg","adp")) 
    }
  else if(input$scoring_format == "Standard"){
      df %>% 
      mutate(adp = ifelse(standard_adp == "null",999,as.numeric(standard_adp))) %>%
      select(1,2,5,7) %>% 
      data.frame() %>% 
      `colnames<-`(c("player_team","position","ppg","adp"))
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
    dfr() %>%
          filter(!player_team %in% input$drafted_players) %>%
          arrange(adp)
  })
  ap2 <- reactive({
    dfr() %>%
          filter(!player_team %in% input$drafted_players) %>%
          arrange(adp) %>%
          slice((next_pick1() - length(dp0())): dim(dfr())[1])
  })
  ap3 <- reactive({
    dfr() %>%
          filter(!player_team %in% input$drafted_players) %>%
          arrange(adp) %>%
          slice((next_pick2() - length(dp0())): dim(dfr())[1])
  })
  n3 <- reactive({
    rbind(
    ap1() %>%
      group_by(position) %>%
      arrange(adp) %>%
      slice(1),
    ap2() %>%
      group_by(position) %>%
      arrange(adp) %>%
      slice(1),
    ap3() %>%
      group_by(position) %>%
      arrange(adp) %>%
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
      `colnames<-`(c("POS","BEST_AVAILABLE","PPG","BEST_AVAILABLE_NEXT_PICK","PPG_BA_NEXT_PICK","PCT_DROP","RAW_DROP"))
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
      dfr() %>%
        filter(!player_team %in% input$drafted_players) %>%
        arrange(adp) %>%
        select(1,2,4,3) %>%
        data.frame() %>%
        `colnames<-`(c("Player","POS","ADP","PPG"))
  })
  
})