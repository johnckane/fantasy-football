# library(httr, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
# library(curl, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
# library(xml2, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
# library(readr, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
# library(googlesheets, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
#library(Rcpp,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
#library(httr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
#library(curl,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
#library(xml2,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
#library(readr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")


library(googlesheets,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(xml2,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(httr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(curl,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(shiny,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(dplyr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(reshape2,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(tidyr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(readr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")





workbook <- gs_url("https://docs.google.com/spreadsheets/d/1c24qtCDF6MnL1I-nNG2ovymFB3fYj1NsWpLe3SGCbJs/pubhtml")

last_updated <- workbook$update %>% as.Date(.,format = "%m/%d/&y") %>%
  as.character() %>%
  strptime(., "%Y-%m-%d", tz = "GMT") %>%
  format(., "%B %d, %Y")

owner <- workbook %>% gs_read(ws = "Owner-Team Name")
games <- workbook %>% gs_read(ws = "Regular Season Games")

owner_games <- left_join(games,owner,by=c("year","team"))

## Arrange data and create win, loss and tie variables
data <- arrange(owner_games,game_id,points)
data$W <- rep(0,dim(data)[1])
data$L <- rep(0,dim(data)[1])
data$T <- rep(0,dim(data)[1])
data$PA <- rep(0,dim(data)[1])
data$result <- rep(" ",dim(data)[1])
data$last_of_streak <- rep(0,dim(data)[1])
data$Opponent <- rep("",dim(data)[1])

games_played <- data %>% filter(points > 0)
### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(games_played)[1],by=2)){
  
  games_played$PA[i]   <- games_played$points[i+1]
  games_played$PA[i+1] <- games_played$points[i]
  
  games_played$Opponent[i] <- games_played$owner[i+1]
  games_played$Opponent[i+1] <- games_played$owner[i]
  
  if(games_played$points[i] < games_played$points[i+1]){
    games_played$L[i] <- 1
    games_played$W[i+1] <- 1
    games_played$result[i] <- "L"
    games_played$result[i+1] <- "W"
    
  }
  if(games_played$points[i] == games_played$points[i+1]){
    games_played$T[i] <- 1
    games_played$T[i+1] <- 1
    games_played$result[i] <- "T"
    games_played$result[i+1] <- "T"
  }
  
}


games_played <- games_played %>%
  arrange(year,week,desc(points)) %>%
  group_by(year,week) %>%
  mutate(rk = rank(points,ties.method="min"),
         opp_rk = rank(PA,ties.method="min")) 

# Now to calculate and add up proportional wins
games_played$pw<- ifelse(games_played$year==2009,(games_played$rk-1)/9,(games_played$rk-1)/11)

games_played$opp_pw <- ifelse(games_played$year==2009,(games_played$opp_rk-1)/9,(games_played$opp_rk-1)/11)


# Want to incorporate empirical playoff rates
g <- function(x){
  
  data_all <- games_played %>% 
    filter(year != 2009,year!=2020,week<=x) %>%
    group_by(year,team,playoffs) %>%
    summarise(wins = sum(W)) %>%
    group_by(wins) %>%
    summarise(yeas = sum(playoffs),
              ttl  = n()) %>%
    select(wins,ttl,yeas)
  
  data_summary <- 
    data_all %>%
    group_by(wins) %>%
    summarise(total_yeas = sum(yeas),
              total      = sum(ttl),
              playoff_rate = total_yeas/total) %>%
    mutate(week = x)
  
  return(data_summary)
}


full_data <- rbind(g(1),g(2),g(3),g(4),g(5),g(6),g(7),g(8),g(9),g(10),g(11),g(12),g(13))

rate_matrix <- dcast(full_data,week~wins,value.var='playoff_rate')

#long_rates <- rate_matrix %>% gather(key="week",value ="prob", na.rm = TRUE) 
long_rates <- full_data %>% select(5,1,4) %>% rename("prob" = "playoff_rate")

long_rates <- long_rates %>% mutate(wins_plus_1 = as.numeric(wins) + 1, wins = as.numeric(wins))

## Create "bye" empirical rates

h <- function(x){
  
  bye_data_all <- games_played %>% 
    filter(year != 2009,year!=2020,week<=x) %>%
    group_by(year,team,bye) %>%
    summarise(wins = sum(W)) %>%
    group_by(wins) %>%
    summarise(yeas = sum(bye),
              ttl  = n()) %>%
    select(wins,ttl,yeas)
  
  bye_data_summary <- 
    bye_data_all %>%
    group_by(wins) %>%
    summarise(total_yeas = sum(yeas),
              total      = sum(ttl),
              bye_rate = total_yeas/total) %>%
    mutate(week = x)
  
  return(bye_data_summary)
}

bye_data <- rbind(h(1),h(2),h(3),h(4),h(5),h(6),h(7),h(8),h(9),h(10),h(11),h(12),h(13))

bye_rates <- bye_data %>% select(5,1,4)

bye_rates <- bye_rates %>% mutate(wins = as.numeric(wins))




# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  titlePanel("Bad Newz Advanced Stadings"),
  p(paste0("Last Updated: ",last_updated)),
  a(href= "mailto:Stats.Corner@gmail.com","Stats.Corner@gmail.com"),
  selectInput("season",label = "Season", choices = c(2009:2020),selected = 2020),
  dataTableOutput("standings"),
  h4("Glossary"),
  h5("PW (Proportional Wins): The number of wins truly earned, without consideration of scheduled opponents."),
  h5("SOS (Strength of Schedule): Averaged PW of opponents played. With 1 indicating hardest schedule, 0 the easiest."),
  h5("Luck: The difference between actual wins and Proportional Wins."),
  h5("Playoff Probability: Based on league history, teams with this many wins at this point in the season make the playoffs X% 
     of the time"),
  h5("Playoff Leverage: The difference in playoff probability given a win vs. a loss this week"),
  h5("BYE Probability: Based on league history, teams with this many wins at this point in the season get a BYE in the playoffs X%
     of the time"),
  h5("BYE Leverage: The difference in BYE probability given a win vs. a loss this week")
  )
)

# The output is a table...
server <- shinyServer(function(input, output) {
  
  this_week <- reactive({max(games_played %>% ungroup() %>% filter(year == input$season) %>% select(week))})
  #this_week <- reactive({1})
  
  data_season <- reactive({filter(games_played, year == input$season)})
  
  data_season_summary <- reactive({data_season() %>%
      group_by(owner) %>%
      summarise(W = sum(W),
                L = sum(L),
                Points = round(sum(points),1),
                PA = round(sum(PA),1),
                PW = round(sum(pw),1),
                SOS = round(sum(opp_pw)/n(),2),
                Luck = W - PW,
                week = this_week()) %>%
      arrange(desc(PW))
  })
  
  
  
  data_season_summary2 <- reactive({
    left_join(data_season_summary(),
              long_rates,
              by = c("week","W" = "wins")) %>%
      mutate(next_week= this_week() + 1, 
             playoff_prob = paste0(round(100*prob,1),"%"))
  })
  
  
  
  
  data_season_summary3 <- reactive({
    if(this_week() != 13){
      data_season_summary2() %>%
        mutate(w_plus_1 = W + 1) %>%
        left_join(.,
                  long_rates,
                  by = c("next_week" = "week","W" = "wins")) %>% 
        rename(p_stay = prob.y) %>%
        left_join(.,
                  long_rates,
                  by = c("next_week" = "week","w_plus_1" = "wins")) %>%
        rename(p_win = prob) %>%
        mutate(leverage = paste0(round(100*(p_win - p_stay),1),"%"),
               this_week = next_week - 1) %>% 
        left_join(.,
                  bye_rates %>% mutate(bye_rate = paste0(round(100*bye_rate,1),"%")),
                  by = c("this_week" = "week","W" = "wins")) %>%
        left_join(.,
                  bye_rates %>% rename(bye_p_stay = bye_rate),
                  by = c("next_week" = "week", "W" = "wins")) %>%
        left_join(.,
                  bye_rates %>% rename(bye_p_win = bye_rate),
                  by = c("next_week" = "week", "w_plus_1" = "wins")) %>%
        mutate(bye_leverage = paste0(round(100*(bye_p_win - bye_p_stay),1),"%")) %>% 
        select(1:8,13,19,21,24) %>% 
        `colnames<-`(c("Owner","W","L","Points","PA","PW","SOS","Luck","Playoff Probability","Playoff Leverage","BYE Probability","BYE Leverage"))
    }
    else if(this_week() == 13){ #Here we don't include leveage statistics
      data_season_summary2() %>%
        mutate(w_plus_1 = W + 1) %>%
        left_join(.,
                  long_rates,
                  by = c("next_week" = "week","W" = "wins")) %>% 
        rename(p_stay = prob.y) %>%
        left_join(.,
                  long_rates,
                  by = c("next_week" = "week","w_plus_1" = "wins")) %>%
        rename(p_win = prob) %>%
        mutate(leverage = paste0(round(100*(p_win - p_stay),1),"%"),
               this_week = next_week - 1) %>% 
        left_join(.,
                  bye_rates %>% mutate(bye_rate = paste0(round(100*bye_rate,1),"%")),
                  by = c("this_week" = "week","W" = "wins")) %>%
        left_join(.,
                  bye_rates %>% rename(bye_p_stay = bye_rate),
                  by = c("next_week" = "week", "W" = "wins")) %>%
        left_join(.,
                  bye_rates %>% rename(bye_p_win = bye_rate),
                  by = c("next_week" = "week", "w_plus_1" = "wins")) %>%
        mutate(bye_leverage = paste0(round(100*(bye_p_win - bye_p_stay),1),"%")) %>% 
        select(1:8,13) %>% 
        `colnames<-`(c("Owner","W","L","Points","PA","PW","SOS","Luck","Playoff Probability"))
    }
  })
  
  output$standings <- renderDataTable({data_season_summary3()},
                                      options = list(paging = FALSE, searching = FALSE))
  
})

# Run the application 
shinyApp(ui = ui, server = server)