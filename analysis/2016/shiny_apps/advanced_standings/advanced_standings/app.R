library(httr, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
library(curl, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
library(xml2, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
library(readr, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
library(googlesheets, lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.3")
library(dplyr)
library(reshape2)
library(shiny)

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


data2016 <- filter(games_played, year == 2016)
data2016_summary <- data2016 %>%
  group_by(owner) %>%
  summarise(W = sum(W),
            L = sum(L),
            Points = sum(points),
            PA = sum(PA),
            PW = round(sum(pw),1),
            SOS = round(sum(opp_pw)/n(),2),
            Luck = W - PW) %>%
  arrange(desc(PW))



# Want to incorporate empirical playoff rates
g <- function(x){
  
  data_all <- games_played %>% 
    filter(year != 2009,year!=2016,week<=x) %>%
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
this_week <- max(games_played %>% ungroup() %>% filter(year == 2016) %>% select(week))
data2016_summary2 <- left_join(data2016_summary,
                               cbind(c(0:12),t(rate_matrix[this_week,2:14])) %>% 
                                 data.frame() %>% 
                                 `colnames<-`(c("W","playoff_prob")),
                               by = "W") %>%
  mutate(playoff_prob = paste0(round(100*playoff_prob,1),"%")) %>%
  `colnames<-`(c("Owner","W","L","Points","PA","PW","SOS","Luck","Playoff Probability"))

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
     titlePanel("Bad Newz Advanced Stadings"),
     p(paste0("Last Updated: ",last_updated)),
     a(href= "mailto:Stats.Corner@gmail.com","Stats.Corner@gmail.com"),
     dataTableOutput("standings"),
     h4("Glossary"),
     h5("PW (Proportional Wins): The number of wins truly earned, without consideration of scheduled opponents."),
     h5("SOS (Strength of Schedule): Averaged PW of opponents played. With 1 indicating hardest schedule, 0 the easiest."),
     h5("Luck: The difference between actual wins and Proportional Wins."),
     h5("Playoff Probability: Based on league history, teams with this many wins at this point in the season make the playoffs X% 
         of the time")
    )
   )

# The output is a table...
server <- shinyServer(function(input, output) {
   
  output$standings <- renderDataTable({data2016_summary2},
                                      options = list(paging = FALSE, searching = FALSE))
})

# Run the application 
shinyApp(ui = ui, server = server)

