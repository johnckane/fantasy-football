library(googlesheets)
library(dplyr)
library(stringr)
sheet <- gs_title("Bad Newz Archives")


owner <- sheet %>% gs_read(ws = "Owner-Team Name")
games <- sheet %>% gs_read(ws = "Regular Season Games")

owner_game <- left_join(games,owner,by=c("year","team"))

## Arrange data and create win, loss and tie variables
data <- arrange(owner_game,game_id,points)
data$W <- rep(0,dim(data)[1])
data$L <- rep(0,dim(data)[1])
data$T <- rep(0,dim(data)[1])
data$PA <- rep(0,dim(data)[1])
data$result <- rep(" ",dim(data)[1])
data$last_of_streak <- rep(0,dim(data)[1])

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

data2015 <- filter(games_played, year == 2015,week<=3)
data2015_summary <- data2015 %>%
    group_by(owner) %>%
    summarise(W = sum(W),
              L = sum(L),
              Points = sum(points),
              PA = sum(PA),
              PW = sum(pw),
              SOS = sum(opp_pw)/n(),
              luck = W - PW) %>%
    arrange(desc(PW))
View(data2015_summary)

### Calculate SOS remaining ###
games_to_play <- filter(data, is.na(points)==TRUE) %>% arrange(game_id)
### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(games_to_play)[1],by=2)){    
    games_to_play$Opponent[i] <- games_to_play$owner[i+1]
    games_to_play$Opponent[i+1] <- games_to_play$owner[i]
}

tbp <- games_to_play %>% select(owner,Opponent,week)
colnames(tbp)
library(sqldf)
d1 <- sqldf('select
                a.owner
            ,   a.Opponent
            ,   a.week
            ,   b.PW
            from
                tbp as a
            ,   data2015_summary as b
            where
                a.Opponent = b.owner')
d1 %>% group_by(owner) %>% summarise(sos_remaining = sum(PW)/3/n()) %>% arrange(desc(sos_remaining))
