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

games_played <- games_played %>% arrange(gameid)

### To predict probability of winning, fit LR model to all games but the one in question ###
for(i in seq(from=1, to=dim(games_played)[1], by = 2)){
    lr <- glm(data=games_played[-c(i,i+1),], W~points,family = 'binomial')
    games_played$lr_prob[i] <- predict(lr,type='response',newdata = games_played[i,])
    games_played$lr_prob[i+1] <- predict(lr, type='response', newdata = games_played[i+1,])
}
with(games_played,plot(points,lr_prob))

## Add up for this year ##
games_played %>% filter(year == 2015) %>% group_by(owner) %>% 
    summarise(lrwins = sum(lr_prob),
              totalpw = sum(pw)) %>%
    arrange(desc(lrwins))
games_played %>% ungroup() %>% filter(year == 2015,owner=="Thieneman") %>% 
    select(owner,points,lr_prob,pw)

with(games_played,plot(pw,lr_prob))
colnames(games_played)
