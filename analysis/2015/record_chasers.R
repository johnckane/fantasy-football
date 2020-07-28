library(sqldf)
library(dplyr)
library(reshape2)
library(knitr)
library(googleVis)
library(RCurl)
library(googlesheets)
this_week <- 5 #to be changed every week
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

## points through five weeks 
five_weeks <- games_played %>%
    filter(week <= 5) %>%
    group_by(year,owner) %>%
    summarise(ttl_points = sum(points)) %>%
    ungroup() %>%
    arrange(desc(ttl_points))
five_weeks

cum_five_weeks <- games_played %>%
    ungroup() %>%
    filter(week <= 5) %>%
    arrange(year,owner,week) %>%
    group_by(year,owner) %>%
    mutate(cum_points = cumsum(points)) %>%
    ungroup() 

top5 <- games_played %>%
    filter(week <= 5) %>%
    group_by(year,owner) %>%
    summarise(ttl_points = sum(points)) %>%
    ungroup() %>%
    arrange(desc(ttl_points)) %>%
    slice(1:5)

bottom5 <- games_played %>%
    filter(week <= 5) %>%
    group_by(year,owner) %>%
    summarise(ttl_points = sum(points)) %>%
    ungroup() %>%
    arrange(ttl_points) %>%
    slice(1:5)

top5_cum <- inner_join(top5,cum_five_weeks,by=c("owner","year")) %>% 
    arrange(year,owner,week) %>%
    mutate(label = paste(year," ",owner,sep=""))
bottom5_cum <- inner_join(bottom5,cum_five_weeks,by=c("owner","year")) %>%
    arrange(year,owner,week) %>%
    mutate(label = paste(year," ",owner,sep=""))

library(ggplot2)
library(directlabels)

plot1 <- ggplot(data=top5_cum,
                aes(x=week,y=cum_points,group=label))
plot1 + geom_line(aes(colour=label)) + geom_dl(method="last.qp", mapping = aes(label = label))

plot2 <- ggplot(data=bottom5_cum,
                aes(x=week,y=cum_points,group=label))
plot2 + geom_line(aes(colour=label)) +geom_point(aes(colour=label))
