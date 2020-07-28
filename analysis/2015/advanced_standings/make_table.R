library(sqldf)
library(dplyr)
library(reshape2)
library(knitr)
library(googleVis)
library(RCurl)
library(googlesheets)
this_week <- 6 #to be changed every week
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

data2015 <- filter(games_played, year == 2015,week<=this_week)
data2015_summary <- data2015 %>%
    group_by(owner) %>%
    summarise(W = sum(W),
              L = sum(L),
              Points = sum(points),
              PA = sum(PA),
              PW = sum(pw),
              Luck = W - PW,
              SOS = sum(opp_pw)/n()) %>%
    arrange(desc(W),desc(Points))

### Calculate SOS remaining ###
games_to_play <- filter(data, is.na(points)==TRUE) %>% arrange(game_id)
### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(games_to_play)[1],by=2)){    
    games_to_play$Opponent[i] <- games_to_play$owner[i+1]
    games_to_play$Opponent[i+1] <- games_to_play$owner[i]
}

tbp <- games_to_play %>% select(owner,Opponent,week)
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
d1 <- d1 %>% group_by(owner) %>% summarise(sos_remaining = sum(PW)/this_week/n()) %>% arrange(desc(sos_remaining))

standings <- left_join(data2015_summary,d1,by=c("owner"))

### LR Wins (call it moderated Points?)
### To predict probability of winning, fit LR model to all games but the one in question ###
for(i in seq(from=1, to=dim(games_played)[1], by = 2)){
    lr <- glm(data=games_played[-c(i,i+1),], W~points,family = 'binomial')
    games_played$lr_prob[i] <- predict(lr,type='response',newdata = games_played[i,])
    games_played$lr_prob[i+1] <- predict(lr, type='response', newdata = games_played[i+1,])
}
## Add up for this year ##
lrwins_df <- games_played %>% 
    filter(year == 2015) %>% 
    group_by(owner) %>% 
    summarise(lrwins = sum(lr_prob)) %>%
    ungroup()

standings2 <- left_join(standings,lrwins_df,by=c("owner"))

## Empirical Playoff Rate ##
setwd("~/stats_corner/2015/advanced_standings")
rates <- read.csv("playoff_rates.csv",stringsAsFactors=FALSE,header=TRUE)
standings3 <- left_join(standings2 %>% mutate(wins = W),
                        rates %>% filter(week == this_week) %>% select(wins,playoff_rate),
                        by = c("wins"))
### Projected Wins ###
games_played_2015 <- filter(ungroup(games_played), year == 2015)
f <- function(a,b){
    df <- data.frame(A = games_played_2015$points[games_played_2015$owner==a],
                     B = games_played_2015$points[games_played_2015$owner==b])
    df_melt <- melt(df)
    df_melt %>% arrange(desc(value))
    df_melt$p = 1/dim(df)[1]
    df_melt <- df_melt %>%
        group_by(variable) %>%
        mutate(rk = rank(value)) %>%
        ungroup() %>%
        arrange(desc(value))
    df_melt$weight <- 1
    for(i in 1:(dim(df_melt)[1])){
        df_melt$p_win[i] <- sum(df_melt$weight[which(df_melt$variable != df_melt$variable[i] & df_melt$value[i] > df_melt$value)])/sum(df_melt$weight[which(df_melt$variable == df_melt$variable[i])])
    }
    df_melt <- df_melt %>%
        group_by(variable) %>%
        summarise(prob_win = sum(p_win*p))
    df_melt
    df_cast <- dcast(df_melt,.~variable,value.var="prob_win")
    return(df_cast)
}
games_to_play$win_proj <- rep(0,dim(games_to_play)[1])
for(i in 1:dim(games_to_play)[1]){
    games_to_play$win_proj[i] = f(games_to_play$owner[i],games_to_play$Opponent[i])$A
}

proj_wins_df <- games_to_play %>% 
    group_by(owner) %>% 
    summarise(total = sum(win_proj)) %>% 
    ungroup()

standings4 <- left_join(standings3,proj_wins_df,by=c("owner")) %>%
    mutate(proj_wins_total = W + total) %>%
    select(1,2,3,6,7,8,12,4,5,9,14)

standings4$PW <- round(standings4$PW,2)
standings4$Luck <- round(standings4$Luck,2)
standings4$SOS <- round(100*standings4$SOS,1)
standings4$sos_remaining <- round(100*standings4$sos_remaining,1)
standings4$proj_wins_total <- round(standings4$proj_wins_total,2)
standings4$playoff_rate <- round(100*standings4$playoff_rate,1)
colnames(standings4) <- c("Owner",
                          "W",
                          "L",
                          "Proportional Wins",
                          "Luck",
                          "Strength of Schedule",
                          "Playoff Chances (%)",
                          "Points For",
                          "Points Against",
                          "Strength of Schedule Remaining",
                          "Projected Win Total")
write.csv(standings4,"standings_week6.csv",row.names=FALSE)
