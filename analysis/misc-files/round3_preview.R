library(RCurl)
library(dplyr)
library(sqldf)
library(reshape2)
library(ggplot2)
library(directlabels)
library(gridExtra)
library(XML)
library(httr)
library(lubridate)


################################################################################
########         First, Round 1's projected vs. actual over time        ########
################################################################################

url <- "https://docs.google.com/spreadsheets/d/1bwTLuC4bMte00o46yOlmUun0WuiHhe8mwdZuZT3ApVE/pubhtml?gid=179437598&single=true"

readSpreadsheet <- function(url, sheet = 1){
  library(httr)
  r <- GET(url)
  html <- content(r)
  sheets <- readHTMLTable(html, header=FALSE, stringsAsFactors=FALSE)
  df <- sheets[[sheet]]
  dfClean <- function(df){
    nms <- t(df[1,])
    names(df) <- nms
    df <- df[-1,-1] 
    row.names(df) <- seq(1,nrow(df))
    df
  }
  dfClean(df)
}
df <- readSpreadsheet(url)

df <- df[-1,]


df$projected_points = as.numeric(df$projected_points)
df$actual_points = as.numeric(df$actual_points)
df$minutes_remaining = as.numeric(df$minutes_remaining)

# Plot 1, do it by calendar day/time
# First limit datapoints so it's not too cluttered
table(df$time)
times <- c("Sunday 12:00 PM","Sunday 12:45 PM","Sunday 1:30 PM",
           "Sunday 2:15 PM", "Sunday 3:00 PM","Sunday 3:45 PM",
           "Sunday 5:00 PM", "Sunday 6:30 PM","Sunday 11:00 PM",
           "Monday 8:30 PM","Monday 11:00 PM")
df2 <- filter(df, time %in% times)
df2$time <- factor(df2$time,levels = times)

plot1 <- ggplot(data=df2,aes(time)) +
  geom_line(aes(y=actual_points,colour=team,group=team),size=2) +
  geom_line(aes(y=projected_points,colour=team,group=team),linetype="dotted",size=2) +
  geom_point(aes(y=actual_points,colour=team,group=team)) +
  facet_wrap(~game,ncol=1) +
  geom_dl(aes(y=actual_points,label=team,color=team),list('smart.grid',cex=0.75)) +
  theme(legend.position='none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Points (Projected and Acutal) by Calendar Day/Time") +
  ylab("Points") +
  xlab("Day/Time")

plot1

#need a dataframe for annotations in first facet only
ann_text <- data.frame(game = rep("Harrington vs. D'Skae",2),
                       projected_points = c(105,80),
                       time = c(1.5,1.5),
                       lab = c("Projected","Actual"))
plot1 +
  geom_text(data=ann_text,aes(y=projected_points,label=lab,size=2)) +
  geom_segment(data=ann_text,aes(x=2.1,xend=2.5,y=108,yend=118),arrow=arrow(length = unit(0.1,'cm'))) +
  geom_segment(data=ann_text,aes(x=2.1,xend=2.5,y=75,yend=75),arrow=arrow(length = unit(0.1,'cm')))

#### Plot 2, projected points by time remaining
plot2 <- ggplot(data=df2,aes(minutes_remaining)) +
  geom_line(aes(y=actual_points,colour=team,group=team),size=2) +
  geom_line(aes(y=projected_points,colour=team,group=team),linetype="dotted",size=2) +
  geom_point(aes(y=actual_points,colour=team,group=team)) +
  scale_x_reverse() +
  facet_wrap(~game,ncol = 1) +
  geom_dl(aes(y=actual_points,label=team,color=team),list('smart.grid',cex=0.75)) +
  theme(legend.position='none') +
  ggtitle("Points (Projected and Acutal) by Minutes Remaining") +
  ylab("Points") +
  xlab("Minutes Remaining")

#need a dataframe for annotations in first facet only
ann_text <- data.frame(game = rep("Harrington vs. D'Skae",2),
                       projected_points = c(100,75),
                       minutes_remaining = c(550,550),
                       lab = c("Projected","Actual"))

plot2 +
  geom_text(data=ann_text,aes(y=projected_points,label=lab,size=2)) +
  geom_segment(data=ann_text,aes(x=520,xend=500,y=100,yend=110),arrow=arrow(length = unit(0.1,'cm'))) +
  geom_segment(data=ann_text,aes(x=520,xend=500,y=75,yend=65),arrow=arrow(length = unit(0.1,'cm'))) 

################################################################################
############       Playoff PW and the eventual champion     ####################
################################################################################

#Get the data
r <- "https://docs.google.com/spreadsheet/pub?key=0ArXHYgdmAzA8dEt4SXNVd2pLdmE0elBxMzNoNm1HZmc&single=true&gid=7&output=csv"
tr <- getURL(r, ssl.verifypeer=FALSE)
regular_season <- read.csv(textConnection(tr),stringsAsFactors=FALSE)

p <- "https://docs.google.com/spreadsheet/pub?key=0ArXHYgdmAzA8dEt4SXNVd2pLdmE0elBxMzNoNm1HZmc&single=true&gid=11&output=csv"
tp <- getURL(p,ssl.verifypeer=FALSE)
playoffs <- read.csv(textConnection(tp),stringsAsFactors=FALSE)

o <- "https://docs.google.com/spreadsheet/pub?key=0ArXHYgdmAzA8dEt4SXNVd2pLdmE0elBxMzNoNm1HZmc&single=true&gid=2&output=csv"
oc <- getURL(o,ssl.verifypeer=FALSE)
owner <- read.csv(textConnection(oc),stringsAsFactors=FALSE)


owner_regular_season <- sqldf('
                              select
                              a.*,
                              b.owner,
                              b.playoffs,
                              b.seed,
                              b.place
                              from
                              regular_season as a,
                              owner as b
                              where
                              a.year = b.year 
                              and a.team = b.team')
owner_playoffs <- sqldf('
                        select
                        a.*,
                        b.owner,
                        b.playoffs,
                        b.seed,
                        b.place
                        from
                        playoffs as a,
                        owner as b
                        where
                        a.year = b.year 
                        and a.team = b.team')

# check to see it all matched up
sqldf('select
      owner,
      count(owner)
      from
      owner_regular_season
      group by
      owner
      order by
      2')

sqldf('select
      owner,
      count(owner)
      from
      owner_playoffs
      group by
      owner
      order by
      2')

## Arrange data and create win, loss and tie variables
data <- arrange(owner_playoffs,game_id,points) %>% filter(game %in% c("W1","W2"))
data$W <- rep(0,dim(data)[1])
data$L <- rep(0,dim(data)[1])
data$T <- rep(0,dim(data)[1])
data$PA <- rep(0,dim(data)[1])
data$opponent <- rep(" ",dim(data)[1])

### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(data)[1],by=2)){
  
  if(data$game[i] != "BYE"){
    
    
    data$PA[i]   <- data$points[i+1]
    data$PA[i+1] <- data$points[i]
    
    data$opponent[i] <- as.character(data$owner[i+1])
    data$opponent[i+1] <- as.character(data$owner[i])
    
    if(data$points[i] < data$points[i+1]){
      data$L[i] <- 1
      data$W[i+1] <- 1
    }
    if(data$points[i] == data$points[i+1]){
      data$T[i] <- 1
      data$T[i+1] <- 1
    }
    
    
  }
  
  else { #Here else means they got a BYE
    data$W[i] <- 1
    data$W[i+1] <- 1
  }
  
}

filter(data,game=="BYE") %>% select(year,points)

## calculate playoff records
playoff_records <- data %>%
  filter(game %in% c("W1","W2"), is.na(points)==FALSE) %>%
  group_by(owner) %>%
  summarise(wins = sum(W),
            losses = sum(L),
            record = paste(wins,losses,sep="-")) %>%
            arrange(desc(wins))
playoff_records

owner %>% select(owner,playoffs) %>% group_by(owner) %>% summarise(playoff_seasons = sum(playoffs))

data %>% filter(owner == 'Olson') %>% select(year,game_id,opponent,points,PA,W,L)


################################################################################
###########                  This week's matchups              #################
################################################################################

## Need this week's games
R32014 <- owner_playoffs %>%
  filter(year==2014,week=="R3") %>%
  select(owner,game_id)

## Now merge with regular season data
round3_preview <- sqldf('
                        select
                          a.owner,
                          a.week,
                          a.points,
                          b.game_id
                        from
                          owner_regular_season as a,
                          R32014 as b
                        where
                          a.year = 2014
                        and a.owner = b.owner')
table(round3_preview$game_id)

round3_preview$game <- factor(round3_preview$game_id,
                              levels <- c("2014R3W1",
                                          "2014R3L3",
                                          "2014R3WC1",
                                          "2014R3L2",
                                          "2014R3WC2",
                                          "2014R3L1"))

levels(round3_preview$game) <- c("Champtionship",
                                 "SAT Bowl",
                                 "3rd Place",
                                 "9th Place",
                                 "5th Place",
                                 "7th Place")

plot3 <- ggplot(data=round3_preview,aes(x=week,y=points,colour=owner))
plot3 +
  geom_point() +
  geom_line(group=owner) +
  facet_wrap(~game,ncol=2) +
  ggtitle("Playoffs Round 3 \n Matchup Preview \n 2014 Regular Season") +
  theme(legend.position = 'none') +
  geom_dl(aes(label=owner),list('top.bumptwice',cex=0.75)) +
  scale_x_continuous("Week",limits = c(1,15)) +
  scale_y_continuous("Points",limits = c(50,200))

### Look up previous playoff matches 
winners_bracket <- owner_playoffs %>% 
  filter(game %in% c("W1","W2"),owner %in% c("Regan","Harrington"),is.na(points) == FALSE) %>%
  arrange(year,week,game)

### Regular season head to head
data2 <- arrange(owner_regular_season,game_id,points)
data2$W <- rep(0,dim(data2)[1])
data2$L <- rep(0,dim(data2)[1])
data2$T <- rep(0,dim(data2)[1])
data2$PA <- rep(0,dim(data2)[1])
data2$opponent <- rep(" ",dim(data2)[1])


### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(data2)[1],by=2)){
  
  data2$PA[i]   <- data2$points[i+1]
  data2$PA[i+1] <- data2$points[i]
  
  data2$opponent[i] <- as.character(data2$owner[i+1])
  data2$opponent[i+1] <- as.character(data2$owner[i])
  
  if(data2$points[i] < data2$points[i+1]){
    data2$L[i] <- 1
    data2$W[i+1] <- 1
  }
  if(data2$points[i] == data2$points[i+1]){
    data2$T[i] <- 1
    data2$T[i+1] <- 1
  }
}

rivalry_data <- data2 %>%
  group_by(owner,opponent) %>%
  summarise(wins = sum(W),
            losses = sum(L),
            ties   = sum(T)) %>%
  mutate(record = paste(wins,'-',losses,'-',ties,sep=""))

record_matrix <- dcast(rivalry_data,owner~opponent,value.var='record')

total_games <- data %>%
  group_by(owner,opponent) %>%
  tally(sort=TRUE)

