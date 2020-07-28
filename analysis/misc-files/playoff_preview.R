library(RCurl)
library(dplyr)
library(sqldf)
library(reshape2)
library(ggplot2)
library(directlabels)
library(gridExtra)


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


### Examine records
arrange(owner_playoffs,desc(points)) %>% select(year,game_id,owner,points)
filter(owner_playoffs,game_id=='2013R2L2')

#first round byes
filter(owner_playoffs,game=='BYE') %>%
  select(year,owner)
  group_by(owner) #%>%
  summarise(total_byes = n()) #%>%
  arrange(desc(total_byes))

## Arrange data and create win, loss and tie variables
data <- arrange(owner_playoffs,game_id,points) %>% filter(year != 2014)
data$W <- rep(0,dim(data)[1])
data$L <- rep(0,dim(data)[1])
data$T <- rep(0,dim(data)[1])
data$PA <- rep(0,dim(data)[1])

### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(data)[1],by=2)){
  
  if(data$game[i] != "BYE"){
  
    print("not bye")
  
  data$PA[i]   <- data$points[i+1]
  data$PA[i+1] <- data$points[i]
  
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

#filter out non-winner's bracket games
data <- data %>%
  filter(game  %in% c('W1','W2'))

data %>% 
  group_by(seed,week) %>% 
  summarise(appearances = n(),
            wins = sum(W),
            losses = sum(L),
            win_pct = wins/appearances) %>%
  mutate(record = paste(wins,losses,sep=" - "))

## Now for the dot plots dileanating regular season by playoff games 
## by color, for each year since 2009.

regular_season <- filter(owner_regular_season,playoffs==1)
post_season    <- filter(owner_playoffs,playoffs == 1, year != 2014)

#data frame to label points

plot1 <- ggplot(data=regular_season, aes(x=as.character(seed),y=points))
plot1 +
  geom_point(colour='black') +
  geom_point(data=post_season,aes(x=as.character(seed),y=points),colour='red') +
  facet_wrap(~year,ncol=3) +
  geom_text(data=regular_season,
           aes(x=as.character(seed),y=40,label=owner,angle = 45,
           size=4,
           family = "Times",
           fontface = "plain")) +
  #geom_dl(aes(label=owner),method="angled.endpoints") +
  theme(legend.position = 'none') +
  scale_y_continuous("Points",limits=c(0, 185)) +
  scale_x_discrete("Seed",labels=owner) +
  ggtitle("Regular Season and Playoff Points by Seed \n Black = Reg. Season; Red = Playoffs") 
  
### Calculate proportional wins by last 6 and all 13


data2 <- arrange(owner_regular_season,game_id,points)
data2$W <- rep(0,dim(data2)[1])
data2$L <- rep(0,dim(data2)[1])
data2$T <- rep(0,dim(data2)[1])
data2$PA <- rep(0,dim(data2[1]))

### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(data2)[1],by=2)){
  
  if(data2$game[i] != "BYE"){
    
    
    data2$PA[i]   <- data2$points[i+1]
    data2$PA[i+1] <- data2$points[i]
    
    if(data2$points[i] < data2$points[i+1]){
      data2$L[i] <- 1
      data2$W[i+1] <- 1
    }
    if(data2$points[i] == data2$points[i+1]){
      data2$T[i] <- 1
      data2$T[i+1] <- 1
    }
    
    
  }
  
  else { #Here else means they got a BYE
    data2$W[i] <- 1
    data2$W[i+1] <- 1
  }
  
}


data2 <- data2 %>%
  arrange(year,week,desc(points)) %>%
  group_by(year,week) %>%
  mutate(rk = rank(points,ties.method="min"))

## got to go right  now but it looks like rank method = min is the one to
## go with, have to subtract one and divide by 10 to get PW

# Now to calculate and add up proportional wins
data2$pw<- ifelse(data2$year==2009,(data2$rk-1)/9,(data2$rk-1)/11)

### first all 13, by year. 
pw_all_13 <- data2 %>% 
  group_by(owner,year) %>%
  summarise(all_13_pw = sum(pw))

pw_last_6 <- data2 %>%
  filter(week >= 8) %>%
  group_by(owner,year) %>%
  summarise(last_6_pw = sum(pw))

pw_data <- sqldf('
                 select
                  a.owner,
                  a.year,
                  a.all_13_pw,
                  b.last_6_pw
                 from
                  pw_all_13 as a,
                  pw_last_6 as b
                 where
                  a.year = b.year
                 and a.owner = b.owner')

pw_playoffs <- sqldf('
                     select
                      a.*,
                      b.all_13_pw,
                      b.last_6_pw
                     from
                      data as a,
                      pw_data as b
                     where
                      a.playoffs = 1
                     and a.owner = b.owner
                     and a.year = b.year')

# first plot, last 6
plot2 <- ggplot(data=pw_playoffs,aes(x=as.factor(W),y=last_6_pw))
plot2 +
  geom_point() +
  facet_grid(year~week)

# now all 13
plot2 <- ggplot(data=pw_playoffs,aes(x=as.factor(W),y=all_13_pw))
plot2 +
  geom_point() +
  facet_grid(year~week)

# neither of these are compelling

#quick scatterplot of playoffspoints by pw
qplot(data=pw_playoffs,x=last_6_pw,y=points,colour=W)
qplot(data=pw_playoffs,x=all_13_pw,y=points,colour=W)

########################################
# This week's matchups, season summary #
########################################

## Need this week's games
R12014 <- owner_playoffs %>%
  filter(year==2014) %>%
  select(owner,game_id)

## Now merge with regular season data
round1_preview <- sqldf('
                        select
                          a.owner,
                          a.week,
                          a.points,
                          b.game_id
                        from
                          owner_regular_season as a,
                          R12014 as b
                        where
                          a.year = 2014
                        and a.owner = b.owner')

round1_preview$game <- factor(round1_preview$game_id,
                              levels <- c("2014R1W1",
                                          "2014R1L1",
                                          "2014R1W2",
                                          "2014R1L2",
                                          "2014R1BYE",
                                          "2014R1L3"))
levels(round1_preview$game) <- c("Winner's Bracket 5 vs. 6",
                                 "Loser's Bracket 7 vs. 8",
                                 "Winner's Bracket 3 vs. 4",
                                 "Loser's Bracket 9 vs. 10",
                                 "Winner's Bracket BYE",
                                 "Loser's Bracket 11 vs. 12")

plot4 <- ggplot(data=round1_preview,aes(x=week,y=points,colour=owner))
plot4 +
  geom_point() +
  geom_line(group=owner) +
  facet_wrap(~game,ncol=2) +
  ggtitle("Playoffs Round 1 \n Matchup Preview") +
  theme(legend.position = 'none') +
  geom_dl(aes(label=owner),list('top.bumptwice',cex=0.75)) +
  scale_x_continuous("Week",limits = c(1,15)) +
  scale_y_continuous("Points",limits = c(50,200))
