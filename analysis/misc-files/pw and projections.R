library(RCurl)
library(dplyr)
library(sqldf)
library(reshape2)
library(ggplot2)

# Get the data
u <- "https://docs.google.com/spreadsheet/pub?key=0ArXHYgdmAzA8dEt4SXNVd2pLdmE0elBxMzNoNm1HZmc&single=true&gid=7&output=csv"
tc <- getURL(u, ssl.verifypeer=FALSE)
games <- read.csv(textConnection(tc),stringsAsFactors=FALSE)


o <- "https://docs.google.com/spreadsheet/pub?key=0ArXHYgdmAzA8dEt4SXNVd2pLdmE0elBxMzNoNm1HZmc&single=true&gid=2&output=csv"
oc <- getURL(o,ssl.verifypeer=FALSE)
owner <- read.csv(textConnection(oc),stringsAsFactors=FALSE)


owner_game <- sqldf('
                    select
                    a.*,
                    b.owner,
                    b.playoffs
                    from
                    games as a,
                    owner as b
                    where
                    a.year = b.year 
                    and a.team = b.team')

# check to see it all matched up
sqldf('select
      owner,
      count(owner)
      from
      owner_game
      group by
      owner
      order by
      2')

## Arrange data and create win, loss and tie variables
data <- arrange(owner_game,game_id,points)
data$W <- rep(0,dim(data)[1])
data$L <- rep(0,dim(data)[1])
data$T <- rep(0,dim(data)[1])
data$PA <- rep(0,dim(data[1]))
data$result <- rep("",dim(data[1]))

### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(data)[1],by=2)){
  
  data$PA[i]   <- data$points[i+1]
  data$PA[i+1] <- data$points[i]
  
  if(data$points[i] < data$points[i+1]){
    data$L[i] <- 1
    data$result[i] <- "L"
    data$W[i+1] <- 1
    data$result[i+1] <- "W"
  }
  if(data$points[i] == data$points[i+1]){
    data$T[i] <- 1
    data$T[i+1] <- 1
    data$result[i] <- "T"
    data$result[i+1] <- "T"
  }
}


data <- data %>%
  arrange(year,week,desc(points)) %>%
  group_by(year,week) %>%
  mutate(rk = rank(points,ties.method="min"))

## got to go right  now but it looks like rank method = min is the one to
## go with, have to subtract one and divide by 10 to get PW

# Now to calculate and add up proportional wins
data$pw<- ifelse(data$year==2009,(data$rk-1)/9,(data$rk-1)/11)

#look at 2014 only, and add up all season and just last five games
pw <- filter(data,year==2014) %>% select(week,W,game_id,owner,pw)

pw_14 <- pw %>% 
          group_by(owner) %>% 
          summarise(sum_pw = sum(pw))
          
pw_l5 <- filter(pw, week >= 7) %>% 
          group_by(owner) %>%
          summarise(sum_pw_l5 = sum(pw))
  
wins <- filter(pw,week < 12) %>%
        group_by(owner) %>%
        summarise(Wins = sum(W))

data_14 <- sqldf('
                 select
                  a.*,
                  b.sum_pw,
                  c.Wins
                 from
                  pw as a,
                  pw_14 as b,
                  wins as c
                 where
                  a.owner = b.owner
                 and a.week >= 12
                 and a.owner = c.owner') 

data_l5 <- sqldf('
                 select
                  a.*,
                  b.sum_pw_l5,
                  c.Wins
                 from
                  pw as a,
                  pw_l5 as b,
                  wins as c
                 where
                  a.owner = b.owner
                 and a.week >= 12
                 and a.owner = c.owner')



for(i in seq(from=1, to=dim(data_14)[1], by = 2)){
data_14$projected_wins[i]   <- data_14$sum_pw[i]/(data_14$sum_pw[i] + data_14$sum_pw[i+1])
data_14$projected_wins[i+1] <- data_14$sum_pw[i+1]/(data_14$sum_pw[i] + data_14$sum_pw[i+1])
}



for(i in seq(from=1, to=dim(data_l5)[1], by = 2)) {
data_l5$projected_wins_l5[i]   <- data_l5$sum_pw_l5[i]/(data_l5$sum_pw_l5[i] + data_l5$sum_pw_l5[i+1])
data_l5$projected_wins_l5[i+1] <- data_l5$sum_pw_l5[i+1]/(data_l5$sum_pw_l5[i] + data_l5$sum_pw_l5[i+1])
}

### add up pw
data_14 <- data_14 %>%
            group_by(owner) %>%
            summarise(sum_projected_wins = sum(projected_wins))

data_l5 <- data_l5 %>%
            group_by(owner) %>%
            summarise(sum_projected_wins = sum(projected_wins_l5))

data_14 <- sqldf('
                 select
                  a.*,
                  b.Wins,
                  b.Wins + a.sum_projected_wins as total_wins
                 from
                  data_14 as a,
                  wins as b
                 where
                  a.owner = b.owner
                 order by
                  total_wins')

data_l5 <- sqldf('
                 select
                  a.*,
                  b.Wins,
                  b.Wins + a.sum_projected_wins as total_wins
                 from
                  data_l5 as a,
                  wins as b
                 where
                  a.owner = b.owner
                 order by
                  total_wins')
