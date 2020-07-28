library(RCurl)
library(dplyr)
library(sqldf)
library(reshape2)
library(ggplot2)

# Get the data
u <- "https://docs.google.com/spreadsheet/pub?key=0ArXHYgdmAzA8dEt4SXNVd2pLdmE0elBxMzNoNm1HZmc&single=true&gid=7&output=csv"
tc <- getURL(u, ssl.verifypeer=FALSE)
games <- read.csv(textConnection(tc))


o <- "https://docs.google.com/spreadsheet/pub?key=0ArXHYgdmAzA8dEt4SXNVd2pLdmE0elBxMzNoNm1HZmc&single=true&gid=2&output=csv"
oc <- getURL(o,ssl.verifypeer=FALSE)
owner <- read.csv(textConnection(oc))


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

### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(data)[1],by=2)){
  
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



data <- data %>%
  arrange(year,week,desc(points)) %>%
  group_by(year,week) %>%
  mutate(rk = rank(points,ties.method="min"))

data$pw<- ifelse(data$year==2009,(data$rk-1)/9,(data$rk-1)/11)

# Loop again to set SOS by pw
data <- arrange(data,game_id,points)
for(i in seq(from=1,to=dim(data)[1],by=2)){
  data$pw_a[i]   <- data$pw[i+1]
  data$pw_a[i+1] <- data$pw[i]
}

# Determine yearly proportional wins and rankings
pw_a_owner <- data %>%
  group_by(year,owner) %>%
  summarise(sum_pw_a = sum(pw_a)) %>%
  arrange(desc(sum_pw_a))

pw_a_owner <- pw_a_owner %>%
  group_by(year) %>%
  mutate(rk  = 13-rank(sum_pw_a,ties.method="max"))

# restructure data
pw_a_year_ranks <- dcast(pw_a_owner,owner~year,value.var='rk')
colnames(pw_a_year_ranks) <- c("owner","y09","y10","y11","y12","y13","y14")
pw_a_year_ranks <- pw_a_year_ranks %>%
  mutate(string = paste(y09,y10,y11,y12,y13,y14,sep="-"))

# Now just by owner, need to normalize by number of games played
pw_a_owner <- data %>%
  group_by(owner) %>%
  summarise(sum_pw_a = sum(pw_a)) %>%
  arrange(desc(sum_pw_a))
pw_gp <- data %>%
  group_by(owner) %>%
  tally()

pw_a_summary <- sqldf('
                    select
                      a.owner,
                      a.sum_pw_a/b.n as avg_percentile,
                      c.string
                    from
                      pw_a_owner as a,
                      pw_gp as b,
                      pw_a_year_ranks as c
                    where
                        a.owner = b.owner
                    and a.owner = c.owner
                    order by
                      2 desc')

quantile(data$points,c(0.46,0.57))