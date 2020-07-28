library(RCurl)
library(dplyr)
library(sqldf)
library(reshape2)
library(ggplot2)
library(directlabels)

#load data
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
data$PA <- rep(0,dim(data)[1])
data$result <- rep("",dim(data)[1])

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

data$pw<- ifelse(data$year==2009,(data$rk-1)/9,(data$rk-1)/11)

#Harrington plot
#calculate sos
# Loop again to set SOS by pw
data <- filter(data,
               year == 2014 & week < 12) %>%
  arrange(game_id,points)

data$pw_a <- rep(0,dim(data)[1])

for(i in seq(from=1,to=dim(data)[1],by=2)){
  data$pw_a[i]   <- data$pw[i+1]
  data$pw_a[i+1] <- data$pw[i]
}

# Determine yearly proportional wins and rankings
pw_a_owner <- data %>%
  group_by(year,owner) %>%
  summarise(sum_pw_a = sum(pw_a),
            sum_w  = sum(W)) %>%
  arrange(desc(sum_pw_a))

harrington_plot <- ggplot(data=pw_a_owner,
                          aes(x=sum_pw_a,y=sum_w))
harrington_plot +
  geom_point(aes(size=2)) +
  scale_x_continuous("Strength of Schedule \n (Higher is Harder)",
                     limits=c(3,6.5)) +
  scale_y_continuous("Wins") +
  geom_dl(aes(label=owner),method="smart.grid") +
  theme(legend.position = 'none') +
  ggtitle("Proportional Wins by Strength of Schedule")