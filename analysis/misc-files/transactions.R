library(RCurl)
library(dplyr)
library(sqldf)
library(reshape2)
library(ggplot2)
library(directlabels)


trans <- read.csv("/home/john/stats_corner/Datasets/trans_counter_week13.csv",header=TRUE)

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

## got to go right  now but it looks like rank method = min is the one to
## go with, have to subtract one and divide by 10 to get PW

# Now to calculate and add up proportional wins
data$pw<- ifelse(data$year==2009,(data$rk-1)/9,(data$rk-1)/11)

#Filter out Week 13 2014
data <- data %>%
  filter(year == 2014 & week < 13)

#summarise
data_summary <- data %>%
  group_by(owner) %>%
  summarise(wins = sum(W),
            sum_pw = sum(pw))

colnames(trans) <- c('owner',
                     'trades',
                     'acq',
                     'drops',
                     'activate')
#combine transaction data with owner data
data2 <- sqldf('
               select
                a.*,
                b.trades,
                b.acq,
                b.drops,
                b.activate
               from
                data_summary as a,
                trans as b
               where
                a.owner = b.owner')

#create the label variable
data2$label <- paste(data2$owner,", ",data2$wins,sep="")

#Now the plots. First up trades
plot1 <- ggplot(data=data2,aes(x=trades,y=sum_pw,label=label))
plot1+
  geom_point() +
  scale_x_continuous("# of Trades Made",
                     limits=c(0,11)) +
  scale_y_continuous("Proportional Wins") +
  geom_dl(aes(label=owner),method="smart.grid") +
  theme(legend.position = 'none') +
  ggtitle("Proportional Wins by Trades Made") +
  geom_smooth(method=lm,se=FALSE)

#Now acquisitions
plot2 <- ggplot(data=data2,aes(x=acq,y=sum_pw,label=label))
plot2+
  geom_point() +
  scale_x_continuous("# of Acquisitions (FA Signings)",
                     limits=c(0,40)) +
  scale_y_continuous("Proportional Wins") +
  geom_dl(aes(label=owner),method="smart.grid") +
  theme(legend.position = 'none') +
  ggtitle("Proportional Wins by Acquisitions") +
  geom_smooth(method=lm,se=FALSE)

#Now activations
plot3 <- ggplot(data=data2,aes(x=activate,y=sum_pw,label=label))
plot3+
  geom_point() +
  scale_x_continuous("# of Activations (Substitutions)",
                     limits=c(25,70)) +
  scale_y_continuous("Proportional Wins") +
  geom_dl(aes(label=owner),method="smart.grid") +
  theme(legend.position = 'none') +
  ggtitle("Proportional Wins by Activations") +
  geom_smooth(method=lm,se=FALSE)