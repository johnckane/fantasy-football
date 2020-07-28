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
data$opponent <- rep(" ",dim(data[1]))


### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(data)[1],by=2)){
  
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

rivalry_data <- data %>%
  group_by(owner,opponent) %>%
  summarise(wins = sum(W),
            losses = sum(L),
            ties   = sum(T)) %>%
  mutate(record = paste(wins,'-',losses,'-',ties,sep=""))

record_matrix <- dcast(rivalry_data,owner~opponent,value.var='record')

total_games <- data %>%
  group_by(owner,opponent) %>%
  tally(sort=TRUE)

## Create the playoff rate plot
ggplot(rivalry_data, aes(x=opponent,y=owner,label=record)) + 
  theme_bw() + 
  geom_text(aes(size=4)) +
  scale_x_discrete("Opponent") +
  scale_y_discrete("You", limits=rev(levels(rivalry_data$owner))) +
  labs(title='Bad Newz Head to Head Records \n (Through Week 7 2014)') +
  theme(legend.position = 'none',panel.grid.minor=element_line(colour='white'),
    axis.text.x=element_text(angle=45,hjust=1,size=12),
    axis.text.y=element_text(size=12))

