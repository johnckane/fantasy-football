library(RCurl)
library(dplyr)
library(sqldf)
library(reshape2)

# Get the data
u <- "https://docs.google.com/spreadsheet/pub?key=0ArXHYgdmAzA8dEt4SXNVd2pLdmE0elBxMzNoNm1HZmc&single=true&gid=7&output=csv"
tc <- getURL(u, ssl.verifypeer=FALSE)
games <- read.csv(textConnection(tc))


o <- "https://docs.google.com/spreadsheet/pub?key=0ArXHYgdmAzA8dEt4SXNVd2pLdmE0elBxMzNoNm1HZmc&single=true&gid=2&output=csv"
oc <- getURL(o,ssl.verifypeer=FALSE)
owner <- read.csv(textConnection(oc))

# Remove owner data from 2014, we don't know who made the playoffs yet
owner <- filter(owner,year != 2014)

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

### Loop to determine winners
for(i in seq(from=1,to=dim(data)[1],by=2)){
  if(data$points[i] < data$points[i+1]){
    data$L[i] <- 1
    data$W[i+1] <- 1
  }
  if(data$points[i] == data$points[i+1]){
    data$T[i] <- 1
    data$T[i+1] <- 1
  }
  }



# This version includes all record options, including ties. While nice, it muddies up
# the presentation. I will re-do this citing only wins rather than record
f <- function(x){
data09 <- data %>% 
  filter(year==2009,week<=x) %>%
  group_by(team,playoffs) %>%    # no need to group by year, all 2009 teams
  summarise(wins = sum(W),
            losses = sum(L),
            ties   = sum(T)) %>%
  mutate(record = paste(wins,'-',losses,'-',ties)) %>%
  group_by(record) %>% 
  summarise(yeas_ = sum(playoffs),
            ttl  = n(),
            playoff_rate = yeas_/ttl,
            yeas = ttl*playoff_rate**(log(0.5)/log(0.6))) %>%
  select(record,ttl,yeas)

data_all <- data %>% 
  filter(year != 2009,week<=x) %>%
  group_by(year,team,playoffs) %>%
  summarise(wins = sum(W),
            losses = sum(L),
            ties   = sum(T)) %>%
  mutate(record = paste(wins,'-',losses,'-',ties)) %>%
  group_by(record) %>%
  summarise(yeas = sum(playoffs),
            ttl  = n()) %>%
  select(record,ttl,yeas)

all_data <- sqldf('
                  select 
                    *
                  from
                    data09
                  union
                  select
                    *
                  from 
                    data_all')
data_summary <- 
  all_data %>%
  group_by(record) %>%
  summarise(total_yeas = sum(yeas),
            total      = sum(ttl),
            playoff_rate = total_yeas/total)

return(data_summary)
}

g <- function(x){
  data09 <- data %>% 
    filter(year==2009,week<=x) %>%
    group_by(team,playoffs) %>%    # no need to group by year, all 2009 teams
    summarise(wins = sum(W)) %>%
    group_by(wins) %>% 
    summarise(yeas_ = sum(playoffs),
              ttl  = n(),
              playoff_rate = yeas_/ttl,
              yeas = ttl*playoff_rate**(log(0.5)/log(0.6))) %>%
    select(wins,ttl,yeas)
  
  data_all <- data %>% 
    filter(year != 2009,week<=x) %>%
    group_by(year,team,playoffs) %>%
    summarise(wins = sum(W)) %>%
    group_by(wins) %>%
    summarise(yeas = sum(playoffs),
              ttl  = n()) %>%
    select(wins,ttl,yeas)
  
  all_data <- sqldf('
                  select 
                    *
                  from
                    data09
                  union
                  select
                    *
                  from 
                    data_all')
  data_summary <- 
    all_data %>%
    group_by(wins) %>%
    summarise(total_yeas = sum(yeas),
              total      = sum(ttl),
              playoff_rate = total_yeas/total) %>%
    mutate(week = x)
  
  return(data_summary)
}
full_data <- rbind(g(1),g(2),g(3),g(4),g(5),g(6),g(7),g(8),g(9),g(10),g(11),g(12),g(13))

rate_matrix <- dcast(full_data,week~wins,value.var='playoff_rate')

full_data$round_prob = paste(format(round(full_data$playoff_prob*100)),"%",sep="")

## Create the playoff rate plot
ggplot(full_data, aes(x=week,y=wins,fill = playoff_prob,label=round_prob)) + 
  geom_raster() +
  scale_fill_gradient(low="blue", high="red", na.value="black", name="") +
  theme_bw() +
  geom_text() +
  scale_x_continuous("After Week X",breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) +
  scale_y_continuous("Number of Wins",breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(title='Bad Newz Historical Playoff Rates') +
  theme(legend.position = 'none',panel.grid.minor=element_line(colour='white'))

## Create the sample size plot
ggplot(full_data, aes(x=week,y=wins,fill = total,label=total)) + 
  geom_raster() +
  scale_fill_gradient(low="blue", high="red", na.value="black", name="") +
  theme_bw() +
  geom_text() +
  scale_x_continuous("After Week X",breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) +
  scale_y_continuous("Number of Wins",breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(title='Bad Newz Historical Playoff Rates Sample Sizes') +
  theme(legend.position = 'none',panel.grid.minor=element_line(colour='white'))
  
