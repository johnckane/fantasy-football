library(RCurl)
library(dplyr)
library(sqldf)
library(reshape2)
library(ggplot2)
library(directlabels)

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


data <- data %>%
  arrange(year,week,desc(points)) %>%
  group_by(year,week) %>%
  mutate(rk = rank(points,ties.method="min"))

# Now to calculate and add up proportional wins
data$pw<- ifelse(data$year==2009,(data$rk-1)/9,(data$rk-1)/11)

data$year_week <- substr(as.character(data$game_id),1,6)


points_by_week <- data %>%
                    group_by(year_week) %>%
                    summarise(mean_pts = mean(points)) %>%
                    arrange(desc(mean_pts))

points_by_week$points_z <- (points_by_week$mean_pts - mean(points_by_week$mean_pts))/sd(points_by_week$mean_pts)


# Rank everyone's best games
rank_by_owner <- data %>%
                  group_by(owner) %>%
                  arrange(owner,desc(points)) %>%
                  #here we rank by -points to get descending rank
                  mutate(rk = rank(-points,ties.method="min"))

year_week_ranks <- filter(rank_by_owner,year_week=="201408")
select(year_week_ranks,owner,rk) %>% arrange(rk)

### Pull # of games to get percentile
games <- sqldf('select
      owner,
      count(owner) as games
      from
      owner_game
      group by
      owner
      order by
      2')

year_week_ranks <- filter(rank_by_owner,year_week=="201408") %>%
  select(year_week_ranks,owner,rk) %>% 
  arrange(rk)


percentile <- sqldf('
                    select
                      a.owner,
                      a.rk,
                      b.games
                    from
                      games as b,
                      year_week_ranks as a
                    where 
                      a.owner = b.owner')
percentile <- mutate(percentile,p = 1-(rk-1)/games) %>% arrange(rk)


### Calculate average score by week ###
by_week <- data %>%
  filter(week==8,year != 2014) %>%
  group_by(week) %>%
  summarise(mean_points = mean(points))
  