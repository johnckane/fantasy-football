library(RCurl)
library(dplyr)
library(sqldf)
library(reshape2)
library(ggplot2)
library(Hmisc)

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
### Box plot of points vs win/loss
p1 <- ggplot(data=data,aes(x=as.factor(W),y=points))
p1 + geom_boxplot()
t.test(data$points~data$W)
summary(data$points[data$W==1])
summary(data$points[data$W==0])

p2 <- ggplot(data=data,aes(x=points)) 
p2 + geom_histogram() + facet_grid(.~W)
 
# Create the bins
data$bin <- cut2(data$points,c(70,80,90,100,110,120,130))

data <- filter(data,year != 2014)

data2 <-
  data %>%
  group_by(bin) %>%
  summarise(wins  = sum(W),
            games = n()) %>%
  mutate(w_pct = wins/games)

plot <- ggplot(data=data2,
               aes(x=bin,y=100*w_pct))               
plot + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=paste(round(100*w_pct,1),"%",sep=""),y=100*w_pct+2),
            size=3) +
  theme_bw() +
  scale_y_continuous("% of Games Won") +
  scale_x_discrete("Points Scored") +
  labs(title="Winning % By Points Scored \n (2009 - 2013)")


#compare points scored in 2009 (decimal scoring) to ever since
data %>%
  group_by(year) %>%
  summarise(total_points = sum(points))
# adjusting 2009 points for 12 teams, renders 15836.4, so right in line
# with decimal scoring years, no need to adjust individual game levels,
# in my opinion

# Now look at it by year, examine whether the winning percentage by bin
# is changing year to year

data3 <-
  data %>%
  group_by(year,bin) %>%
  summarise(wins  = sum(W),
            games = n()) %>%
  mutate(w_pct = wins/games)

plot <- ggplot(data=data3,
               aes(x=bin,y=w_pct,
                   label = paste(round(100*w_pct,1),"%",sep="")),
               fill=w_pct)
plot + geom_bar(stat="identity") + geom_text() + facet_grid(year~.)
  
### Now let's evaluate bye-weeks effect on scoring

data4 <- filter(data,year != 2014)

data4 <- mutate(data4,bye_week = ifelse(
    (year == 2009 &  week >= 4 & week <= 10) |
    (year == 2010 &  week >= 4 & week <= 10) |
    (year == 2011 & ((week >= 5 & week <=  9) | week == 11)) |
    (year == 2012 &  week >= 4 & week <= 11) |
    (year == 2013 &  week >= 4 & week <= 12),"Yes","No")
)

# Now summarise
data4b <- data4 %>%
  group_by(bin,bye_week) %>%
  summarise(wins  = sum(W),
            games = n()) %>%
  mutate(w_pct = wins/games)

plot <- ggplot(data=data4b,
               aes(x=bin,y=w_pct*100,
                   fill = bye_week))
plot + 
  geom_bar(stat="identity",position="dodge") + 
  geom_text(aes(label=paste(round(100*w_pct,1),"%",sep=""),y=100*w_pct+2),
                  position=position_dodge(width=1),
                  size=3) +
  theme_bw() +
  scale_y_continuous("% of Games Won") +
  scale_x_discrete("Points Scored") +
  scale_fill_discrete("Bye Week?") +
  labs(title="Winning % By Points Scored and Bye Week Status \n (2009 - 2013)")

# Can't explain the discrpency for the [100,110) bin. Let's take a look
data4b

### Now do this by year as well
data5 <- data4 %>%
  group_by(bin,year,bye_week) %>%
  summarise(wins  = sum(W),
            games = n()) %>%
  mutate(w_pct = wins/games)

plot <- ggplot(data=data5,
               aes(x=bin,y=w_pct*100,fill=bye_week))
               
plot + 
  facet_grid(year~.) +
  geom_bar(stat="identity",position="dodge") + 
  geom_text(aes(label=paste(round(100*w_pct,1),"%",sep=""),y=100*w_pct+2),
            position=position_dodge(width=1)) +
  theme_bw() +
  scale_y_continuous("% of Games Won") +
  scale_x_discrete("Points Scored") +
  scale_fill_discrete("Bye Week?") +
  labs(title="Winning % By Points Scored and Bye Week Status \n (Since 2009)")

  