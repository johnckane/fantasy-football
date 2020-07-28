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


#Hidgon graph
higdon <- data %>%
            filter(owner=='Higdon',
                   year < 2014 | week <= 10)

win_data <- data.frame(cbind(factor(c(2009,2010,2011,2012,2013,2014)),
                  c(7,10,5,3,2,3),
                  c(150,150,150,150,150,150)))
colnames(win_data) <- c("Year","Wins","y2")

higdon_plot <- ggplot(data=higdon,aes(x=as.factor(year),y=points))
higdon_plot +
  geom_point() +
  ggtitle("Higdon Through the Years") +
  scale_y_continuous("Points") +
  scale_x_discrete("Year") +
  stat_smooth(se=FALSE,aes(group=1)) +
  geom_text(data=win_data,aes(x=Year,y=y2,label=Wins)) +
  annotate("text",x=1,y=160,label="# of Wins")

#Matovina graphs
matovina <- data %>%
  filter(owner=='Matovina',
         year < 2014 | week <= 10) %>%
  group_by(year) %>%
  summarise(mean_pts = mean(points))

win_data <- data.frame(cbind(factor(c(2009,2010,2011,2012,2013,2014)),
                             c(6,4,4,3,4,5),
                             c(108,108,108,108,108,108)))
colnames(win_data) <- c("Year","Wins","y2")

matovina_plot <- ggplot(data=matovina,aes(x=as.factor(year),y=mean_pts))
matovina_plot +
  geom_bar(stat="Identity") +
  ggtitle("Matovina Through the Years") +
  scale_y_continuous("Average Points Per Game") +
  scale_x_discrete("Year") +
  geom_text(data=win_data,aes(x=Year,y=y2,label=Wins)) +
  annotate("text",x=1,y=115,label="# of Wins")

# Olson graphs
Cutler <- c(86.2,119.2,94.7,82.5,95.5,109.6,74.4,108.6,68.7)
Olson <- c(107,110.2,83.4,103.5,108.1,138.7,89.8,107.1,94.3)
result <- c("Luke lost","W","L","W","W","W","W","L","L")
week   <- factor(c(1,2,3,4,5,6,7,8,10))
luke_data <- data.frame(week,Cutler,Olson,result)

luke_data2 <- melt(luke_data)


luke_plot1 <- ggplot(data=subset(luke_data2,variable=="Cutler"),
                                 aes(x=week,y=value))
luke_plot1 +
  geom_line(aes(group=1)) +
  geom_point() +
  #geom_text(data=luke_data2,aes(x=week,y=max(value),label=result,color='black')) +
  scale_y_continuous("Cutler Rating/Olson Fantasy Points \n ?") +
  scale_x_discrete("Week") +
  theme(legend.position='none') +
  ggtitle("Luke or Jay?")

luke_plot2 <- ggplot(data=luke_data2,aes(x=week,y=value,group=variable,
                                        colour=variable))
luke_plot2 +
  geom_line() +
  geom_point() +
  geom_dl(aes(label=variable),method="smart.grid") +
  #geom_text(data=luke_data2,aes(x=week,y=max(value),label=result,color='black')) +
  scale_y_continuous("Cutler Rating/Olson Fantasy Points") +
  scale_x_discrete("Week") +
  theme(legend.position='none') +
  ggtitle("Luke and Jay, Forever Intertwined")


#Harrington graph
#calculate sos
# Loop again to set SOS by pw
data <- filter(data,
               year == 2014 & week < 11) %>%
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
            sum_pw   = sum(pw)) %>%
  arrange(desc(sum_pw_a))

harrington_plot <- ggplot(data=pw_a_owner,
                          aes(x=sum_pw_a,y=sum_pw))
harrington_plot +
  geom_point(aes(size=2)) +
#  geom_text("text",
  #         aes(x=sum_pw_a,y=sum_pw,label=owner)) +
  scale_x_continuous("Strength of Schedule \n (Higher is Harder)",
                     limits=c(3,6.5)) +
  scale_y_continuous("Proportional Wins") +
  geom_dl(aes(label=owner),method="smart.grid") +
  theme(legend.position = 'none') +
  ggtitle("Proportional Wins by Strength of Schedule")

