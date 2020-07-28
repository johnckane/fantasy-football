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

# Through 6 weeks (make it 7 after this week's games) compare worst defenses
# First by total points
data %>% 
  filter(week <= 7) %>%
  group_by(year,owner) %>%
  summarise(total_pa = sum(PA)) %>%
  arrange(desc(total_pa)) %>%
  top_n(25)

# Second by average points by week 
avg_pa <- data %>%
  filter(week <= 7) %>%
  group_by(year,week) %>%
  summarise(avg_pa = mean(PA),
            sd_pa = sd(PA)) 

seven_weeks <- filter(data, week <= 7)

pa_above_avg <- sqldf('select
                        a.*,
                        b.avg_pa,
                        PA-b.avg_pa as pa_above_avg,
                        (PA-b.avg_pa)/b.sd_pa as std_pa
                      from
                        seven_weeks as a,
                        avg_pa as b
                      where
                          a.week = b.week
                      and a.year = b.year')

pa_above_avg %>% 
  group_by(year,owner) %>%
  summarise(total_pa_sd = sum(std_pa)) %>%
  arrange(desc(total_pa_sd))

### Let's make some dot plots now for those top worst PA teams

pa_above_avg <- mutate(pa_above_avg, year_team = paste(year,owner,sep= " "))

teams <- c('2012 Higdon','2013 Olson','2010 Skrzyskewski', '2010 Ready',
           '2010 Shokunbi' ,'2009 Regan','2014 Harrington','2011 Higdon',
           '2009 Higdon','2012 Kane')

dotplot_data <- filter(pa_above_avg,year_team %in% teams)

dotplot_data$year_team <- as.factor(dotplot_data$year_team)

levels(dotplot_data$year_team) <- teams

plot <- ggplot(data=dotplot_data,aes(x=as.factor(year_team),y=std_pa,colour='black'))
plot+
  geom_jitter(position=position_jitter(width=0.1),size=2) +
  theme(axis.text.x = element_text(angle = 45,hjust=1,size=16),
        legend.position='none') +
  geom_hline(yintercept=0) +
  ylab("Points Against Standard Deviations") +
  xlab("Team") +
  ggtitle("Most Points Against Through 7 Weeks")
  

