library(XML)
library(stringr)
library(dplyr)
library(stringr)


############################################################################
####                          ESPN Projections                          ####
############################################################################
pos_list <- c(0,2,4,6,16,17)

data <- c()


f <-function(wk,pos){
  pos_week <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?leagueId=725667&scoringPeriodId=",wk,
                                  "&seasonId=2014&slotCategoryId=",
                                  pos_list[pos],sep=""))$playertable_0
  pos_week <- pos_week[-1,]
  pos_week$position = pos_list[pos]
  pos_week$week     <- wk
  return(pos_week)
}


for(i in 1:9){
  for(j in 1:6){
    data<-rbind(data,f(i,j))
  }
}

colnames(data) <- c('player',
                    'bad_newz_team',
                    'v3',
                    'nfl_team',
                    'game_result',
                    'pass_comp_att',
                    'pass_yards',
                    'pass_td',
                    'pass_int',
                    'rush_att',
                    'rush_yds',
                    'rush_td',
                    'rec_att',
                    'rec_yards',
                    'rec_td',
                    'projected_points',
                    'position',
                    'week')

data2 <- tbl_df(data)
data2 <- select(data2,player,week,position,projected_points)
data3 <- data2
data3$player <-sapply(str_split(data3$player,',',2),'[',1)
#remove the asterisk from some players\
data3$player <- str_replace(as.vector(data3$player),'*',"")

write.csv(data3,'/home/john/stats_corner/projections_through_week9.csv')

############################################################################
####              pro-football-reference stats                          ####
############################################################################

actual = c()

g <- function(wk,offset){
  actual <- readHTMLTable(
  paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi",
        "?request=1&match=game&year_min=2014&year_max=2014&season_start=",
        "1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id",
        "=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=",
        wk,
        "&week_num_max=",
        wk,
        "&game_day_of_week=&game_month=&game_location=&game_result=&is_active",
        "=&handedness=&is_hof=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2v",
        "al=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5comp=&c6comp",
        "=&c5gtlt=lt&c6mult=1.0&order_by=fantasy_points&order_by_asc=&offset=",
        offset,
        sep=""))$stats
  actual$week <- wk
  return(actual)
}

for(i in 1:9){
  actual <- rbind(actual,g(i,0),g(i,100),g(i,200))
}
str(actual)
colnames(actual) <- tolower(colnames(actual))

actual <- data.frame(lapply(actual, as.character), stringsAsFactors=FALSE)
colnames(actual)[2] <- "player"
actual <- filter(actual, player != "")
actual$week <- as.numeric(actual$week)
actual$fantpt <- as.numeric(actual$fantpt)
colnames(actual)[26] <- "week"
actual <- actual[,c(2,26,13)]
write.csv(actual,"/home/john/stats_corner/actual_through_week9.csv")
