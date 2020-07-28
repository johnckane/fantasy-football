setwd("/home/john/NFL/NFLData_2000-2014/csv")
library(sqldf)
library(dplyr)
library(ggplot2)

players <- tbl_df(read.csv("PLAYER.csv", stringsAsFactors = FALSE, header = TRUE))
offense <- tbl_df(read.csv("OFFENSE.csv", stringsAsFactors = FALSE, header = TRUE))

colnames(players) <- tolower(colnames(players))
colnames(offense) <- tolower(colnames(offense))

## Eliminate rows we don't need to speed this up, only need data for 2010 and onward
offense <- filter(offense, year >= 2010 )
offense <- select(offense, team, player, py, int, tdp, ry, tdr, recy, tdre, fuml, year)

offense <- offense %>%
    mutate(fantasy_points = (0.04*py) + (-2*int) + (4*tdp) + (0.1*ry) + (6*tdr) + (0.1*recy) + (6*tdre) + (-2*fuml)) %>%
    group_by(player,year) %>%
    summarise(points = mean(fantasy_points),
              rectd  = sum(tdre)) %>%
    arrange(points)
## Now join with player data

player_data <- sqldf('select
                         a.fname, 
                         a.lname,
                         a.pos1 as pos,
                         b.year,
                         b.points,
                         b.rectd
                    from
                         players as a,
                         offense as b
                    where
                         a.player = b.player')

player_data <- player_data %>%
     mutate(name = paste(fname, lname, sep = " ")) %>%
     select(name,year,points,pos,rectd)





## Bring in ADP
setwd("/home/john/Fantasy Football/2015 Prep")
adp <- read.csv("adp.csv", header = TRUE, stringsAsFactors = FALSE)

pos_to_use = c("QB", "RB", "WR", "TE")
adp <- adp %>% filter(pos %in% pos_to_use)

player_data_adp <- sqldf('
                         select 
                              a.year,
                              a.name,
                              a.pos,
                              a.pick,
                              b.points,
                              b.rectd
                         from
                              adp as a
                         left join
                             player_data as b
                         on
                             a.year = b.year
                         and a.name = b.name
                         and a.pos  = b.pos
                         order by
                            a.year,
                            a.pos,
                            a.pick')

#Look for dups, I think there should be 10 of them.
table(player_data_adp$year,player_data_adp$pos,player_data_adp$pick)
#investiagte dups
filter(player_data_adp,year==2010,pos=="TE",pick==11)
filter(player_data_adp,year==2011,pos=="TE",pick==12)
filter(player_data_adp,year==2011,pos=="WR",pick==13)
filter(player_data_adp,year==2010,pos=="WR",pick==14)
filter(player_data_adp,year==2010,pos=="WR",pick==15)
filter(player_data_adp,year==2012,pos=="WR",pick==17)
filter(player_data_adp,year==2010,pos=="WR",pick==20) #ok
filter(player_data_adp,year==2011,pos=="WR",pick==20) #ok
filter(player_data_adp,year==2011,pos=="WR",pick==35)
filter(player_data_adp,year==2011,pos=="WR",pick==39) #ok
filter(player_data_adp,year==2011,pos=="WR",pick==53)
filter(player_data_adp,year==2010,pos=="WR",pick==61)
filter(player_data_adp,year==2010,pos=="WR",pick==71) #ok

## using pro-football-reference data I think I've found a way to select the appropriate duplciatesy_
my_which <- function(name,pick,year,rectd){
    return(which(player_data_adp$name  == name  &
                 player_data_adp$rectd == rectd &
                 player_data_adp$pick  == pick  &
                 player_data_adp$year  == year))
}
my_which("Zach Miller",11,2010,5)
to_remove <- c(my_which("Zach Miller",11,2010,5),
               my_which("Steve Smith",14,2010,2),
               my_which("Steve Smith",15,2010,3),
               my_which("Mike Williams",47,2010,5),
               my_which("Mike Williams",61,2010,11),
               my_which("Zach Miller",12,2011,0),
               my_which("Mike Williams",13,2011,1),
               my_which("Steve Smith",35,2011,1),
               my_which("Steve Smith",53,2011,7),
               my_which("Steve Smith",17,2012,0))





# possible name matching errors
filter(player_data_adp,is.na(points)==TRUE)
# the following found by doing:
    filter(players,lname=="Jones",pos1 == "WR")
    filter(players,lname=="Jones",fname=="James")
# Beanie Wells goes by Chris Wells in 'players' file
# Carnell Williams goes by Cadillac Williams
# Joshua Cribbs goes by Josh Cribbs
# Robert Griffin III goes by Robert Griffin
# LeVeon Bell goes by Le 'Veon Bell
# T.Y. Hilton goes by Ty Hilton
# EJ Manuel goes by E.J. Manuel

adp$name <- ifelse(adp$name == "Beanie Wells", "Chris Wells", adp$name) 
adp$name <- ifelse(adp$name == "Carnell Williams", "Cadillac Williams", adp$name) 
adp$name <- ifelse(adp$name == "Joshua Cribbs", "Josh Cribbs", adp$name) 
adp$name <- ifelse(adp$name == "Robert Griffin III", "Robert Griffin", adp$name) 
adp$name <- ifelse(adp$name == "LeVeon Bell", "Le'Veon Bell", adp$name)
adp$name <- ifelse(adp$name == "T.Y. Hilton", "Ty Hilton", adp$name) 
adp$name <- ifelse(adp$name == "EJ Manuel", "E.J. Manuel", adp$name)
adp$name <- ifelse(adp$name == "Mike Sims-Walker", "Mike Walker", adp$name)



player_data_adp2 <- sqldf('
                         select 
                            a.year,
                            a.name,
                            a.pos,
                            a.pick,
                            b.points,
                            b.rectd
                         from
                            adp as a
                         left join
                            player_data as b
                         on
                            a.year = b.year
                         and a.name = b.name
                         and a.pos  = b.pos
                         order by
                            a.year,
                            a.pos,
                            a.pick')
filter(player_data_adp2,is.na(points)==TRUE)

my_which <- function(name,pick,year,rectd){
    return(which(player_data_adp2$name  == name  &
                     player_data_adp2$rectd == rectd &
                     player_data_adp2$pick  == pick  &
                     player_data_adp2$year  == year))
}
my_which("Zach Miller",11,2010,5)
to_remove <- c(my_which("Zach Miller",11,2010,5),
               my_which("Steve Smith",14,2010,2),
               my_which("Steve Smith",15,2010,3),
               my_which("Mike Williams",47,2010,5),
               my_which("Mike Williams",61,2010,11),
               my_which("Zach Miller",12,2011,0),
               my_which("Mike Williams",13,2011,1),
               my_which("Steve Smith",35,2011,1),
               my_which("Steve Smith",53,2011,7),
               my_which("Steve Smith",17,2012,0))



player_data_adp2$pick <- floor(player_data_adp$pick)
player_data_adp2 <- player_data_adp2[-to_remove,]


write.csv(player_data_adp2, "player_data_adp.csv")

## Now visualize
plot <- ggplot(data=player_data_adp,aes(x=pick, y=points)) +
    geom_point(aes(colour=as.factor(year))) +
    facet_wrap(~pos,ncol=1)  
plot + geom_smooth(method="loess") + theme(legend.position=c(0.9, 0.9)) + scale_y_continuous(limits=c(0,35))
