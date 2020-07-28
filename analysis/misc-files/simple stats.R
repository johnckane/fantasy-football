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
owner <- filter(owner,year!= 2014)

games %>% group_by(team,year) %>% summarise(ttl_pts = sum(points)) %>% arrange(ttl_pts)
## They all check out!! Good 

## Top scoring games of all time
arrange(games,desc(points))


## Top scoring seasons of all time
games %>% group_by(team,year) %>% summarise(ttl_pts = sum(points)) %>% arrange(desc(ttl_pts))

# Number of playoff appearances by each owner
owner %>% group_by(owner) %>% summarise(ps=sum(playoffs)) %>% arrange(desc(ps))

# Merge owner data with game data
owner_game <- sqldf('
                select
                    a.*,
                    b.owner
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
### It does!

# Determine points per game
ppg <- sqldf('select
        owner,
        sum(points)/count(owner) as ppg
       from
        owner_game
       group by
        owner
       order by
        2')
arrange(ppg,desc(ppg))


test <- dcast(owner_game,game_id~owner,value.var="points",fill=0)
test$max <- apply(test[,-1],1,max)
test$rsum <- apply(test[,-1],1,sum)


### Very sloppy method to get records and winning percentages 
owners <- c('Kane','Ready','Shokunbi','Thieneman','Olson','Skrzyskewski','Hartman',
            'Harrington','Matovina','McShane','Regan','Higdon')
wins   <- c(sum(test$Kane         == test$max & test$Kane         != test$rsum/3),
            sum(test$Ready        == test$max & test$Ready        != test$rsum/3),
            sum(test$Shokunbi     == test$max & test$Shokunbi     != test$rsum/3),
            sum(test$Thieneman    == test$max & test$Thieneman    != test$rsum/3),
            sum(test$Olson        == test$max & test$Olson        != test$rsum/3),
            sum(test$Skrzyskewski == test$max & test$Skrzyskewski != test$rsum/3),
            sum(test$Hartman      == test$max & test$Hartman      != test$rsum/3),
            sum(test$Harrington   == test$max & test$Harrington   != test$rsum/3),
            sum(test$Matovina     == test$max & test$Matovina     != test$rsum/3),
            sum(test$McShane      == test$max & test$McShane      != test$rsum/3),
            sum(test$Regan        == test$max & test$Regan        != test$rsum/3),
            sum(test$Higdon       == test$max & test$Higdon       != test$rsum/3))
losses <- c(sum(test$Kane         != test$max & test$Kane         > 0),
            sum(test$Ready        != test$max & test$Ready        > 0),
            sum(test$Shokunbi     != test$max & test$Shokunbi     > 0),
            sum(test$Thieneman    != test$max & test$Thieneman    > 0),
            sum(test$Olson        != test$max & test$Olson        > 0),
            sum(test$Skrzyskewski != test$max & test$Skrzyskewski > 0),
            sum(test$Hartman      != test$max & test$Hartman      > 0),
            sum(test$Harrington   != test$max & test$Harrington   > 0),
            sum(test$Matovina     != test$max & test$Matovina     > 0),
            sum(test$McShane      != test$max & test$McShane      > 0),
            sum(test$Regan        != test$max & test$Regan        > 0),
            sum(test$Higdon       != test$max & test$Higdon       > 0))
ties <-   c(sum(test$Kane         == test$rsum/3),
            sum(test$Ready        == test$rsum/3),
            sum(test$Shokunbi     == test$rsum/3),
            sum(test$Thieneman    == test$rsum/3),
            sum(test$Olson        == test$rsum/3),
            sum(test$Skrzyskewski == test$rsum/3),
            sum(test$Hartman      == test$rsum/3),
            sum(test$Harrington   == test$rsum/3),
            sum(test$Matovina     == test$rsum/3),
            sum(test$McShane      == test$rsum/3),
            sum(test$Regan        == test$rsum/3),
            sum(test$Higdon       == test$rsum/3))
#totals <- c(65,65,65,65,52,52,65,65,65,65,65,65)
wlt <- data.frame(owners,wins,losses,ties)

wlt <- mutate(wlt,totals = wins+losses+ties,
                  wpct = wins/totals)
arrange(wlt,desc(wpct))

