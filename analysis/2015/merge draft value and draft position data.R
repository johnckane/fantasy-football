setwd("/home/john/Fantasy Football/2015 Prep")
library(sqldf)
library(stringr)

adp <- read.csv("adp.csv", stringsAsFactors=FALSE, header=TRUE)
draft <- read.csv("draft_summary.csv", stringsAsFactors=FALSE, header=TRUE)
draft$pos <- str_trim(draft$pos)

## looks like we're missing a lot of players. Let's see how many of each position in each dataset
table(adp$pos)
table(draft$pos)

## what is truly important is to have the relative rankings of all the drafted players in our league, non drafted
## players don't matter much
#also let's filter out so we only have QB,RB,WR and TE
pos_to_use = c("QB","RB","WR","TE")

adp <- filter(adp, pos %in% pos_to_use)
draft <- filter(draft, pos %in% pos_to_use)

## Some manuel edits of names to make sure things merge 
draft$player <- ifelse(draft$player == "Le Veon Bell" | draft$player == "Le'Veon Bell",
                       "LeVeon Bell",
                       draft$player)
draft$player <- ifelse(draft$player == "Chad Ochocinco",
                       "Chad Johnson",
                       draft$player)
draft$player <- ifelse(draft$player == "Cadillac Williams",
                       "Carnell Williams",
                       draft$player)
data_leftjoin <- sqldf('
                       select
                         b.year,
                         b.name,
                         b.pos,
                         b.overall,
                         b.pick,
                         a.team,
                         a.value,
                         a.keeper
                       from
                         adp as b
                       left join
                         draft as a
                       on
                         b.year = a.year
                       and b.name = a.player
                       and b.pos = a.pos')

filter(data_leftjoin,is.na(value)==TRUE) %>% arrange(pick)

filter_draft <- function(p,y){
    return(filter(draft,pos==p,year==y) %>% arrange(player))
}

## Now I think what's left are players that weren't drafted, so we'll have to manually set their
## values to 1, team remains 'NA' and keeper = 0.
na_value = which(is.na(data_leftjoin$value) == TRUE)
data_leftjoin$value[na_value] <- 1
data_leftjoin$keeper[na_value] <- 0

## Can only have 797 observations, so we need to remove some duplicates 
## Steve Smith
data_leftjoin <- data_leftjoin[-c(which(data_leftjoin$name=="Steve Smith" & data_leftjoin$team == "Car" & data_leftjoin$overall == 34.2 & data_leftjoin$year == 2010),
                                  which(data_leftjoin$name=="Steve Smith" & data_leftjoin$team == "NYG" & data_leftjoin$overall == 36.6 & data_leftjoin$year == 2010)),]
## Mike Williams
data_leftjoin <- data_leftjoin[-which(data_leftjoin$name=="Mike Williams" & data_leftjoin$team == "Sea" & data_leftjoin$year    == 2011),]

data_leftjoin$pick<- floor(data_leftjoin$pick)

write.csv(data_leftjoin,"adp_draft.csv")
