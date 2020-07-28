setwd("/home/john/stats_corner/2015/keeper_analysis/")

draft <- read.csv("draft_adjusted.csv", header = TRUE, stringsAsFactors = FALSE)
draft <- draft %>%
    filter(keeper == 0) %>%
    select(year,player,pos,value)
library(stringr)
draft$pos <- str_trim(draft$pos)

adp <- read.csv("adp_data.csv", header = TRUE, stringsAsFactors = FALSE)
adp$player <- adp$name
adp <- adp %>%
    select(year,player,pos,pick)

str(draft)
str(adp)

draft$player <- ifelse(draft$player == 'Arian Foster*',
                       'Arian Foster',
                       draft$player)
draft$player <- ifelse(draft$player == 'Chad Ochocinco',
                       'Chad Johnson',
                       draft$player)
draft$player <- ifelse(draft$player == 'Todd Gurley*',
                       'Todd Gurley',
                       draft$player)
draft$player <- ifelse(draft$player == 'T.J. Yeldon',
                       'TJ Yeldon',
                       draft$player)

draft_adp <- inner_join(draft,adp, by = c("year","player","pos"))

anti_join(draft,adp, by = c("year","player","pos")) %>%
    filter(pos %in% c("QB","RB","WR","TE")) %>%
    arrange(player,year)

anti_join(adp,draft, by = c("year","player","pos")) %>%
    filter(pos %in% c("QB","RB","WR","TE")) %>%
    arrange(player,year)

write.csv(draft_adp,"draft_adp.csv",row.names = FALSE)
