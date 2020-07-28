setwd("/home/john/stats_corner/2015/keeper_analysis/")

draft <- read.csv("draft_adjusted.csv", header = TRUE, stringsAsFactors = FALSE)
draft <- draft %>% select(owner,year,player,pos,adj_value,keeper)
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

### Bring in fitted values ###
values <- read.csv("fitted_values.csv", header = TRUE, stringsAsFactors = FALSE)

draft_adp_value <- inner_join(draft_adp,values,by=c("pos","pick"))

keepers <- filter(draft_adp_value, keeper == 1) %>%
    mutate(savings = adj_value - est_cost)
write.csv(keepers,"keepers.csv",row.names = FALSE)
