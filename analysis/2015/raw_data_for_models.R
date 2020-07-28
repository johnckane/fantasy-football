setwd("/home/john/Fantasy Football/2015 Prep")
library(sqldf)
library(dplyr)

adp_draft <- read.csv("adp_draft.csv", stringsAsFactors = FALSE, header = TRUE)
player_data_adp <- read.csv("player_data_adp.csv", stringsAsFactors = FALSE, header = TRUE)

adp_draft <- filter(adp_draft,keeper==0)
adp_draft <- filter(adp_draft,value < 200)

## merge adp with draft cost and fantasy production
summary <- sqldf('
                 select
                 a.year,
                 a.pos,
                 a.pick,
                 a.name,
                 a.points,
                 b.value,
                 b.keeper
                 from
                 player_data_adp as a
                 left join
                 adp_draft as b
                 on
                 a.year = b.year
                 and a.pick = b.pick
                 and a.pos = b.pos
                 and a.name = b.name')
filter(summary,is.na(value))
adp_draft$name <- ifelse(adp_draft$name == "Beanie Wells",
                         "Chris Wells",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "Mike Sims-Walker",
                         "Mike Walker",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "Robert Griffin III",
                         "Robert Griffin",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "EJ Manuel",
                         "E.J. Manuel",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "T.Y. Hilton",
                         "Ty Hilton",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "LeVeon Bell",
                         "Le'Veon Bell",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "Joshua Cribbs",
                         "Josh Cribbs",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "Carnell Williams",
                         "Cadillac Williams",
                         adp_draft$name)

## Try it again
summary2 <- sqldf('
                 select
                    a.year,
                 a.pos,
                 a.pick,
                 a.name,
                 a.points,
                 b.value,
                 b.keeper
                 from
                 player_data_adp as a
                 left join
                 adp_draft as b
                 on
                 a.year = b.year
                 and a.pick = b.pick
                 and a.pos = b.pos
                 and a.name = b.name')
filter(summary2,is.na(value))
## remove keepers, the draft value isn't reflective of an open-market
summary_points <- summary2


#### Now that we have all the data, generate summary statistics by pick. Then merge those 
#### with the values with the fitted values.

point_summary <- summary_points %>%
    group_by(pos,pick) %>%
    arrange(year) %>%
    summarise(n_points = n(),
              mean_points = round(mean(points,na.rm=TRUE),1),
              var_points = round(var(points,na.rm=TRUE),1),
              range_points = paste(round(min(points,na.rm=TRUE),1),", ",round(max(points,na.rm=TRUE),1),sep=""),
              last_points = last(points))

cost_summary <- adp_draft %>%
    group_by(pos,pick) %>%
    arrange(year) %>%
    summarise(n_cost = n(),
              mean_cost = round(mean(value),1),
              var_cost = round(var(value),1),
              range_cost = paste(round(min(value),1),", ",round(max(value),1),sep=""),
              last_cost = last(value))

point_cost <- inner_join(point_summary,cost_summary,by=c("pos","pick"))

write.csv(point_cost,"raw_data_for_models.csv")
