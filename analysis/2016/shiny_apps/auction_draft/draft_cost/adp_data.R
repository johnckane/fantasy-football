#' Data is on github...
library(dplyr)
library(RCurl)
library(stringr)
data_url <- getURL("https://raw.githubusercontent.com/johnckane/stats_corner/master/2015/keeper_analysis/adp_data.csv")
adp_data <- read.csv(text = data_url, stringsAsFactors = FALSE)

head(adp_data)

#' want to arrange by player, calculate lags

adp_data <- adp_data %>%
  arrange(name,year) %>%
  group_by(name) %>%
  mutate(lag1_pos_rank = ifelse(lag(year,1) == year - 1,lag(pick,1), NA),
         lag2_pos_rank = ifelse(lag(year,1) == year - 2,
                       lag(pick,1),
                       ifelse(lag(year,2) == year - 2, 
                              lag(pick,2),
                              NA)),
         lag3_pos_rank = ifelse(lag(year,1) == year - 3, 
                       lag(pick,1),
                       ifelse(lag(year,2) == year - 3,
                              lag(pick,2),
                              ifelse(lag(year,3) == year - 3,
                                     lag(pick,3),
                                     NA))),
         lag4_pos_rank = ifelse(lag(year,1) == year - 4,
                       lag(pick,1),
                       ifelse(lag(year,2) == year - 4,
                              lag(pick,2),
                              ifelse(lag(year,3) == year - 4,
                                     lag(pick,3),
                                     ifelse(lag(year,4) == year -  4,
                                            lag(pick,4),
                                            NA)))),
         lag5_pos_rank = ifelse(lag(year,1) == year - 5,
                       lag(pick,1),
                       ifelse(lag(year,2) == year - 5,
                              lag(pick,2),
                              ifelse(lag(year,3) == year - 5,
                                     lag(pick,3),
                                     ifelse(lag(year,4) == year -  5,
                                            lag(pick,4),
                                            ifelse(lag(year,5) == year - 5,
                                                   lag(pick,5),
                                                   NA)))))) %>%
  mutate(lag1_overall = ifelse(lag(year,1) == year - 1,lag(overall,1), NA),
         lag2_overall = ifelse(lag(year,1) == year - 2,
                                lag(overall,1),
                                ifelse(lag(year,2) == year - 2, 
                                       lag(overall,2),
                                       NA)),
         lag3_overall = ifelse(lag(year,1) == year - 3, 
                                lag(overall,1),
                                ifelse(lag(year,2) == year - 3,
                                       lag(overall,2),
                                       ifelse(lag(year,3) == year - 3,
                                              lag(overall,3),
                                              NA))),
         lag4_overall = ifelse(lag(year,1) == year - 4,
                                lag(overall,1),
                                ifelse(lag(year,2) == year - 4,
                                       lag(overall,2),
                                       ifelse(lag(year,3) == year - 4,
                                              lag(overall,3),
                                              ifelse(lag(year,4) == year -  4,
                                                     lag(overall,4),
                                                     NA)))),
         lag5_overall = ifelse(lag(year,1) == year - 5,
                                lag(overall,1),
                                ifelse(lag(year,2) == year - 5,
                                       lag(overall,2),
                                       ifelse(lag(year,3) == year - 5,
                                              lag(overall,3),
                                              ifelse(lag(year,4) == year -  5,
                                                     lag(overall,4),
                                                     ifelse(lag(year,5) == year - 5,
                                                            lag(overall,5),
                                                            NA))))))
  
  
adp_data$name <- str_trim(adp_data$name)

adp_data$name <- ifelse(adp_data$name == "Steve Smith" & adp_data$team == "BAL",
                        "Steve Smith Sr.",
                        adp_data$name)
write.csv(adp_data,"/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/adp_data.csv")
