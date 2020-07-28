#' Data is on github...
library(dplyr)
library(RCurl)
library(stringr)
data_url <- getURL("https://raw.githubusercontent.com/johnckane/stats_corner/master/2015/keeper_analysis/draft_data.csv")
draft_data <- read.csv(text = data_url, stringsAsFactors = FALSE)

#' we don't want keepers or coaches
draft_data <- draft_data %>% 
  mutate(pos = str_trim(pos)) %>%
  filter(keeper == 0, pos != "HC", value < 209) %>%
  mutate(pos = ifelse(pos == "D/","D/ST",pos))

draft_data$player <- str_replace(draft_data$player, "[*]","")

draft_data$player <- ifelse(draft_data$player == "Le Veon Bell", "Le'Veon Bell",draft_data$player)

table(draft_data$pos)
#' want to arrange by player, year calculate lagged values as far back as we can

draft_data %>%
  arrange(player, year) %>%
  head()
 
#' Look at all D/ST, see if we need to change those names
#' 
draft_data %>% filter(pos == "D/ST")

draft_data$first_name <-  sapply(str_split(draft_data$player,' '), '[[', 1)      

draft_data <- draft_data %>%
  mutate(player = ifelse(pos == "D/ST",
                         paste(first_name,"D/ST",sep = " "),
                         player))
draft_data %>% filter(pos == "D/ST")
draft_data <-
draft_data %>%
  arrange(player,year) %>%
  group_by(player) %>%
  mutate(lag1 = ifelse(lag(year,1) == year - 1,lag(value,1), NA),
         lag2 = ifelse(lag(year,1) == year - 2,
                             lag(value,1),
                             ifelse(lag(year,2) == year - 2, 
                                    lag(value,2),
                                    NA)),
         lag3 = ifelse(lag(year,1) == year - 3, 
                             lag(value,1),
                             ifelse(lag(year,2) == year - 3,
                                    lag(value,2),
                                    ifelse(lag(year,3) == year - 3,
                                           lag(value,3),
                                           NA))),
         lag4 = ifelse(lag(year,1) == year - 4,
                             lag(value,1),
                             ifelse(lag(year,2) == year - 4,
                                    lag(value,2),
                                    ifelse(lag(year,3) == year - 4,
                                               lag(value,3),
                                               ifelse(lag(year,4) == year -  4,
                                                      lag(value,4),
                                                      NA)))),
        lag5 = ifelse(lag(year,1) == year - 5,
                             lag(value,1),
                             ifelse(lag(year,2) == year - 5,
                                    lag(value,2),
                                    ifelse(lag(year,3) == year - 5,
                                               lag(value,3),
                                               ifelse(lag(year,4) == year -  5,
                                                      lag(value,4),
                                                      ifelse(lag(year,5) == year - 5,
                                                             lag(value,5),
                                                             NA))))))

draft_data$player <- str_trim(draft_data$player)
  
draft_data$player <- ifelse(draft_data$player == "Steve Smith" & draft_data$team %in% c("BAL","CAR"),
                        "Steve Smith Sr.",
                        draft_data$player)

write.csv(draft_data,"/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/draft_data.csv")
         

   
         