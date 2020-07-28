library(httr)
library(XML)
library(RCurl)

url16 <- "https://fantasyfootballcalculator.com/adp.php?format=2qb&year=2016&teams=12&view=graph&pos=all"

#url15 <- "http://fantasyfootballcalculator.com/adp.php?format=standard&year=2015&teams=12&view=graph&pos=all"

data16 <- GET(url16)
data16 <- readHTMLTable(rawToChar(data16$content), stringsAsFactors = F)
data16 <- as.data.frame(data16)
colnames(data16) <- c('obs',
                      'pick',
                      'name',
                      'pos',
                      'team',
                      'bye',
                      'overall',
                      'std_dev',
                      'high',
                      'low',
                      'times',
                      'graph')
# make some of the edits we did for 2010 - 2015
data16$pos <- ifelse(data16$pos == "DEF","D/ST",
                       ifelse(data16$pos == "PK",
                              "K",
                              data16$pos))
data16$name <- str_replace(data16$name, "Defense","D/ST")
data16$name <- str_replace(data16$name, "Arizona","Cardinals")
data16$name <- str_replace(data16$name, "Atlanta","Falcons") 
data16$name <- str_replace(data16$name, "Baltimore","Ravens")
data16$name <- str_replace(data16$name, "Buffalo","Bills")
data16$name <- str_replace(data16$name, "Carolina","Panthers")
data16$name <- str_replace(data16$name, "Chicago","Bears")
data16$name <- str_replace(data16$name, "Cincinnati","Bengals")
data16$name <- str_replace(data16$name, "Cleveland","Browns")
data16$name <- str_replace(data16$name, "Dallas","Cowboys")
data16$name <- str_replace(data16$name, "Denver","Broncos")
data16$name <- str_replace(data16$name, "Detroit","Lions")
data16$name <- str_replace(data16$name, "Green Bay","Packers")
data16$name <- str_replace(data16$name, "Houston","Texans")
data16$name <- str_replace(data16$name, "Indianapolis","Colts")
data16$name <- str_replace(data16$name, "Jacksonville","Jaguars") 
data16$name <- str_replace(data16$name, "Kansas City","Chiefs")
data16$name <- str_replace(data16$name, "Miami","Dolphins")
data16$name <- str_replace(data16$name, "Minnesota","Vikings")
data16$name <- str_replace(data16$name, "New England","Patriots")
data16$name <- str_replace(data16$name, "New Orleans","Saints")
data16$name <- str_replace(data16$name, "NY Giants","Giants")
data16$name <- str_replace(data16$name, "NY Jets","Jets")
data16$name <- str_replace(data16$name, "Oakland","Raiders")
data16$name <- str_replace(data16$name, "Philadelphia","Eagles")
data16$name <- str_replace(data16$name, "Pittsburgh","Steelers")
data16$name <- str_replace(data16$name, "San Diego","Chargers")
data16$name <- str_replace(data16$name, "San Francisco","49ers")
data16$name <- str_replace(data16$name, "Seattle","Seahawks")
data16$name <- str_replace(data16$name, "St. Louis","Rams")
data16$name <- str_replace(data16$name, "Los Angeles","Rams")
data16$name <- str_replace(data16$name, "Tampa Bay","Buccaneers")
data16$name <- str_replace(data16$name, "Tennessee","Titans")
data16$name <- ifelse(data16$pos == "D/ST",
                        str_replace(data16$name, "Washington","Redskins"),
                        data16$name)

data16$name <- ifelse(data16$name == "Carnell Williams", "Cadillac Williams",data16$name)
data16$name <- ifelse(data16$name == "LeVeon Bell", "Le'Veon Bell", data16$name)
data16$name <- ifelse(data16$name == "TJ Yeldon", "T.J. Yeldon", data16$name)
data16$name <- ifelse(data16$year %in% c(2010,2011) & data16$name == "Chad Johnson",
                        "Chad Ochocinco",
                        data16$name)

data16$name <- str_trim(data16$name)

data16$year <- 2016

write.csv(data16, "/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/adp2016.csv",row.names = F)

## Now bring in 2015 and before data to get lags (up to 5 years if available) ##

data2010_2015 <- read.csv("/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/adp_data.csv",
                          stringsAsFactors = F)

head(data2010_2015)

prev_data <- data2010_2015 %>% select(name,pos,overall,pick,year)
str(prev_data)
head(data16)
data16 <- data16 %>% select(name,pos,overall,pick,year)
str(data16)
data16$overall <- as.numeric(data16$overall)
data16$pick <- as.numeric(data16$pick)
all_adp_data <- bind_rows(prev_data, data16)
all_adp_data$player <- all_adp_data$name

library(dplyr)
all_adp_data %>%
  arrange(player,year)

# limit to the positions we're interested in

all_adp_data <- all_adp_data %>%
  arrange(player,year) %>%
  filter(pos %in% c("QB","RB","WR","TE"))

all_adp_data$player <- ifelse(all_adp_data$player == "Steve Smith" & all_adp_data$year == 2016,
                              "Steve Smith Sr.",
                              all_adp_data$player)

adp2016 <- all_adp_data %>%
  group_by(player) %>%
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
                                                           NA)))))) %>%
  filter(year == 2016)

table(data16$pos)
29 + 62+19+68

write.csv(adp2016,"/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/adp2016.csv",row.names = F)

# Now we need the draft data (to get draft price) with the lags ...
draft_data <- read.csv("/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/draft_data.csv", 
                       stringsAsFactors = F)
head(draft_data)
draft_data <- draft_data %>%
  filter(pos %in% c("QB","RB","WR","TE")) %>%
  select(player,year,pos,value)

# for each player, create a 2016 year
player_only <- draft_data %>%
  group_by(player,pos) %>%
  slice(1) %>%
  select(player,pos)
player_only$year <- 2016
player_only$value <- NA

full_draft_data <- bind_rows(draft_data,player_only)

full_draft_data <- full_draft_data %>%
  arrange(player,year) %>%
  group_by(player) %>%
  mutate(lag1_value = ifelse(lag(year,1) == year - 1,lag(value,1), NA),
         lag2_value = ifelse(lag(year,1) == year - 2,
                       lag(value,1),
                       ifelse(lag(year,2) == year - 2, 
                              lag(value,2),
                              NA)),
         lag3_value = ifelse(lag(year,1) == year - 3, 
                       lag(value,1),
                       ifelse(lag(year,2) == year - 3,
                              lag(value,2),
                              ifelse(lag(year,3) == year - 3,
                                     lag(value,3),
                                     NA))),
         lag4_value = ifelse(lag(year,1) == year - 4,
                       lag(value,1),
                       ifelse(lag(year,2) == year - 4,
                              lag(value,2),
                              ifelse(lag(year,3) == year - 4,
                                     lag(value,3),
                                     ifelse(lag(year,4) == year -  4,
                                            lag(value,4),
                                            NA)))),
         lag5_value = ifelse(lag(year,1) == year - 5,
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
draft_data_2016 <- full_draft_data %>%
  filter(year == 2016)

write.csv(draft_data_2016,"/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/draft_data_2016.csv")

## Now merge draft data with adp_data

colnames(adp2016)
colnames(draft_data_2016)
adp2016$player <- ifelse(adp2016$player == "CJ Anderson",
                       "C.J. Anderson",
                       adp2016$player)
adp2016$player <- str_trim(adp2016$player)
aj1 <- anti_join(adp2016,draft_data_2016, by = c("player","pos"))
aj2 <- anti_join(draft_data_2016, adp2016, by = c("player","pos"))

filter(ungroup(aj1), pos == "TE") %>% select(player, pos) %>% arrange(player) %>% print(n = 28)
filter(ungroup(aj2), pos == "TE") %>% select(player,pos) %>% arrange(player) %>% print(n = 94)

fj1 <- full_join(adp2016,draft_data_2016, by = c("player","pos"))
colnames(fj1)
fj2 <- fj1 %>% ungroup() %>% select(6,2,3,4,7:16,19:23)

draft_adp_final_2016 <- fj2


colnames(draft_adp_final_2016)
draft_adp_final_2016 <- 
  draft_adp_final_2016 %>%
  rename(lag1_price = lag1_value,
         lag2_price = lag2_value,
         lag3_price = lag3_value,
         lag4_price = lag4_value,
         lag5_price = lag5_value,
         overall_rank = overall,
         pos_rank = pick)
pred_cost <- predict(final_model, newdata = draft_adp_final_2016)
draft_adp_final_2016$pred_cost <- pred_cost
View(draft_adp_final_2016)
which(is.na(draft_adp_final_2016$pred_cost))

write.csv(draft_adp_final_2016, "/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/draft_adp_final_2016.csv",
          row.names = F)
table(draft_adp_final_2016$pred_cost)



### Now bring in projection data

projection_data <- read.csv("/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/FFA-CustomRankings_standard.csv",
                            stringsAsFactors = F)

## need to split the projection data
colnames(projection_data)
pd2 <- projection_data %>%
  select(3,4,9) %>%
  filter(position %in% c("QB","RB","WR","TE")) %>%
  mutate(ppg = points/13) %>%
  rename(player = playername,
         pos = position) %>%
  select(-points)

filter(pd2, pos == "WR") %>% arrange(player)
pd2$player <- ifelse(pd2$player == "C.J. Prosise",
                     "CJ Prosise",
                     pd2$player)
pd2$player <- ifelse(pd2$player == "LeVeon Bell",
                     "Le'Veon Bell",
                     pd2$player)
pd2$player <- ifelse(pd2$player == "Odell Beckham Jr ",
                     "Odell Beckham Jr",
                     pd2$player)
pd2$player <- ifelse(pd2$player == "Steve Smith",
                     "Steve Smith Sr.",
                     pd2$player)
pd2$player <- ifelse(pd2$player == "Robert Griffin",
                     "Robert Griffin III",
                     pd2$player)   
pd2$player <- ifelse(pd2$player == "Ted Ginn",
                     "Ted Ginn Jr",
                     pd2$player)   
# join projections with cost, left join, don't want projections for people we don't have costs for

full_data <- left_join(draft_adp_final_2016,pd2, by = c("player","pos")) %>% arrange(overall_rank)

# All done!

write.csv(full_data, "/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/final_data.csv")

