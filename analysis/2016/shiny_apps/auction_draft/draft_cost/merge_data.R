#' Read in the data

adp_data   <- read.csv("/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/adp_data.csv",
                     stringsAsFactors = FALSE,
                     header = TRUE)
adp_data$pos <- ifelse(adp_data$pos == "DEF","D/ST",
                       ifelse(adp_data$pos == "PK",
                              "K",
                              adp_data$pos))
adp_data$name <- str_replace(adp_data$name, "Defense","D/ST")
adp_data$name <- str_replace(adp_data$name, "Arizona","Cardinals")
adp_data$name <- str_replace(adp_data$name, "Atlanta","Falcons") 
adp_data$name <- str_replace(adp_data$name, "Baltimore","Ravens")
adp_data$name <- str_replace(adp_data$name, "Buffalo","Bills")
adp_data$name <- str_replace(adp_data$name, "Carolina","Panthers")
adp_data$name <- str_replace(adp_data$name, "Chicago","Bears")
adp_data$name <- str_replace(adp_data$name, "Cincinnati","Bengals")
adp_data$name <- str_replace(adp_data$name, "Cleveland","Browns")
adp_data$name <- str_replace(adp_data$name, "Dallas","Cowboys")
adp_data$name <- str_replace(adp_data$name, "Denver","Broncos")
adp_data$name <- str_replace(adp_data$name, "Detroit","Lions")
adp_data$name <- str_replace(adp_data$name, "Green Bay","Packers")
adp_data$name <- str_replace(adp_data$name, "Houston","Texans")
adp_data$name <- str_replace(adp_data$name, "Indianapolis","Colts")
adp_data$name <- str_replace(adp_data$name, "Jacksonville","Jaguars") 
adp_data$name <- str_replace(adp_data$name, "Kansas City","Chiefs")
adp_data$name <- str_replace(adp_data$name, "Miami","Dolphins")
adp_data$name <- str_replace(adp_data$name, "Minnesota","Vikings")
adp_data$name <- str_replace(adp_data$name, "New England","Patriots")
adp_data$name <- str_replace(adp_data$name, "New Orleans","Saints")
adp_data$name <- str_replace(adp_data$name, "NY Giants","Giants")
adp_data$name <- str_replace(adp_data$name, "NY Jets","Jets")
adp_data$name <- str_replace(adp_data$name, "Oakland","Raiders")
adp_data$name <- str_replace(adp_data$name, "Philadelphia","Eagles")
adp_data$name <- str_replace(adp_data$name, "Pittsburgh","Steelers")
adp_data$name <- str_replace(adp_data$name, "San Diego","Chargers")
adp_data$name <- str_replace(adp_data$name, "San Francisco","49ers")
adp_data$name <- str_replace(adp_data$name, "Seattle","Seahawks")
adp_data$name <- str_replace(adp_data$name, "St. Louis","Rams")
adp_data$name <- str_replace(adp_data$name, "Los Angeles","Rams")
adp_data$name <- str_replace(adp_data$name, "Tampa Bay","Buccaneers")
adp_data$name <- str_replace(adp_data$name, "Tennessee","Titans")
adp_data$name <- ifelse(adp_data$pos == "D/ST",
                        str_replace(adp_data$name, "Washington","Redskins"),
                        adp_data$name)
                        
adp_data$name <- ifelse(adp_data$name == "Carnell Williams", "Cadillac Williams",adp_data$name)
adp_data$name <- ifelse(adp_data$name == "LeVeon Bell", "Le'Veon Bell", adp_data$name)
adp_data$name <- ifelse(adp_data$name == "TJ Yeldon", "T.J. Yeldon", adp_data$name)
adp_data$name <- ifelse(adp_data$year %in% c(2010,2011) & adp_data$name == "Chad Johnson",
                        "Chad Ochocinco",
                        adp_data$name)

adp_data$name <- str_trim(adp_data$name)

draft_data <- read.csv("/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/draft_data.csv",
                       stringsAsFactors = FALSE,
                       header = TRUE)

#' Get rid of that Andre Johnson pick
draft_data <- draft_data %>% filter(value < 209)

table(adp_data$team)
table(draft_data$team)
draft_data$team <- toupper(draft_data$team)
draft_data$player <- str_trim(draft_data$player)

aj1 <- anti_join(draft_data,adp_data %>% mutate(player = name) , by = c("year","player","pos"))

y <- 2010
p <- "D/ST"
filter(aj1, pos == p) %>% arrange(year, player) %>% filter(year == y)
filter(adp_data, pos == p, year == y) %>% arrange(name)

lj1 <- left_join(draft_data, adp_data %>% mutate(player = name), by = c("year","player","pos"))
cbind(colnames(lj1))
head(lj1)
lj2 <- lj1 %>% select(3,4,5,6,7,8,9,11:15,20,21,22:31)
colnames(lj2)
colnames(lj2) <- c("year","owner","player","team","pos","value","keeper","lag1_price","lag2_price","lag3_price","lag4_price",
                   "lag5_price","overall_rank","pos_rank","lag1_pos_rank","lag2_pos_rank","lag3_pos_rank","lag4_pos_rank",
                   "lag5_pos_rank","lag1_overall","lag2_overall","lag3_overall","lag4_overall","lag5_overall")

filter(lj2, player == "Andre Johnson")

write.csv(lj2, "/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/adp_draft_data.csv",
          row.names = FALSE)
