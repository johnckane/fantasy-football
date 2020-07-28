

adp_10_14 <- read.csv("/home/john/Fantasy Football/2015 Prep/adp.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

adp_15 <- read.csv("/home/john/Fantasy Football/2015 Prep/data15.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE)
adp_15$year <- 2015

library(dplyr)
adp_15 <- adp_15 %>% select(year,name,pos,team,overall,pick)


adp <- bind_rows(adp_10_14,adp_15)
adp2 <- adp %>%
    group_by(year,pos) %>%
    mutate(pick = rank(overall))
write.csv(adp2,"adp_data.csv",row.names = FALSE)
