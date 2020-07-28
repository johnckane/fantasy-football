# Recode lookups

lookup2015 <- read.csv("/home/john/stats_corner/fantasy_football/lookup2015.csv",stringsAsFactors = FALSE,header=FALSE) 
lookup2014 <- read.csv("/home/john/stats_corner/fantasy_football/lookup2014.csv",stringsAsFactors = FALSE,header=FALSE) 
lookup2013 <- read.csv("/home/john/stats_corner/fantasy_football/lookup2013.csv",stringsAsFactors = FALSE,header=FALSE) 
lookup2012 <- read.csv("/home/john/stats_corner/fantasy_football/lookup2012.csv",stringsAsFactors = FALSE,header=FALSE) 
lookup2011 <- read.csv("/home/john/stats_corner/fantasy_football/lookup2011.csv",stringsAsFactors = FALSE,header=FALSE) 
lookup2010 <- read.csv("/home/john/stats_corner/fantasy_football/lookup2010.csv",stringsAsFactors = FALSE,header=FALSE) 

load("/home/john/stats_corner/fantasy_football/coded_draft_data2.Rda")

colnames(lookup2015)
colnames(coded_draft_data2)

colnames(lookup2015) <- c("year","week","player","pos","pts","keeper","obs")

library(tidyverse)
lookup2015_2 <- lookup2015 %>%
  left_join(coded_draft_data2 %>% 
              unite(player_name,first_name,last_name,sep = " ") %>%
              select(year,player,player_name),
            by = c("V3" = "player","V1" = "year"))
  

coded_draft_data2 %>% 
  unite(player_name,first_name,last_name,sep = " ") %>% select(player_name)
