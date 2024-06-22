library(tidyverse)
library(rvest)
library(RMySQL)
library(magrittr)
library(pool)
load("/home/john/projects/fantasy-football/data/team_df.Rda")
#link: https://fantasyfootballcalculator.com/adp?format=standard&year=&teams=12&view=graph&pos=all
url_base <- "https://fantasyfootballcalculator.com/adp?format=standard&year="
url_coda <- "&teams=12&view=graph&pos=all"
year <- 2023
dim <- 171

for(j in 1:1){
  
  webpage <- read_html(paste0(url_base,year,url_coda))
  
  adp <- html_nodes(webpage,'td , .adp-player-name a') %>%
    html_text() %>%
    matrix(.,nrow = dim,ncol = 13, byrow = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    `colnames<-`(c("obs","pick","player","player2","pos","team","bye","overall","std_dev","high","low","times_drafted","blank")) %>%
    mutate(year = year) %>%
    select(year,pick,player,pos,team,overall,std_dev,high,low,times_drafted) %>%
    mutate(overall = as.numeric(overall),
           std_dev = as.numeric(std_dev),
           high = as.numeric(high),
           low = as.numeric(low),
           times_drafted = as.numeric(times_drafted))
  
  rm(webpage)
}

adp
adp$first_name <- sapply(str_split(adp$player,' '), '[[',1)
adp$last_name  <- sapply(str_split(adp$player,' '), '[[',2)

save(adp, file = "/home/john/projects/fantasy-football/data/adp-data/adp_2023_raw.Rda")

head(adp)
