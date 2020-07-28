library(httr)
library(dplyr)
library(XML)
library(RCurl)

### cbssports.com ###


cbs_url_qb <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/QB/season?&print_rows=9999"
cbs_qb <- data.frame(readHTMLTable(cbs_url_qb)[[4]])

cbs_url_rb <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/season/avg/standard?&print_rows=9999"
cbs_rb <- data.frame(readHTMLTable(cbs_url_rb)[[4]]) 

cbs_url_wr <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/season/avg/standard?&print_rows=9999"
cbs_wr <- data.frame(readHTMLTable(cbs_url_wr)[[4]])

cbs_url_te <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/TE/season/avg/standard?&print_rows=9999"
cbs_te <- data.frame(readHTMLTable(cbs_url_te)[[4]])

cbs_url_k <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/K/season/avg/standard"
cbs_k <- data.frame(readHTMLTable(cbs_url_k)[[4]])

cbs_url_dst <-"http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/DST/season/avg/standard"
cbs_dst <- data.frame(readHTMLTable(cbs_url_dst)[[4]])

head(cbs_qb)
cbs_qb2 <- cbs_qb[-c(1:6),]
cbs_qb3 <- cbs_qb2  
cbs_qb3$V4  <- as.numeric(as.character(cbs_qb3$V4))
cbs_qb3$V5  <- as.numeric(as.character(cbs_qb3$V5))
cbs_qb3$V6  <- as.numeric(as.character(cbs_qb3$V6))
cbs_qb3$V10 <- as.numeric(as.character(cbs_qb3$V10))
cbs_qb3$V12 <- as.numeric(as.character(cbs_qb3$V12))
cbs_qb3$V13 <- as.numeric(as.character(cbs_qb3$V13))

colnames(cbs_qb3)
cbs_qb4 <- cbs_qb3 %>%
    mutate(points = 0.04*V4 + 4*V5 -2*V6 +0.1*V10 + 6*V12 -2*V13)
cbs_qb4
## now fix the names ##
cbs_qb4$player <- str_sub(cbs_qb4$V1,end = str_locate(cbs_qb4$V1,",")[,1] - 1)
cbs_qb4$pos <- "QB"
cbs_qb4$team <- str_sub(cbs_qb4$V1, start = str_locate(cbs_qb4$V1,"Â")[,1] + 1)
cbs_qb5 <- cbs_qb4[,c(9,10,11,8)]
cbs_qb5

## Now RBs
head(cbs_rb)
cbs_rb2 <- cbs_rb[-c(1:6),]
cbs_rb3 <- cbs_rb2  
cbs_rb3$V3  <- as.numeric(as.character(cbs_rb3$V3))
cbs_rb3$V5  <- as.numeric(as.character(cbs_rb3$V5))
cbs_rb3$V7  <- as.numeric(as.character(cbs_rb3$V7))
cbs_rb3$V9  <- as.numeric(as.character(cbs_rb3$V9))
cbs_rb3$V10 <- as.numeric(as.character(cbs_rb3$V10))
colnames(cbs_rb3)
cbs_rb4 <- cbs_rb3 %>%
    mutate(points = 0.1*V3 + 6*V5 + 0.1*V7 + 6*V9 -2*V10)
cbs_rb4
## now fix the names ##
cbs_rb4$player <- str_sub(cbs_rb4$V1,end = str_locate(cbs_rb4$V1,",")[,1] - 1)
cbs_rb4$pos <- "RB"
cbs_rb4$team <- str_sub(cbs_rb4$V1, start = str_locate(cbs_rb4$V1,"Â")[,1] + 1)
cbs_rb5 <- cbs_rb4[,c(13,14,15,12)]
cbs_rb5

## Now WRs
head(cbs_wr)
cbs_wr2 <- cbs_wr[-c(1:6),]
cbs_wr3 <- cbs_wr2  
cbs_wr3$V3  <- as.numeric(as.character(cbs_wr3$V3))
cbs_wr3$V5  <- as.numeric(as.character(cbs_wr3$V5))
cbs_wr3$V6  <- as.numeric(as.character(cbs_wr3$V6))
colnames(cbs_wr3)
cbs_wr4 <- cbs_wr3 %>%
    mutate(points = 0.1*V3 + 6*V5 + -2*V6)
cbs_wr4
## now fix the names ##
cbs_wr4$player <- str_sub(cbs_wr4$V1,end = str_locate(cbs_wr4$V1,",")[,1] - 1)
cbs_wr4$pos <- "WR"
cbs_wr4$team <- str_sub(cbs_wr4$V1, start = str_locate(cbs_wr4$V1,"Â")[,1] + 1)
cbs_wr5 <- cbs_wr4[,c(9,10,11,8)]
cbs_wr5

## Now TE
head(cbs_te)
cbs_te2 <- cbs_te[-c(1:6),]
cbs_te3 <- cbs_te2  
cbs_te3$V3  <- as.numeric(as.character(cbs_te3$V3))
cbs_te3$V5  <- as.numeric(as.character(cbs_te3$V5))
cbs_te3$V6  <- as.numeric(as.character(cbs_te3$V6))
colnames(cbs_te3)
cbs_te4 <- cbs_te3 %>%
    mutate(points = 0.1*V3 + 6*V5 + -2*V6)
cbs_te4
## now fix the names ##
cbs_te4$player <- str_sub(cbs_te4$V1,end = str_locate(cbs_te4$V1,",")[,1] - 1)
cbs_te4$pos <- "TE"
cbs_te4$team <- str_sub(cbs_te4$V1, start = str_locate(cbs_te4$V1,"Â")[,1] + 1)
cbs_te5 <- cbs_te4[,c(9,10,11,8)]
cbs_te5

### Now D/ST
head(cbs_dst)
cbs_dst2 <- cbs_dst[-c(1:5),]
cbs_dst3 <- cbs_dst2  
cbs_dst3$points  <- as.numeric(as.character(cbs_dst3$V10))
cbs_dst4 <- select(cbs_dst3, V1, points)
## now fix the names ##
cbs_dst4$player <- str_sub(cbs_dst4$V1,end = str_locate(cbs_dst4$V1,",")[,1] - 1)
cbs_dst4$pos <- "D/ST"
cbs_dst4$team <- str_sub(cbs_dst4$V1, start = str_locate(cbs_dst4$V1,"Â")[,1] + 1)
cbs_dst4$player <- paste0(cbs_dst4$player," ","D/ST")
cbs_dst5 <- cbs_dst4[,c(3,4,5,2)]
cbs_dst5


## Now kickers ##
head(cbs_k)
cbs_k2 <- cbs_k[-c(1:5),]
cbs_k3 <- cbs_k2  
cbs_k3$points  <- as.numeric(as.character(cbs_k3$V5))
cbs_k4 <- select(cbs_k3, V1, points)
## now fix the names ##
cbs_k4$player <- str_sub(cbs_k4$V1,end = str_locate(cbs_k4$V1,",")[,1] - 1)
cbs_k4$pos <- "K"
cbs_k4$team <- str_sub(cbs_k4$V1, start = str_locate(cbs_k4$V1,"Â")[,1] + 1)
cbs_k5 <- cbs_k4[,c(3,4,5,2)]
cbs_k5


#### Now put them all together
cbs_projections <- bind_rows(cbs_qb5,
                             cbs_rb5,
                             cbs_wr5,
                             cbs_te5,
                             cbs_dst5,
                             cbs_k5)
## Now fold in draft data
str(cbs_projections)
espn_df6$team <- toupper(espn_df6$team)
str(espn_df6)
cbs_projections$team <- str_trim(cbs_projections$team)
espn_df6$player <- str_replace(espn_df6$player,"[*]"," ")
cbs_projections$team <- ifelse(cbs_projections$team == "WAS",
                               "WSH",
                               cbs_projections$team)
cbs_projections$player <- str_trim(cbs_projections$player)
espn_df6$player <- str_trim(espn_df6$player)
espn_df6$player <- ifelse(espn_df6$player == "Odell Beckham Jr.",
                          "Odell Beckham",
                          espn_df6$player)
espn_df6$player <- ifelse(espn_df6$player == "Steve Smith Sr.",
                          "Steve Smith",
                          espn_df6$player)
library(ifultools)
espn_df6$team <- ifelse(espn_df6$pos == 'D/ST',
                        properCase(espn_df6$team),
                        espn_df6$team)

cbs_projections_draft <- sqldf('
                               select
                                    a.owner
                               ,    a.player
                               ,    a.pos
                               ,    a.team
                               ,    a.points as espn_points
                               ,    b.points as cbs_points
                               from
                                    espn_df6 as a
                               ,    cbs_projections as b
                               where
                                    a.player = b.player
                               and  a.pos = b.pos
                               and  a.team = b.team')
a <- anti_join(espn_df6,cbs_projections,by=c("player","pos","team"))

# join just the defenses
d_merge_cbs <- inner_join(
    filter(espn_df6, pos == 'D/ST'),
    filter(cbs_projections, pos == 'D/ST'),
    by = c("player"))
d_merge_cbs <- d_merge_cbs[,c(1,2,3,7,5,8)]
d_merge_cbs
colnames(d_merge_cbs) <- c("owner","player","pos","team","espn_points","cbs_points")
d_merge_cbs

cbs_final <- bind_rows(cbs_projections_draft, d_merge_cbs)

write.csv(cbs_final,
          "cbs_espn_draft_projections.csv",
          row.names = FALSE)
