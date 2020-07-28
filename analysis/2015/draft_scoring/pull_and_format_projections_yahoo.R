library(httr)
library(dplyr)
library(XML)
library(RCurl)

### yahoo ###

yahoo_df <- data.frame()

yahoo_url_qb <- "http://www.fantasypros.com/nfl/projections/qb.php"
yahoo_url_rb <- "http://www.fantasypros.com/nfl/projections/rb.php"
yahoo_url_wr <- "http://www.fantasypros.com/nfl/projections/wr.php"
yahoo_url_te <- "http://www.fantasypros.com/nfl/projections/te.php"
yahoo_url_k <- "http://www.fantasypros.com/nfl/projections/k.php"
yahoo_url_dst <- "http://www.fantasypros.com/nfl/projections/dst.php"

yahoo_df_qb <- rbind(yahoo_df,data.frame(readHTMLTable(yahoo_url_qb)$data))
yahoo_df_rb<- rbind(yahoo_df,data.frame(readHTMLTable(yahoo_url_rb)$data))
yahoo_df_wr <- rbind(yahoo_df,data.frame(readHTMLTable(yahoo_url_wr)$data))
yahoo_df_te<- rbind(yahoo_df,data.frame(readHTMLTable(yahoo_url_te)$data))
yahoo_df_k <- rbind(yahoo_df,data.frame(readHTMLTable(yahoo_url_k)$data))
yahoo_df_dst <- rbind(yahoo_df,data.frame(readHTMLTable(yahoo_url_dst)$data))

## QBs ##
head(yahoo_df_qb)
yahoo_df_qb2 <- yahoo_df_qb 
yahoo_df_qb2$YDS <- as.numeric(
    str_replace(
        as.character(yahoo_df_qb2$YDS),
        "[,]",
        "")
)
yahoo_df_qb2$TDS <- as.numeric(as.character(yahoo_df_qb2$TDS))
yahoo_df_qb2$INTS <- as.numeric(as.character(yahoo_df_qb2$INTS))
yahoo_df_qb2$YDS.1 <- as.numeric(as.character(yahoo_df_qb2$YDS.1))
yahoo_df_qb2$TDS.1 <- as.numeric(as.character(yahoo_df_qb2$TDS.1))
yahoo_df_qb2$FL <- as.numeric(as.character(yahoo_df_qb2$FL))
yahoo_df_qb2 <- yahoo_df_qb2 %>%
    mutate(points = 0.04 * YDS + 4*TDS -2 * INTS + 0.1*YDS.1 + 6*TDS.1 - 2*FL)
yahoo_df_qb2
## extract player, team, pos
yahoo_df_qb2$pos <- "QB"
yahoo_df_qb2$fn <- lapply(str_split(yahoo_df_qb2$Player," ",3),'[[',1)
yahoo_df_qb2$ln <- lapply(str_split(yahoo_df_qb2$Player," ",3),'[[',2)
yahoo_df_qb2$player <- paste(yahoo_df_qb2$fn,yahoo_df_qb2$ln,sep = " ")
yahoo_df_qb2$team <- str_trim(str_sub(yahoo_df_qb2$Player,start = -3))
yahoo_df_qb2 <- yahoo_df_qb2[,c(16,13,17,12)]
yahoo_df_qb2

## Now RBs ## 
head(yahoo_df_rb)
yahoo_df_rb2 <- yahoo_df_rb 
yahoo_df_rb2$YDS <- as.numeric(
    str_replace(
        as.character(yahoo_df_rb2$YDS),
        "[,]",
        "")
)
yahoo_df_rb2$TDS <- as.numeric(as.character(yahoo_df_rb2$TDS))
yahoo_df_rb2$YDS.1 <- as.numeric(as.character(yahoo_df_rb2$YDS.1))
yahoo_df_rb2$TDS.1 <- as.numeric(as.character(yahoo_df_rb2$TDS.1))
yahoo_df_rb2$FL <- as.numeric(as.character(yahoo_df_rb2$FL))
yahoo_df_rb2 <- yahoo_df_rb2 %>%
    mutate(points = 0.1 * YDS + 6*TDS -2 * FL + 0.1*YDS.1 + 6*TDS.1)
yahoo_df_rb2
## extract player, team, pos
yahoo_df_rb2$pos <- "RB"
yahoo_df_rb2$fn <- lapply(str_split(yahoo_df_rb2$Player," ",3),'[[',1)
yahoo_df_rb2$ln <- lapply(str_split(yahoo_df_rb2$Player," ",3),'[[',2)
yahoo_df_rb2$player <- paste(yahoo_df_rb2$fn,yahoo_df_rb2$ln,sep = " ")
yahoo_df_rb2$team <- str_trim(str_sub(yahoo_df_rb2$Player,start = -3))
yahoo_df_rb2 <- yahoo_df_rb2[,c(14,11,15,10)]
yahoo_df_rb2


## Now WR ##
head(yahoo_df_wr)
yahoo_df_wr2 <- yahoo_df_wr 
yahoo_df_wr2$YDS <- as.numeric(
    str_replace(
        as.character(yahoo_df_wr2$YDS),
        "[,]",
        "")
)
yahoo_df_wr2$YDS.1 <- as.numeric(
    str_replace(
        as.character(yahoo_df_wr2$YDS.1),
        "[,]",
        "")
)
yahoo_df_wr2$TDS <- as.numeric(as.character(yahoo_df_wr2$TDS))
yahoo_df_wr2$TDS.1 <- as.numeric(as.character(yahoo_df_wr2$TDS.1))
yahoo_df_wr2$FL <- as.numeric(as.character(yahoo_df_wr2$FL))
yahoo_df_wr2 <- yahoo_df_wr2 %>%
    mutate(points = 0.1 * YDS + 6*TDS -2 * FL + 0.1*YDS.1 + 6*TDS.1)
yahoo_df_wr2
## extract player, team, pos
yahoo_df_wr2$pos <- "WR"
yahoo_df_wr2$fn <- lapply(str_split(yahoo_df_wr2$Player," ",3),'[[',1)
yahoo_df_wr2$ln <- lapply(str_split(yahoo_df_wr2$Player," ",3),'[[',2)
yahoo_df_wr2$player <- paste(yahoo_df_wr2$fn,yahoo_df_wr2$ln,sep = " ")
yahoo_df_wr2$team <- str_trim(str_sub(yahoo_df_wr2$Player,start = -3))
yahoo_df_wr2 <- yahoo_df_wr2[,c(14,11,15,10)]
yahoo_df_wr2


## Now TE ## 
head(yahoo_df_te)
yahoo_df_te2 <- yahoo_df_te 
yahoo_df_te2$YDS <- as.numeric(
    str_replace(
        as.character(yahoo_df_te2$YDS),
        "[,]",
        "")
)
yahoo_df_te2$TDS <- as.numeric(as.character(yahoo_df_te2$TDS))
yahoo_df_te2$FL <- as.numeric(as.character(yahoo_df_te2$FL))
yahoo_df_te2 <- yahoo_df_te2 %>%
    mutate(points = 0.1 * YDS + 6*TDS -2 * FL)
yahoo_df_te2
## extract player, team, pos
yahoo_df_te2$pos <- "TE"
yahoo_df_te2$fn <- lapply(str_split(yahoo_df_te2$Player," ",3),'[[',1)
yahoo_df_te2$ln <- lapply(str_split(yahoo_df_te2$Player," ",3),'[[',2)
yahoo_df_te2$player <- paste(yahoo_df_te2$fn,yahoo_df_te2$ln,sep = " ")
yahoo_df_te2$team <- str_trim(str_sub(yahoo_df_te2$Player,start = -3))
yahoo_df_te2 <- yahoo_df_te2[,c(11,8,12,7)]
yahoo_df_te2


## Now D/ST ## 
head(yahoo_df_dst)
yahoo_df_dst2 <- yahoo_df_dst
yahoo_df_dst2$points <- as.numeric(as.character(yahoo_df_dst2$FPTS))
## extract player, team, pos
yahoo_df_dst2$pos <- "D/ST"
yahoo_df_dst2$player <- paste(str_trim(str_sub(yahoo_df_dst2$Player,end = -4)),
                              "D/ST",
                              sep = " ")
yahoo_df_dst2$team <- str_trim(str_sub(yahoo_df_dst2$Player,start = -3))
yahoo_df_dst2 <- yahoo_df_dst2[,c(14,13,15,12)]
yahoo_df_dst2


## Now K ##
head(yahoo_df_k)
yahoo_df_k2 <- yahoo_df_k
yahoo_df_k2$points <- as.numeric(as.character(yahoo_df_k2$FPTS))
## extract player, team, pos
yahoo_df_k2$pos <- "K"
yahoo_df_k2$player <- str_sub(yahoo_df_k2$Player,end = -4)
yahoo_df_k2$team <- str_trim(str_sub(yahoo_df_k2$Player,start = -3))
yahoo_df_k2 <- yahoo_df_k2[,c(8,7,9,6)]
yahoo_df_k2
yahoo_df_dst2

### combine all data

yahoo_projections <- bind_rows(yahoo_df_qb2,
                               yahoo_df_rb2,
                               yahoo_df_wr2,
                               yahoo_df_te2,
                               yahoo_df_dst2,
                               yahoo_df_k2)
View(yahoo_projections)


### Now bring in draft data ###
yahoo_projections$team <- ifelse(yahoo_projections$team == 'WAS',
                                 'WSH',
                                 yahoo_projections$team)
yahoo_projections$player <- ifelse(yahoo_projections$player == 'Christopher Ivory',
                                   'Chris Ivory',
                                   yahoo_projections$player)
yahoo_projections$player <- ifelse(yahoo_projections$player == 'Devante Parker',
                                   'DeVante Parker',
                                   yahoo_projections$player)
yahoo_projections$player <- str_trim(yahoo_projections$player)
yahoo_projections_draft <- sqldf('
                               select
                                    a.owner
                               ,    a.player
                               ,    a.pos
                               ,    a.team
                               ,    a.espn_points
                               ,    a.cbs_points
                               ,    b.points as yahoo_points
                               from
                                    cbs_final as a
                               ,    yahoo_projections as b
                               where
                                    a.player = b.player
                               and  a.pos = b.pos
                               and  a.team = b.team')
a <- anti_join(cbs_final,yahoo_projections,by=c("player","pos","team"))

d_merge_yahoo <- inner_join(
    filter(cbs_final, pos == 'D/ST'),
    filter(yahoo_projections, pos == 'D/ST'),
    by = c("team"))
d_merge_yahoo <- d_merge_yahoo[,c(1,2,3,4,5,6,9)]
d_merge_yahoo
colnames(d_merge_yahoo) <- c("owner","player","pos","team","espn_points",
                             "cbs_points","yahoo_points")
final_projections <- bind_rows(yahoo_projections_draft,d_merge_yahoo)
write.csv(final_projections,"final_projections_data.csv",row.names = FALSE)
