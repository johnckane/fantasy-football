library(httr)
library(dplyr)
library(XML)
library(RCurl)
library(stringr)


### ESPN ###
url_base <- "http://games.espn.go.com/ffl/tools/projections?&startIndex="
url_end <- "&leagueId=725667"

espn_df <- data.frame()

for(i in c(0,40,80,120,160,200,240,280,320)) {
    url <- paste(url_base,i,url_end,sep="")
    espn_df <- rbind(espn_df,data.frame(readHTMLTable(url)$playertable_0,
                                        stringsAsFactors = FALSE))             
}

#doing this after the draft loads in all the players for me!
head(espn_df)
espn_df2 <- espn_df %>%
    select(V2,V3,V15)
head(espn_df2)
espn_df3 <- espn_df2 %>% filter(str_trim(V2) != "PLAYER, TEAM POS")
head(espn_df3)

# first the easy one, get projected points
espn_df3$points <- as.numeric(as.character(espn_df3$V15))

# now pull off player
espn_df3$player <- sapply(str_split(espn_df3$V2,","),'[',1)
espn_df3$team_position <- str_trim(sapply(str_split(espn_df3$V2,","),'[',2))
espn_df3$team <- str_trim(str_sub(espn_df3$team_position,end = 3))
espn_df3$pos <- str_trim(str_sub(espn_df3$team_position,start = 4, end = 6))
                  
espn_df3[which(is.na(espn_df3$team_position)),]$pos <- "D/ST"
espn_df3[which(is.na(espn_df3$team_position)),]$player <-
    str_replace(espn_df3[which(is.na(espn_df3$team_position)),]$player, "D/ST", "")
espn_df3[which(is.na(espn_df3$team_position)),]$team <- 
    str_trim(
        str_sub(espn_df3$player[which(is.na(espn_df3$team_position))],
                end = sapply(str_locate(espn_df3$player,"D/ST"),'[',1)[which(is.na(espn_df3$team_position))] - 1
            ))
#success!

espn_df4 <- espn_df3 %>%
    select(player,pos,team,points)

## now merge with draft data
library(sqldf)
espn_df5 <- sqldf('
                  select
                    b.owner
                  , a.player
                  , a.pos
                  , a.team
                  , a.points
                  from
                    espn_df4 as a
                  , draft_df as b
                  where
                    a.player = b.player
                  and a.pos = b.position
                  and a.team = b.team')
anti_join(draft_df,espn_df4,by = c("player"))
filter(draft_df, position == "D/ST")
filter(espn_df4, pos == "D/ST")    
# just merge the defenses here
d_merge <- inner_join(filter(draft_df, position == "D/ST"),
           filter(espn_df4, pos == "D/ST"), by = c("team")) %>%
    select(1,2,3,4,8)
colnames(d_merge) <- c("owner","player","pos","team","points")

espn_df6 <- bind_rows(espn_df5,d_merge)
# Now head coaches, Matovina's FA kicker and Jordy Nelson are excluded 

write.csv(espn_df6, "espn_projections.csv", row.names = FALSE)
