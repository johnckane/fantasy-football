setwd("/home/john/stats_corner/2015/keeper_analysis/")

draft <- read.csv("draft_data.csv", header = TRUE, stringsAsFactors = FALSE)


### First need to arrange by player, so that we can assign appropriate keeper values

draft <- draft %>%
    arrange(player,year) %>%
    group_by(player) %>%
    mutate(player_obs = row_number())
View(draft)

## Easiest way to do this is in a loop I think
draft$adj_value <- draft$value
for(i in 1:dim(draft)[1]){
    if(draft$keeper[i] == 1){
        if(draft$player_obs[i] != 1){
            if(draft$year[i] == (draft$year[i-1] + 1)){
            draft$adj_value[i] <- draft$adj_value[i-1] + 7
            }
        }
    }
}

write.csv(draft,"draft_adjusted.csv",row.names = FALSE)
