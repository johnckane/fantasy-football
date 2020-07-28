library(reshape2)
library(dplyr)
games_played_2015 <- filter(ungroup(games_played), year == 2015)
f <- function(a,b){
    df <- data.frame(A = games_played_2015$points[games_played_2015$owner==a],
                     B = games_played_2015$points[games_played_2015$owner==b])
    df_melt <- melt(df)
    df_melt %>% arrange(desc(value))
    df_melt$p = 1/dim(df)[1]
    df_melt <- df_melt %>%
        group_by(variable) %>%
        mutate(rk = rank(value)) %>%
        ungroup() %>%
        arrange(desc(value))
    df_melt$weight <- 1
    for(i in 1:(dim(df_melt)[1])){
        df_melt$p_win[i] <- sum(df_melt$weight[which(df_melt$variable != df_melt$variable[i] & df_melt$value[i] > df_melt$value)])/sum(df_melt$weight[which(df_melt$variable == df_melt$variable[i])])
    }
    df_melt <- df_melt %>%
        group_by(variable) %>%
        summarise(prob_win = sum(p_win*p))
    df_melt
    df_cast <- dcast(df_melt,.~variable,value.var="prob_win")
    return(df_cast)
}
f("Kane","Thieneman")$A
games_to_play$win_proj <- rep(0,dim(games_to_play)[1])
for(i in 1:dim(games_to_play)[1]){
    games_to_play$win_proj[i] = f(games_to_play$owner[i],games_to_play$Opponent[i])$A
}

games_to_play %>% group_by(owner) %>% summarise(proj_win_total = sum(win_proj)) %>% arrange(desc(proj_win_total))
str(games_to_play)
