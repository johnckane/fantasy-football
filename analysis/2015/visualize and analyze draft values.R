setwd("/home/john/Fantasy Football/2015 Prep")
library(ggplot2)
library(dplyr)

df <- read.csv("adp_draft.csv", stringsAsFactors=FALSE, header=TRUE)
str(df)
df <- filter(df, keeper == 0)

plot <- ggplot(data=df,aes(x=pick, y=value)) +
        geom_point(aes(colour=as.factor(year))) +
        facet_wrap(~pos,ncol=1)  
plot + geom_smooth(method="loess") + theme(legend.position=c(0.9, 0.9)) + scale_y_continuous(limits=c(0,105))

## averages by pick
summary <- df %>%
     group_by(pos,pick) %>%
     summarise(avg = mean(value),
               cnt = n())


plot2 <- ggplot(data=player_cost_points,aes(x=fitted_cost,y=fitted_points))+
    geom_point(#aes(colour=as.factor(year))
        ) +
    facet_wrap(~pos,ncol=1)
plot2 + geom_smooth(method="loess")
str(df)
