library(XML)
library(httr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(directlabels)
library(dplyr)


url <- "https://docs.google.com/spreadsheets/d/1bwTLuC4bMte00o46yOlmUun0WuiHhe8mwdZuZT3ApVE/pubhtml"

readSpreadsheet <- function(url, sheet = 1){
  library(httr)
  r <- GET(url)
  html <- content(r)
  sheets <- readHTMLTable(html, header=FALSE, stringsAsFactors=FALSE)
  df <- sheets[[sheet]]
  dfClean <- function(df){
    nms <- t(df[1,])
    names(df) <- nms
    df <- df[-1,-1] 
    row.names(df) <- seq(1,nrow(df))
    df
  }
  dfClean(df)
}
df <- readSpreadsheet(url)

df <- df[-1,]


df$projected_points = as.numeric(df$projected_points)
df$actual_points = as.numeric(df$actual_points)
df$minutes_remaining = as.numeric(df$minutes_remaining)


# Plot 1, do it by calendar day/time
# First limit datapoints so it's not too cluttered
times <- c("Sunday 12:00 PM","Sunday 1:00 PM","Sunday 2:00 PM",
           "Sunday 3:00 PM", "Sunday 6:00 PM","Sunday 10:15 PM",
            "Monday 11:00 PM")
df2 <- filter(df, time %in% times)
df2$time <- factor(df2$time,levels = times)

plot1 <- ggplot(data=df2,aes(time)) +
  geom_line(aes(y=actual_points,colour=team,group=team)) +
  geom_line(aes(y=projected_points,colour=team,group=team),linetype="dotted") +
  geom_point(aes(y=actual_points,colour=team,group=team)) +
  facet_wrap(~game) +
  geom_dl(aes(y=actual_points,label=team,color=team),list('smart.grid',cex=0.75)) +
  theme(legend.position='none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Points (Projected and Acutal) by Calendar Day/Time") +
  ylab("Points") +
  xlab("Day/Time")

#need a dataframe for annotations in first facet only
ann_text <- data.frame(game = rep("Harrington vs. Regan",2),
                       projected_points = c(100,75),
                       time = c("Sunday 12:00 PM", "Sunday 12:00 PM"),
                       lab = c("Projected","Actual"))

plot1 +
  geom_text(data=ann_text,aes(y=projected_points,label=lab,size=2)) +
  geom_segment(data=ann_text,aes(x=1.5,xend=2,y=100,yend=110),arrow=arrow(length = unit(0.1,'cm'))) +
  geom_segment(data=ann_text,aes(x=1.5,xend=2,y=75,yend=65),arrow=arrow(length = unit(0.1,'cm')))


#### Plot 2, projected points by time remaining
plot2 <- ggplot(data=df,aes(minutes_remaining)) +
  geom_line(aes(y=actual_points,colour=team,group=team)) +
  geom_line(aes(y=projected_points,colour=team,group=team),linetype="dotted") +
  geom_point(aes(y=actual_points,colour=team,group=team)) +
  scale_x_reverse() +
  facet_wrap(~game) +
  geom_dl(aes(y=actual_points,label=team,color=team),list('smart.grid',cex=0.75)) +
  theme(legend.position='none') +
  ggtitle("Points (Projected and Acutal) by Minutes Remaining") +
  ylab("Points") +
  xlab("Minutes Remaining")

#need a dataframe for annotations in first facet only
ann_text <- data.frame(game = rep("Harrington vs. Regan",2),
                       projected_points = c(100,75),
                       minutes_remaining = c(550,550),
                       lab = c("Projected","Actual"))

plot2 +
  geom_text(data=ann_text,aes(y=projected_points,label=lab,size=2)) +
  geom_segment(data=ann_text,aes(x=490,xend=430,y=100,yend=110),arrow=arrow(length = unit(0.1,'cm'))) +
  geom_segment(data=ann_text,aes(x=510,xend=450,y=75,yend=65),arrow=arrow(length = unit(0.1,'cm')))

#need a dataframe for annotations 

  
  #theme(legend.position = "top",legend.box="horizontal") +
  theme(legend.text = element_text(colour=as.factor(team), size = 16, face = "bold"))
