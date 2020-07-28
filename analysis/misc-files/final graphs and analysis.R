setwd("/home/john/stats_corner")

library(sqldf)
library(dplyr)
library(ggplot2)
library(reshape2)
library(directlabels)

data2 <- read.csv("projections_analysis_data.csv")

## First standardize by week
data_standard <- data2 %>%
  group_by(position,week) %>%
  mutate(projected_z = (projected_points - mean(projected_points))/sd(projected_points),
         actual_z    = (actual_points - mean(projected_points))/sd(projected_points),
         diff_z      = actual_z - projected_z,
         diff_z_sq   = diff_z**2) 


## Week 9 correlations and plots ##
week9 <- filter(data_standard,week==9)
table(week9$position)

week9_corr <- week9 %>% 
                group_by(position) %>%
                summarise(corr = cor(projected_points,actual_points))
week9_corr <- mutate(week9_corr,
                     my_label = paste("corr = ",
                                      formatC(corr, format="f", digits=2)))
week9$position <- factor(week9$position)
levels(week9$position) <- c("QB","RB","WR","TE")
week9_corr$position <- factor(week9_corr$position)
levels(week9_corr$position) <- levels(week9$position)
plot1 <- ggplot(data=week9,aes(x=projected_points,y=actual_points))
plot1 +
  geom_point() +
  facet_wrap(~position,ncol=2) +
  geom_smooth(method="lm",se=FALSE) +
  geom_text(data=week9_corr,aes(x=10,y=33,label=my_label)) +
  geom_dl(data=subset(week9,abs(diff_z) > 3),
          aes(x=projected_points,y=actual_points,label=player),
          list(cex=0.5,method="smart.grid")) +
  scale_x_continuous("Projected Points") +
  scale_y_continuous("Actual Points Scored") +
  ggtitle("Week 9 Actual vs. Projected Points Scored by Position")


## Correlation graph by week
all_corr <- data_standard %>% 
  group_by(week,position) %>%
  summarise(corr = cor(projected_points,actual_points))

all_corr$position <- factor(all_corr$position)
levels(all_corr$position) <- c("QB","RB","WR","TE")

plot2 <- ggplot(data=all_corr,aes(x=as.factor(week),y=corr,
                                  group=as.factor(position),
                                  colour=as.factor(position)))
plot2 +
    geom_point() +
    geom_line() +
    theme(legend.position='none')+
    labs(colour = "Position") +
    #scale_y_continuous(limits = c(-0.4, 0.75)) +
    geom_dl(aes(label=position),method="first.points") +
    scale_x_discrete("Week") +
    scale_y_continuous("Correlation of Projected and Actual Points") +
    ggtitle("Projection Correlations by Position and Week")

## Look at RMSE and MAE last week
data_mae_rmse_week9 <- week9 %>%
  group_by(position) %>%
  summarise(rmse = sqrt(mean(square_diff)),
            mae  = mean(abs(points_diff))) %>%
  arrange(rmse,mae)

## Mean (SD) by position all season
data_season_summary <- data_standard %>%
  group_by(position) %>%
  summarise(mean_a = mean(actual_points),
            sd_a   = sd(actual_points),
            mean_p = mean(projected_points),
            sd_p   = sd(projected_points))

data_standard_mae_rmse <- data_standard %>%
  group_by(position,week) %>%
  summarise(rmse_z = sqrt(mean(diff_z_sq)),
            mae_z  = mean(abs(diff_z))) %>%
  arrange(rmse_z,mae_z)

filter(data_standard_mae_rmse,week == 9)

data_standard_mae_rmse2 <- melt(data_standard_mae_rmse,id.vars = c("position","week"))

error_plot <- ggplot(data=data_standard_mae_rmse2,aes(x=as.factor(week),y=value,
                                                      group=as.factor(position),
                                                      colour=as.factor(position)))
data_standard_mae_rmse2$variable <- factor(data_standard_mae_rmse2$variable)
levels(data_standard_mae_rmse2$variable) <- c("Root Mean Squared Error", "Mean Absolute Error")

data_standard_mae_rmse2$position <- factor(data_standard_mae_rmse2$position)
levels(data_standard_mae_rmse2$position) <- c("QB","RB","WR","TE")

data_standard_mae_rmse %>% group_by(position) %>%
     summarise(sum_rmse = sum(rmse_z),
                rum_mae  = sum(mae_z))

error_plot +
  geom_point() +
  geom_line() +
  facet_grid(~variable) +
  scale_x_discrete("Week") +
  scale_y_continuous("Value") +
  geom_dl(aes(label=position),method="smart.grid") +
  theme(legend.position = 'none') +
  ggtitle("Error Metrics by Week and Position")


## Error magnitude
p1 <- ggplot(data=data_standard,aes(x=projected_points,y=points_diff))
p1 +
  geom_point() +
  facet_grid(week~position) +
  geom_smooth(method="lm") 

summary(lm(data=data_standard,points_diff~projected_points))
# Look at absolute value of difference
p2 <- ggplot(data=data_standard,aes(x=projected_points,y=abs(points_diff)))
p2 +
  geom_point() +
  facet_grid(week~position) +
  geom_smooth(method="lm") 

summary(lm(data=data_standard,abs(points_diff)~projected_points))

