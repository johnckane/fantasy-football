library(sqldf)
library(dplyr)
library(ggplot2)
library(reshape2)

setwd("/home/john/stats_corner")

actual <- read.csv('actual_through_week9.csv')
projected <- read.csv('projections_through_week9.csv')

data <- sqldf('
              select
                a.player,
                b.position,
                a.fantpt as actual_points,
                a.week,
                b.projected_points,      
                a.fantpt - b.projected_points as points_diff
              from
                actual as a,
                projected as b
              where
                  a.player = b.player
              and a.week = b.week')

data <- mutate(data,square_diff = points_diff**2)

#Filter out the top 32 QB's, 32 RB's, 24 TE's, 48 WR's
top_n <- function (x, n, wt = NULL) 
{
  wt <- substitute(wt) # new line to correct is.null(wt)
  if (is.null(wt)) {
    vars <- tbl_vars(x)
    message("Selecting by ", vars[length(vars)])
    wt <- as.name(vars[length(vars)])
  }
  call <- substitute(filter(x, rank(desc(wt), ties.method = "min") <= 
                              n), list(n = n, wt = substitute(wt)))
  eval(call)
}

data2 <- rbind(
  # QB
  data %>%
    filter(position==0) %>%
    group_by(week) %>%
    arrange(desc(projected_points)) %>%
    top_n(24,projected_points),
  # RB
  data %>%
    filter(position==2) %>%
    group_by(week) %>%
    arrange(desc(projected_points)) %>%
    top_n(24,projected_points),
  # WR
  data %>%
    filter(position==4) %>%
    group_by(week) %>%
    arrange(desc(projected_points)) %>%
    top_n(36,projected_points),
  # TE
  data %>%
    filter(position==6) %>%
    group_by(week) %>%
    arrange(desc(projected_points)) %>%
    top_n(12,projected_points))

write.csv(data2,"projections_analysis_data.csv")
# By position and week calculate the MSE and MAE
data_mae_rmse <- data2 %>%
                  group_by(position,week) %>%
                  summarise(rmse = sqrt(mean(square_diff)),
                            mae  = mean(abs(points_diff))) %>%
                  arrange(rmse,mae)
# plot these
data_mae_rmse2 <- melt(data_mae_rmse,id.vars = c("position","week"))
error_plot <- ggplot(data=data_mae_rmse2,aes(x=week,y=value,
                                             group=as.factor(position),
                                             colour=as.factor(position)))
error_plot +
  geom_point() +
  geom_line() +
  facet_grid(~variable)


## Standardize these by week
data_summary <- data %>%
                  group_by(position,week) %>%
                  summarise(mean_proj_points   = mean(projected_points),
                            sd_proj_points     = sd(projected_points),
                            mean_actual_points = mean(actual_points),
                            sd_actual_points   = sd(actual_points)) %>%
                            arrange(position,week)

#plot these values
data_summary2 <- melt(data_summary,id.vars = c("position","week"))
summary_plot <- ggplot(data=data_summary2,aes(x=week,y=value,
                                             group=as.factor(position),
                                             colour=as.factor(position)))
summary_plot +
  geom_point() +
  geom_line() +
  facet_grid(~variable)

### Standardize the values
data_standard <- data %>%
                   group_by(position,week) %>%
                   mutate(projected_z = (projected_points - mean(projected_points))/sd(projected_points),
                          actual_z    = (actual_points - mean(projected_points))/sd(projected_points),
                          diff_z      = actual_z - projected_z,
                          diff_z_sq   = diff_z**2) 
## Summarize the standard errors, QC check more than anything
data_standard_summary <- data_standard %>%
                          group_by(position,week) %>%
                          summarise(mean_projected_z   = mean(projected_z),
                          sd_projected_z     = sd(projected_z),
                          mean_actual_z = mean(actual_z),
                          sd_actual_z   = sd(actual_z)) %>%
                          arrange(position,week)
## Now calculate error metrics
data_standard_mae_rmse <- data_standard %>%
                            group_by(position,week) %>%
                            summarise(rmse_z = sqrt(mean(diff_z_sq)),
                                      mae_z  = mean(abs(diff_z))) %>%
                            arrange(rmse_z,mae_z)
# plot these
data_standard_mae_rmse2 <- melt(data_standard_mae_rmse,id.vars = c("position","week"))
error_plot <- ggplot(data=data_standard_mae_rmse2,aes(x=week,y=value,
                                             group=as.factor(position),
                                             colour=as.factor(position)))
error_plot +
  geom_point() +
  geom_line() +
  facet_grid(~variable)
                                                                              


# Now evaluate correlations week by week
data_correlations <- data2 %>%
                      group_by(position,week) %>%
                      summarise(corr = cor(projected_points,actual_points)) %>%
                      arrange(desc(corr))

data_correlations_summary <- data_correlations %>%
                              group_by(position) %>%
                              summarise(mean_corr = mean(corr))
# plot these correlations
corr_plot <- ggplot(data=data_correlations,aes(x=week,y=corr,
                                               group=as.factor(position),
                                               colour=as.factor(position)))
corr_plot +
  geom_point() +
  geom_line()

### Look at how errors change by projected points
p1 <- ggplot(data=data_standard,aes(x=projected_points,y=diff_z))
p1 +
  geom_point() +
  facet_grid(position~week) +
  geom_smooth(method="lm") +
  geom_text(data=subset(data_standard, diff_z >= 3|diff_z <= -3),
            aes(projected_points,diff_z,label=player,size=1))

# Look at absolute value of difference
p2 <- ggplot(data=data_standard,aes(x=projected_points,y=abs(diff_z)))
p2 +
  geom_point() +
  facet_grid(position~week) +
  geom_smooth(method="lm") +
  geom_text(data=subset(data_standard, diff_z >= 3|diff_z <= -3),
            aes(projected_points,abs(diff_z),label=player,size=1))
# Let's pull the slopes for each of those lines, first non-absolute vales
data_standard <- filter(data_standard,position!=17)

results <- data_standard %>% 
  group_by(position,week) %>%
  do(mod = lm(diff_z~projected_points, data = .)) %>%
  do(data.frame(var = names(coef(.$mod)),
                coef = coef(.$mod),
                position = .$position,
                week     = .$week))

error_slopes <- filter(results,var=='projected_points')
## plot these
p3 <- ggplot(data=error_slopes,aes(x=week,y=coef,
                                  group=as.factor(position),
                                  colour=as.factor(position)))
p3 +
  geom_point() +
  geom_line() 
# Now do it for absolute values
results_abs_val <- data_standard %>% 
  group_by(position,week) %>%
  do(mod = lm(abs(diff_z)~projected_points, data = .)) %>%
  do(data.frame(var = names(coef(.$mod)),
                coef = coef(.$mod),
                position = .$position,
                week     = .$week))

abs_val_error_slopes <- filter(results_abs_val,var=='projected_points')
## plot these
p4 <- ggplot(data=abs_val_error_slopes,aes(x=week,y=coef,
                                           group=as.factor(position),
                                           colour=as.factor(position)))
p4 +
  geom_point() +
  geom_line() 

#### Now calculate the correlations
data_correlations_diff <- data_standard %>%
  group_by(position,week) %>%
  summarise(corr = cor(projected_points,points_diff)) %>%
  arrange(desc(corr))

data_correlations_summary_diff <- data_correlations_diff %>%
  group_by(position) %>%
  summarise(mean_corr = mean(corr))
# plot these correlations
corr_plot <- ggplot(data=data_correlations_diff,aes(x=week,y=corr,
                                               group=as.factor(position),
                                               colour=as.factor(position)))
corr_plot +
  geom_point() +
  geom_line()
### Repeat again with absolute values
data_correlations_diff_abs_val <- data_standard %>%
  group_by(position,week) %>%
  summarise(corr = cor(projected_points,abs(points_diff))) %>%
  arrange(desc(corr))

data_correlations_summary_diff_abs_val <- data_correlations_diff_abs_val %>%
  group_by(position) %>%
  summarise(mean_corr = mean(corr))
# plot these correlations
corr_plot <- ggplot(data=data_correlations_diff_abs_val,aes(x=week,y=corr,
                                                    group=as.factor(position),
                                                    colour=as.factor(position)))
corr_plot +
  geom_point() +
  geom_line()
