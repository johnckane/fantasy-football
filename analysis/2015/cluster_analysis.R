setwd("/home/john/Fantasy Football/2015 Prep")
library(sqldf)
library(dplyr)

loess_cost <- read.csv('loess_cost.csv',stringsAsFactors = FALSE,header = TRUE)
loess_ppg <- read.csv('loess_ppg.csv',stringsAsFactors = FALSE, header = TRUE)

modeled <- inner_join(loess_cost,loess_ppg,by=c('pos','pick'))


m_qb <- filter(modeled,pos=='QB')
m_rb <- filter(modeled,pos=='RB')
m_wr <- filter(modeled,pos=='WR')
m_te <- filter(modeled,pos=='TE')

kmeans_qb <- kmeans(m_qb %>% select(fitted_cost,fitted_points),
                    6,
                    nstart = 20)
kmeans_qb_df <- data.frame(m_qb$pick,m_qb$fitted_cost,m_qb$fitted_points,
                           as.factor(kmeans_qb$cluster)) %>%
    `colnames<-`(c('pick','cost','points','cluster')) 
a<-qplot(data=kmeans_qb_df,
      x = pick,
      y = cost,
      colour = cluster)
b<-qplot(data=kmeans_qb_df,
      x = pick,
      y = points,
      colour = cluster)
library(gridExtra)
grid.arrange(a,b,ncol=1)



kmeans_rb <- kmeans(m_rb %>% select(fitted_cost,fitted_points),
                    15,
                    nstart= 20)
kmeans_rb_df <- data.frame(m_rb$pick,m_rb$fitted_cost,m_rb$fitted_points,
                            as.factor(kmeans_rb$cluster)) %>%
    `colnames<-`(c('pick','cost','points','cluster')) 
a<-qplot(data=kmeans_rb_df,
         x = pick,
         y = cost,
         colour = cluster)
b<-qplot(data=kmeans_rb_df,
         x = pick,
         y = points,
         colour = cluster)
grid.arrange(a,b,ncol=1)


kmeans_wr <- kmeans(m_wr %>% select(fitted_cost,fitted_points),
                    18,
                    nstart= 20)
kmeans_wr_df <- data.frame(m_wr$pick,m_wr$fitted_cost,m_wr$fitted_points,
                           as.factor(kmeans_wr$cluster)) %>%
    `colnames<-`(c('pick','cost','points','cluster')) 
a<-qplot(data=kmeans_wr_df,
         x = pick,
         y = cost,
         colour = cluster)
b<-qplot(data=kmeans_wr_df,
         x = pick,
         y = points,
         colour = cluster)
grid.arrange(a,b,ncol=1)

kmeans_te <- kmeans(m_te %>% select(fitted_cost,fitted_points),
                    4,
                    nstart= 20)
kmeans_te_df <- data.frame(m_te$pick,m_te$fitted_cost,m_te$fitted_points,
                           as.factor(kmeans_te$cluster)) %>%
    `colnames<-`(c('pick','cost','points','cluster')) 
a<-qplot(data=kmeans_te_df,
         x = pick,
         y = cost,
         colour = cluster)
b<-qplot(data=kmeans_te_df,
         x = pick,
         y = points,
         colour = cluster)
grid.arrange(a,b,ncol=1)

all_data2 <- data.frame(rbind(kmeans_qb_df,
                   kmeans_rb_df,
                   kmeans_te_df,
                   kmeans_wr_df))
all_data2$pos <- c(rep('QB',29),
                   rep('RB',62),
                   rep('TE',21),
                   rep('WR',73))

#rename these clusters so that the top scoring clusters are 1, etc.


averages <- all_data2 %>%
    group_by(pos,cluster) %>%
    summarise(mean_cost = round(mean(cost),0),
              range_cost = paste(round(min(cost),0),", ",round(max(cost),0),sep=""),
              mean_points = round(mean(points),1),
              range_points = paste(round(min(points),1),", ", round(max(points),1),sep=""),
              range_picks = paste(min(pick),", ",max(pick), sep=""))
with(averages,table(pos,cluster))

cluster_data <- sqldf('
                      select
                        a.pos
                      , a.pick
                      , b.cluster
                      , b.mean_cost
                      , b.range_cost
                      , b.mean_points
                      , b.range_points
                      , b.range_picks
                      from
                        all_data2 as a
                      , averages as b
                      where
                        a.pos = b.pos
                      and a.cluster = b.cluster')

write.csv(cluster_data,'cluster_data.csv')

expand_qb <- bind_rows(averages2 %>% filter(pos == 'qb'), averages2 %>% filter(pos == 'qb'))

expand_rb <- bind_rows(averages2 %>% filter(pos == 'rb'), averages2 %>% filter(pos == 'rb'))

expand_wr <- bind_rows((bind_rows(averages2 %>% filter(pos == 'wr'), 
                                  averages2 %>% filter(pos == 'wr'))),
                       averages2 %>% filter(pos == 'wr'))
                       
expand_te <- averages2 %>% filter(pos == 'te')


new_data <- bind_rows(expand_qb,expand_rb,expand_wr,expand_te)

objective <- new_data$mean_points

c1 <- new_data$mean_cost

# 2 QBs
c2 <- ifelse(new_data$pos=='qb',1,0)
# 2 RBs 
c3 <- ifelse(new_data$pos=='rb',1,0)
# 1 TE 
c4 <- ifelse(new_data$pos=='te',1,0)
# 3 WRs 
c5 <- ifelse(new_data$pos=='wr',1,0)



constraints <- matrix(rbind(c1,c2,c3,c4,c5),nrow=5)

## Set up the direction of the constrants ##
direction <- c('<=','==','==','==','==')

## Set up the right-hand-side of the equation
rhs <- c(242,1,1,1,3)

solve <- lp("max",
                  objective,
                  constraints,
                  direction,
                  rhs,
                  all.bin=TRUE,
                  num.bin.solns = 1)

solution <- data.frame(as.character(new_data$pos),
                             as.character(new_data$cluster),
                             new_data$mean_cost,
                             new_data$mean_points,
                             solve$solution) %>% 
    filter(solve$solution==1)
solution <- solution[,-5]
colnames(solution) <- c("pos","cluster","cost","points")
solution
filter(averages2, (pos == 'qb' & cluster2 == 2) |
                 (pos == 'rb' & cluster2 == 7) |
                 (pos == 'wr' & cluster2 == 3) |
                 (pos == 'wr' & cluster2 == 7) |
                 (pos == 'te' & cluster2 == 1))
