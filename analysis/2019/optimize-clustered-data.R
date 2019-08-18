# load libraries
library(tidyverse)
library(lpSolve)

# load the four datasets
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/qb_cluster_data.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/rb_cluster_data.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/wr_cluster_data.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/te_cluster_data.Rda")

# bind them together
clustered_data <- bind_rows(qb_cluster_data %>% mutate(position = 'QB'),
                            rb_cluster_data %>% mutate(position = 'RB'),
                            wr_cluster_data %>% mutate(position = 'WR'),
                            te_cluster_data %>% mutate(position = 'TE'))

clustered_data
wr_cluster_data %>% View()
## Set up the objective function ##
objective <- clustered_data$avg_ppg


## Lay out the constraints ##
# Total salary row
c_salary <- clustered_data$avg_cost
# 2 QBs
c_qb <- ifelse(clustered_data$position=='QB',1,0)
# 2 RBs 
c_rb <- ifelse(clustered_data$position=='RB',1,0)
# 1 TE 
c_te <- ifelse(clustered_data$position=='TE',1,0)
# 3 WRs 
c_wr <- ifelse(clustered_data$position=='WR',1,0)

direction <- c('<','==','==','==','==')


rhs <- c(292,2,2,1,3)
constraints <- matrix(rbind(c_salary,
                            c_qb,
                            c_rb,
                            c_te,
                            c_wr),
                      nrow = 5)


solved <-
lp(direction = "max",
   objective.in = objective,
   const.mat = constraints,
   const.dir = direction,
   const.rhs = rhs,
   all.bin = TRUE,
   num.bin.solns = 1)

solved$solution 
clustered_data$in_solution = solved$solution

clustered_data %>% filter(in_solution == 1) 
clustered_data %>% 
  filter(in_solution == 1) %>% 
  ungroup() %>% 
  summarise(total_ppg = sum(avg_ppg),
            total_cost = sum(avg_cost))


## Account for keepers somehow...

keeper_data <- data.frame(new_cluster = numeric(),
                          avg_cost = numeric(),
                          avg_ppg = numeric(),
                          player = character(),
                          position = character(),
                          stringsAsFactors = F)


keeper_clusters <- rep(0,10)
keeper_avg_cost <- c(19,20,51,105,7,45,26,9,15,24)
keeper_avg_ppg <- c(16.1,13.7,12.8,13.8,8.24,9.88,8.51,7.16,8.68,6.34)
keeper_player <- c('Goff','Darnold','Cook','Johnson','Williams','Kelce','Drake','Barber','Kupp','Sanders')
keeper_position <- c('QB','QB','RB','RB','WR','TE','RB','RB','WR','WR')

keeper_data <- data.frame(new_cluster = keeper_clusters,
                          avg_cost = keeper_avg_cost,
                          avg_ppg = keeper_avg_ppg,
                          player = keeper_player,
                          position = keeper_position,
                          stringsAsFactors = F)

# Jared Goff: 12
# Sam Darnold: 25
# Dalvin Cook: 10
# David Johnson: 5
# Mike Williams: 24
# Travis Kelce: 1
# Kenyan Drake: 26
# Peyton Barber: 46
# Cooper Kupp: 23
# Emmanuel Sanders: 47

clustered_w_keeper <- bind_rows(keeper_data,clustered_data)
clustered_w_keeper$in_solution <- NULL
head(clustered_w_keeper)

objective <- clustered_w_keeper$avg_ppg


## Lay out the constraints ##
# Total salary row
c_salary <- clustered_w_keeper$avg_cost
# 2 QBs
c_qb <- ifelse(clustered_w_keeper$position=='QB',1,0)
# 2 RBs 
c_rb <- ifelse(clustered_w_keeper$position=='RB',1,0)
# 1 TE 
c_te <- ifelse(clustered_w_keeper$position=='TE',1,0)
# 3 WRs 
c_wr <- ifelse(clustered_w_keeper$position=='WR',1,0)
# 2 keepers
c_keeper <- c(rep(1,10),rep(0,164))

direction <- c('<=','==','==','==','==','<=')


rhs <- c(290,2,2,1,3,2)
constraints <- matrix(rbind(c_salary,
                            c_qb,
                            c_rb,
                            c_te,
                            c_wr,
                            c_keeper),
                      nrow = 6)


solved <-
  lp(direction = "max",
     objective.in = objective,
     const.mat = constraints,
     const.dir = direction,
     const.rhs = rhs,
     all.bin = TRUE,
     num.bin.solns = 1)

solved$solution 

clustered_w_keeper$in_solution = solved$solution

clustered_w_keeper %>% filter(in_solution == 1) 
clustered_w_keeper %>% 
  filter(in_solution == 1) %>% 
  ungroup() %>% 
  summarise(total_ppg = sum(avg_ppg),
            total_cost = sum(avg_cost))


results <- list()
## 
for(d in c(150:292)){
  print(d)
  clustered_w_keeper$in_solution <- NULL
  rhs <- c(d,2,2,1,3,2)
  
  solved <-
    lp(direction = "max",
       objective.in = objective,
       const.mat = constraints,
       const.dir = direction,
       const.rhs = rhs,
       all.bin = TRUE,
       num.bin.solns = 1)
  
  clustered_w_keeper$in_solution = solved$solution
  
  clustered_w_keeper %>% filter(in_solution == 1) 
  
  results[[d]] <- clustered_w_keeper %>% filter(in_solution == 1) %>% pull(player)
  
}

unlist(results)

results_df <- as.data.frame(do.call(rbind,results))
results_df$budget <- c(150:292)

results_df

?spread
results_df_spread <-
  results_df %>%
  gather(.,
         place,
         player,
         V1:V8,
         factor_key=TRUE)

head(results_df_spread)

results_df_spread2 <-
  results_df_spread %>%
  select(-place) %>%
  mutate(present = 1)

p <- 
  ggplot(results_df_spread2,
         aes(player,budget)) + 
  geom_tile(aes(fill = present))
p


## Keep Kelce?

clustered_w_keeper$in_solution <- NULL
rhs <- c(247,2,2,0,3,1)

solved <-
  lp(direction = "max",
     objective.in = objective,
     const.mat = constraints,
     const.dir = direction,
     const.rhs = rhs,
     all.bin = TRUE,
     num.bin.solns = 1)

clustered_w_keeper$in_solution = solved$solution

clustered_w_keeper %>% filter(in_solution == 1) 


## what if TEs 1 and 2 are gone?
no_te_clustered_w_keeper <-
  clustered_w_keeper %>%
  filter(!(player %in% c('TE-1-1','TE-1-2')))

no_te_clustered_w_keeper$in_solution <- NULL

head(no_te_clustered_w_keeper)

objective <- no_te_clustered_w_keeper$avg_ppg


## Lay out the constraints ##
# Total salary row
c_salary <- no_te_clustered_w_keeper$avg_cost
# 2 QBs
c_qb <- ifelse(no_te_clustered_w_keeper$position=='QB',1,0)
# 2 RBs 
c_rb <- ifelse(no_te_clustered_w_keeper$position=='RB',1,0)
# 1 TE 
c_te <- ifelse(no_te_clustered_w_keeper$position=='TE',1,0)
# 3 WRs 
c_wr <- ifelse(no_te_clustered_w_keeper$position=='WR',1,0)
# 2 keepers
c_keeper <- c(rep(1,10),rep(0,162))

direction <- c('<=','==','==','==','==','<=')


rhs <- c(290,2,2,1,3,2)
constraints <- matrix(rbind(c_salary,
                            c_qb,
                            c_rb,
                            c_te,
                            c_wr,
                            c_keeper),
                      nrow = 6)


solved <-
  lp(direction = "max",
     objective.in = objective,
     const.mat = constraints,
     const.dir = direction,
     const.rhs = rhs,
     all.bin = TRUE,
     num.bin.solns = 1)

solved$solution 

no_te_clustered_w_keeper$in_solution = solved$solution

no_te_clustered_w_keeper %>% filter(in_solution == 1) 
