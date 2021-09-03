load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/qb_cluster_data_last5.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/rb_cluster_data_last5.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/wr_cluster_data_last5.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/te_cluster_data_last5.Rda")
#load adp
load("/home/john/projects/fantasy-football/data/adp-data/adp_2021_ranked_w_age_bye.Rda")
# load draft data
library(lpSolve)
keeper_data <- data.frame(new_cluster = numeric(),
                          avg_cost = numeric(),
                          avg_ppg = numeric(),
                          player = character(),
                          position = character(),
                          stringsAsFactors = F)







keeper_clusters <- rep(0,9)


keeper_player <- 
  c(
    "Conner",
    "Murray",
    "Godwin",
    "Diggs",
    "Aiyuk",
    "Henry",
    "Moore",
    "Edwards-Hellaire",
    "Winston"
  )

keeper_position <- c('RB','RB','WR','WR','WR','TE','WR','RB','QB')
# Get these from the cost to keep values
keeper_avg_cost <- c(47,9,34,31,7,9,33,81,7)


# Get these from the cluster estimates
keeper_avg_ppg <- c(7,6,9.2,11.4,8.5,5.3,8.5,12,15.3)


adp_2021_ranked_w_age_bye %>%
  #filter(last_name == 'Jones') %>%
  select(player, pos_adp) %>%
  arrange(pos_adp) %>%
  print(n = 27)

rb_cluster_data %>%
  arrange(desc(avg_ppg)) %>%
  ungroup() %>%
  filter(row_number() == 28)


keeper_data <- data.frame(new_cluster = keeper_clusters,
                          avg_cost = keeper_avg_cost,
                          avg_ppg = keeper_avg_ppg,
                          player = keeper_player,
                          position = keeper_position,
                          stringsAsFactors = F)

clustered_data <- bind_rows(qb_cluster_data %>% mutate(position = 'QB'),
                            rb_cluster_data %>% mutate(position = 'RB'),
                            wr_cluster_data %>% mutate(position = 'WR'),
                            te_cluster_data %>% mutate(position = 'TE'))

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
c_keeper <- c(rep(1,9),rep(0,165))

direction <- c('<=','==','==','==','==','<=')


rhs <- c(292,2,2,1,3,2)
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

table(unlist(results))

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


## No TEs



## what if RBs 1,2,3 are gone?
no_rb_clustered_w_keeper <-
  clustered_w_keeper %>%
  filter(!(player %in% c('RB-1-1','RB-1-2','RB-1-3')))

no_rb_clustered_w_keeper$in_solution <- NULL

head(no_rb_clustered_w_keeper)

objective <- no_rb_clustered_w_keeper$avg_ppg

## Lay out the constraints ##
# Total salary row
c_salary <- no_rb_clustered_w_keeper$avg_cost
# 2 QBs
c_qb <- ifelse(no_rb_clustered_w_keeper$position=='QB',1,0)
# 2 RBs 
c_rb <- ifelse(no_rb_clustered_w_keeper$position=='RB',1,0)
# 1 TE 
c_te <- ifelse(no_rb_clustered_w_keeper$position=='TE',1,0)
# 3 WRs 
c_wr <- ifelse(no_rb_clustered_w_keeper$position=='WR',1,0)
# 2 keepers
c_keeper <- c(rep(1,9),rep(0,162))

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
length(solved$solution)
dim(no_rb_clustered_w_keeper)
no_rb_clustered_w_keeper$in_solution = solved$solution

no_rb_clustered_w_keeper %>% filter(in_solution == 1) 

## what if top TEs are gone?
no_te_clustered_w_keeper <-
  clustered_w_keeper %>%
  filter(!(player %in% c('TE-1-2','TE-1-1')))

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
c_keeper <- c(rep(1,10),rep(0,163))

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
length(solved$solution)
dim(no_te_clustered_w_keeper)
no_te_clustered_w_keeper$in_solution = solved$solution

no_te_clustered_w_keeper %>% filter(in_solution == 1)



## what if all top 10 RBs are gone
no_rb_clustered_w_keeper <-
  clustered_w_keeper %>%
  filter(!(player %in% c('RB-1-1','RB-1-2','RB-1-3','RB-2-1','RB-2-2','RB-2-3',
                         'RB-3-1','RB-3-2','RB-3-3','RB-3-4')))

no_rb_clustered_w_keeper$in_solution <- NULL

head(no_rb_clustered_w_keeper)

objective <- no_rb_clustered_w_keeper$avg_ppg

## Lay out the constraints ##
# Total salary row
c_salary <- no_rb_clustered_w_keeper$avg_cost
# 2 QBs
c_qb <- ifelse(no_rb_clustered_w_keeper$position=='QB',1,0)
# 2 RBs 
c_rb <- ifelse(no_rb_clustered_w_keeper$position=='RB',1,0)
# 1 TE 
c_te <- ifelse(no_rb_clustered_w_keeper$position=='TE',1,0)
# 3 WRs 
c_wr <- ifelse(no_rb_clustered_w_keeper$position=='WR',1,0)
# 2 keepers
c_keeper <- c(rep(1,9),rep(0,155))

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
length(solved$solution)
dim(no_rb_clustered_w_keeper)
no_rb_clustered_w_keeper$in_solution = solved$solution

no_rb_clustered_w_keeper %>% filter(in_solution == 1) 