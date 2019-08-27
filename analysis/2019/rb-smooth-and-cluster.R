## load the tidyverse
library(tidyverse)
library(rpart)
## Load the data
load(file="/home/john/projects/fantasy-football/data/clustering-data/cost_adp_prod_cleaned.Rda")

rb_data <- 
  cost_adp_prod_cleaned %>%
  filter(position == 'RB',
         pos_adp < 100)

ggplot(data = rb_data,
       aes(x = pos_adp, y = adj_value)) +
  geom_point() +
  geom_smooth()

rb_smoothed_data <- data.frame(pos_adp = c(1:60))

rb_smoothed_data$smoothed_cost <- predict(object = loess(data = rb_data, adj_value~ pos_adp),
                                          newdata = rb_smoothed_data)

rb_smoothed_data$smoothed_ppg <- predict(object = loess(data = rb_data, ppg ~ pos_adp),
                                         newdata = rb_smoothed_data)
rb_smoothed_data

## center and scale 
rb_smoothed_data_to_scale <- rb_smoothed_data[,c(2,3)]
rb_smoothed_data_to_scale_scaled <- as.data.frame(scale(rb_smoothed_data_to_scale))
colnames(rb_smoothed_data_to_scale_scaled) <- c('smoothed_cost_scaled','smoothed_ppg_scaled')
rb_smoothed_data_to_scale_scaled  <- bind_cols(rb_smoothed_data,rb_smoothed_data_to_scale_scaled)
rb_smoothed_data_to_scale_scaled


max_clusters <- 20
output <- matrix(0,max_clusters-1,3)
data <- rb_smoothed_data_to_scale_scaled

for(i in c(2:max_clusters)){
  cluster_data <- data
  cluster_object <- kmeans(data[,c(4,5)],
                           centers = i,
                           nstart = 20)
  
  cluster_data$cluster <- cluster_object$cluster
  var_explained <- cluster_object$betweenss/cluster_object$totss
  
  cluster_data2 <-
    cluster_data %>%
    group_by(cluster) %>%
    summarise(avg_ppg = mean(smoothed_ppg),
              sd_ppg = sd(smoothed_ppg),
              avg_cost = mean(smoothed_cost),
              sd_cost = sd(smoothed_cost)) %>%
    arrange(desc(avg_ppg)) %>%
    mutate(new_cluster = row_number())
  
  cluster_data3 <-
    cluster_data %>%
    left_join(.,cluster_data2, by = "cluster") %>%
    group_by(new_cluster,pos_adp,avg_ppg,sd_ppg,avg_cost) %>%
    summarise(count = n()) %>%
    arrange(new_cluster,pos_adp)
  
  
  total_adp <-
    cluster_data %>%
    group_by(pos_adp) %>%
    summarise(total_pos_adp = n())
  
  total_cluster_size <-
    cluster_data3 %>%
    ungroup() %>%
    group_by(new_cluster) %>%
    summarise(total_new_cluster = sum(count))
  
  cluster_data4 <-
    cluster_data3 %>%
    left_join(total_adp, by = "pos_adp") %>%
    left_join(total_cluster_size, by = "new_cluster") %>%
    mutate(pct_of_cluster = count/total_new_cluster,
           pct_of_adp = count/total_pos_adp)
  
  
  explain_adp <- rpart(data = cluster_data4, pos_adp ~ new_cluster, method = "anova")
  tmp <- printcp(explain_adp)
  rsquare_values <- 1-tmp[,c(3,4)]  
  rsquare <- rsquare_values[nrow(rsquare_values),][[1]]
  
  output[i-1,] <- c(i,var_explained,rsquare)
  
  rm(cluster_data,
     cluster_data2,
     cluster_data3,
     cluster_data4,
     cluster_object,
     var_explained,
     total_adp,
     total_cluster_size,
     explain_adp,
     tmp,
     rsquare_values,
     rsquare)
}

output_df <- data.frame(num_clusters = output[,1],
                        var_explained = output[,2],
                        adp_explained = output[,3])
ggplot(data = output_df,
       aes(x = var_explained,
           y = adp_explained,
           label = num_clusters)) +
  geom_point() +
  geom_label()

# 5,6,7,10,15
f <- function(x){
  cluster_data <- data
  cluster_object <- kmeans(data[,c(4,5)],
                           centers = x,
                           nstart = 20)
  
  cluster_data$cluster <- cluster_object$cluster
  var_explained <- cluster_object$betweenss/cluster_object$totss
  var_explained
  cluster_data2 <-
    cluster_data %>%
    group_by(cluster) %>%
    summarise(avg_ppg = mean(smoothed_ppg),
              sd_ppg = sd(smoothed_ppg),
              avg_cost = mean(smoothed_cost),
              sd_cost = sd(smoothed_cost)) %>%
    arrange(desc(avg_ppg)) %>%
    mutate(new_cluster = row_number())
  
  cluster_data3 <-
    cluster_data %>%
    left_join(.,cluster_data2, by = "cluster") %>%
    group_by(new_cluster,pos_adp,avg_ppg,sd_ppg,avg_cost) %>%
    summarise(count = n()) %>%
    arrange(new_cluster,pos_adp)
  
  cluster_data3 %>%
    group_by(new_cluster) %>%
    summarise(min_adp = min(pos_adp),
              max_adp = max(pos_adp),
              len = n()) %>%
    mutate(full_set = ifelse(max_adp-min_adp +1 == len,T,F))
}
## Start with 5
f(5)
f(6)
f(7)
f(10)
f(15)

cluster_data <- data
cluster_object <- kmeans(data[,c(4,5)],
                         centers = 10,
                         nstart = 20)

cluster_data$cluster <- cluster_object$cluster
var_explained <- cluster_object$betweenss/cluster_object$totss
var_explained
cluster_data2 <-
  cluster_data %>%
  group_by(cluster) %>%
  summarise(avg_ppg = mean(smoothed_ppg),
            sd_ppg = sd(smoothed_ppg),
            avg_cost = mean(smoothed_cost),
            sd_cost = sd(smoothed_cost)) %>%
  arrange(desc(avg_ppg)) %>%
  mutate(new_cluster = row_number())

cluster_data3 <-
  cluster_data %>%
  left_join(.,cluster_data2, by = "cluster") %>%
  group_by(new_cluster,pos_adp,avg_ppg,sd_ppg,avg_cost) %>%
  summarise(count = n()) %>%
  arrange(new_cluster,pos_adp)

cluster_data3 %>%
  group_by(new_cluster) %>%
  summarise(min_adp = min(pos_adp),
            max_adp = max(pos_adp),
            len = n()) %>%
  mutate(full_set = ifelse(max_adp-min_adp +1 == len,T,F))








# create a dataset like
# |player|ppg|cost|
# with one obs per cluster
rb_cluster_data <-
cluster_data3 %>%
  ungroup() %>%
  select(new_cluster,avg_ppg,avg_cost) %>%
  group_by(new_cluster) %>%
  mutate(player = paste0("RB-",new_cluster,"-",row_number()))

save(rb_cluster_data,
     file = "/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/rb_cluster_data.Rda")


rb_single_obs_per_cluster <-
  cluster_data3 %>% group_by(new_cluster) %>% slice(1) %>% rename(cluster = new_cluster) %>% select(cluster,avg_ppg,avg_cost)
rb_single_obs_per_cluster

save(rb_single_obs_per_cluster,
     file = "/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/rb_single_obs_per_cluster.Rda")



