## load the tidyverse
library(tidyverse)
library(rpart)
## Load the data
load(file="/home/john/projects/fantasy-football/data/clustering-data/cost_adp_prod_cleaned.Rda")

qb_data <- 
  cost_adp_prod_cleaned %>%
  filter(position == 'QB',
         pos_adp < 32,
         year >= 2015)

ggplot(data = qb_data,
       aes(x = pos_adp, y = adj_value)) +
  geom_point() +
  geom_smooth(span=0.50)

ggplot(data = qb_data,
       aes(x = pos_adp, y = ppg)) +
  geom_point() +
  geom_smooth(span=0.75)

qb_smoothed_data <- data.frame(pos_adp = c(1:32))

qb_smoothed_data$smoothed_cost <- predict(object = loess(data = qb_data, 
                                                         adj_value~ pos_adp,
                                                         span=0.25),
                                          newdata = qb_smoothed_data)

qb_smoothed_data$smoothed_ppg <- predict(object = loess(data = qb_data,
                                                        ppg ~ pos_adp,
                                                        span=0.75),
                                                newdata = qb_smoothed_data)
qb_smoothed_data

# remove NA and one negative value for cost
qb_smoothed_data <- qb_smoothed_data[-c(26:32),]
qb_smoothed_data


## center and scale 
qb_smoothed_data_to_scale <- qb_smoothed_data[,c(2,3)]
qb_smoothed_data_to_scale_scaled <- as.data.frame(scale(qb_smoothed_data_to_scale))
colnames(qb_smoothed_data_to_scale_scaled) <- c('smoothed_cost_scaled','smoothed_ppg_scaled')
qb_smoothed_data_to_scale_scaled  <- bind_cols(qb_smoothed_data,qb_smoothed_data_to_scale_scaled)
qb_smoothed_data_to_scale_scaled


max_clusters <- 9
output <- matrix(0,max_clusters-1,3)
data <- qb_smoothed_data_to_scale_scaled

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

# 7,8,9

## Start with 7
cluster_data <- data
cluster_object <- kmeans(data[,c(4,5)],
                         centers = 7,
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

cluster_data3

## Try 9
cluster_data <- data
cluster_object <- kmeans(data[,c(4,5)],
                         centers = 9,
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

View(cluster_data3)

## Finally, 8
cluster_data <- data
cluster_object <- kmeans(data[,c(4,5)],
                         centers = 8,
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

View(cluster_data3)


## Go with 7
cluster_data <- data
cluster_object <- kmeans(data[,c(4,5)],
                         centers = 7,
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
qb_cluster_data <-
cluster_data3 %>%
  ungroup() %>%
  select(new_cluster,avg_ppg,avg_cost) %>%
  group_by(new_cluster) %>%
  mutate(player = paste0("QB-",new_cluster,"-",row_number()))


save(qb_cluster_data,
     file = "/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/qb_cluster_data_last5.Rda")

qb_single_obs_per_cluster <-
  cluster_data3 %>% group_by(new_cluster) %>% slice(1) %>% rename(cluster = new_cluster) %>% select(cluster,avg_ppg,avg_cost)

save(qb_single_obs_per_cluster,
     file = "/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/qb_single_obs_per_cluster_last5.Rda")
