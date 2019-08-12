## load the tidyverse
library(tidyverse)
library(rpart)
## Load the data
load("/home/john/projects/fantasy-football/data/clustering-data/cost_adp_prod_scaled_cleaned.Rda")
## Filter on position
data <- cost_adp_prod_scaled_cleaned %>% 
  filter(pos == 'RB') %>%
  filter(pos_adp <= 36)

max_clusters <- 9

output <- matrix(0,max_clusters-1,3)

for(i in c(2:max_clusters)){
  cluster_data <- data
  cluster_object <- kmeans(data[,10:12],
                           centers = i,
                           nstart = 20)
  
  cluster_data$cluster <- cluster_object$cluster
  var_explained <- cluster_object$betweenss/cluster_object$totss
  
  cluster_data2 <-
    cluster_data %>%
    group_by(cluster) %>%
    summarise(avg_ppg = mean(ppg),
              avg_ppg_sd = mean(ppg_sd),
              avg_cost = mean(adj_value),
              sd_adp = sd(pos_adp),
              total_obs = n()) %>%
    arrange(desc(avg_ppg)) %>%
    mutate(new_cluster = row_number())
  
  cluster_data3 <-
    cluster_data %>%
    left_join(.,cluster_data2, by = "cluster") %>%
    group_by(new_cluster,pos_adp,avg_ppg) %>%
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


# rm(data)

output_df <- data.frame(num_clusters = output[,1],
                        var_explained = output[,2],
                        adp_explained = output[,3])
ggplot(data = output_df,
       aes(x = var_explained,
           y = adp_explained,
           label = num_clusters)) +
  geom_point() +
  geom_label()


## Looks like 7 is the way to go...

cluster_data <- data
cluster_object <- kmeans(data[,10:12],
                         centers = 7,
                         nstart = 20)

cluster_data$cluster <- cluster_object$cluster
var_explained <- cluster_object$betweenss/cluster_object$totss
var_explained
cluster_data2 <-
  cluster_data %>%
  group_by(cluster) %>%
  summarise(avg_ppg = mean(ppg),
            avg_ppg_sd = mean(ppg_sd),
            avg_cost = mean(adj_value),
            sd_adp = sd(pos_adp),
            total_obs = n()) %>%
  arrange(desc(avg_ppg)) %>%
  mutate(new_cluster = row_number())

cluster_data3 <-
  cluster_data %>%
  left_join(.,cluster_data2, by = "cluster") %>%
  group_by(new_cluster,pos_adp,avg_ppg) %>%
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

cluster_data4 %>%
  group_by(pos_adp) %>%
  arrange(desc(pct_of_adp)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(new_cluster,pos_adp) %>%
  View()
