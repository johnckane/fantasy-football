install.packages("gbm")
library(gbm)
library(dplyr)
colnames(train_adp_value)

train_data <-train_adp_value %>% select(3,5,6,8,9,10,11,12,13,14,15,16,17,18)

str(train_data)
train_data$player <- as.factor(train_data$player)
train_data$pos <- as.factor(train_data$pos)

trees <- c(1000,2000,5000,10000)
depths <- c(1,2,4,8,16)
shrinks <- c(0.0001,.001,0.01,0.1)
models <- list()
iterator <- 1
set.seed(2016)
for(i in 1:4){
  for(j in 1:5) {
    for(k in 1:4){
      models[[iterator]] <- 
        gbm(value ~ .,
            data = train_data,
            distribution = "gaussian",
            n.trees = trees[i],
            interaction.depth = depths[j],
            shrinkage = shrinks[k],
            bag.fraction = 0.5,
            train.fraction = 1.0,
            cv.folds = 10,
            keep.data = TRUE,
            verbose = "CV")
      iterator <- iterator + 1
    }
  }
}
####

str(models[[1]])


cv_errors <- lapply(models, '[['])
?lapply

# squared error
# for each model, calculate root mean square error 


models[[1]]$Terms

which(models[[2]]$cv.error == min(models[[2]]$cv.error)) 

rmse <- rep(0,80)
for(h in 1:80){
  rmse[h] <- sqrt(models[[h]]$cv.error[which(models[[h]]$cv.error == min(models[[h]]$cv.error))])
}
rmse
h = 1

which(rmse == min(rmse))

models[[70]]

models[[70]]$n.trees
models[[70]]$interaction.depth
models[[70]]$shrinkage



models[[h]]$cv.error[which(models[[h]]$cv.error = min(models[[h]]$cv.error))]
set.seed(2016)


test_data <- test_adp_value %>% select(3,5,6,8,9,10,11,12,13,14,15,16,17,18)

test_data$player <- as.factor(test_data$player)
test_data$pos <- as.factor(test_data$pos)

p <- predict(models[[70]], newdata = test_data)
sqrt(mean((p - test_data$value)**2))

# break out by position
df <- data.frame(p,v = test_data$value,pos = as.character(test_data$pos), stringsAsFactors = FALSE)
str(df)
library(dplyr)
detach(package:plyr)
df %>%
  group_by(pos) %>%
  summarise(rmse = sqrt(mean((p-v)**2)))

### QBs

train_data <-train_adp_value %>% 
  select(3,5,6,8,9,10,11,12,13,14,15,16,17,18) %>% 
  filter(pos == "QB" ) %>%
  select(-pos,-player)
#train_data$player <- as.factor(train_data$player)
trees <- c(1000,2000,5000,10000)
depths <- c(1,2,4,8,16)
shrinks <- c(0.0001,.001,0.01,0.1)
models_qb <- list()
iterator <- 1
set.seed(2016)
for(i in 1:4){
  for(j in 1:5) {
    for(k in 1:4){
      models_qb[[iterator]] <- 
        gbm(value ~ .,
            data = train_data,
            distribution = "gaussian",
            n.trees = trees[i],
            interaction.depth = depths[j],
            shrinkage = shrinks[k],
            bag.fraction = 0.5,
            train.fraction = 1.0,
            cv.folds = 10,
            keep.data = TRUE,
            verbose = "CV")
      iterator <- iterator + 1
    }
  }
}
####

rmse <- rep(0,80)
for(h in 1:80){
  rmse[h] <- sqrt(models_qb[[h]]$cv.error[which(models_qb[[h]]$cv.error == min(models_qb[[h]]$cv.error))])
}
rmse


which(rmse == min(rmse))

models_qb[[40]]$cv.error[which(models_qb[[40]]$cv.error == min(models_qb[[40]]$cv.error))] %>% sqrt()



test_data <- test_adp_value %>% select(3,5,6,8,9,10,11,12,13,14,15,16,17,18) %>% filter(pos == "QB")

test_data$player <- as.factor(test_data$player)
test_data$pos <- as.factor(test_data$pos)

p <- predict(models_qb[[11]], newdata = test_data)
sqrt(mean((p - test_data$value)**2))

# break out by position
df <- data.frame(p,v = test_data$value,pos = as.character(test_data$pos), stringsAsFactors = FALSE)
str(df)
library(dplyr)
detach(package:plyr)
df %>%
  group_by(pos) %>%
  summarise(rmse = sqrt(mean((p-v)**2)))


#### rb
train_data <-train_adp_value %>% 
  select(3,5,6,8,9,10,11,12,13,14,15,16,17,18) %>% 
  filter(pos == "RB" ) %>%
  select(-pos,-player)
#train_data$player <- as.factor(train_data$player)
trees <- c(1000,2000,5000,10000)
depths <- c(1,2,4,8,16)
shrinks <- c(0.0001,.001,0.01,0.1)
models_rb <- list()
iterator <- 1
set.seed(2016)
for(i in 1:4){
  for(j in 1:5) {
    for(k in 1:4){
      models_rb[[iterator]] <- 
        gbm(value ~ .,
            data = train_data,
            distribution = "gaussian",
            n.trees = trees[i],
            interaction.depth = depths[j],
            shrinkage = shrinks[k],
            bag.fraction = 0.5,
            train.fraction = 1.0,
            cv.folds = 10,
            keep.data = TRUE,
            verbose = "CV")
      iterator <- iterator + 1
    }
  }
}
####

rmse_rb <- rep(0,80)
for(h in 1:80){
  rmse_rb[h] <- sqrt(models_rb[[h]]$cv.error[which(models_rb[[h]]$cv.error == min(models_rb[[h]]$cv.error))])
}
rmse


which(rmse_rb == min(rmse_rb))

#models_qb[[40]]$cv.error[which(models_qb[[40]]$cv.error == min(models_qb[[40]]$cv.error))] %>% sqrt()
#test_data <- test_adp_value %>% select(3,5,6,8,9,10,11,12,13,14,15,16,17,18) %>% filter(pos == "QB")
#test_data$player <- as.factor(test_data$player)
#test_data$pos <- as.factor(test_data$pos)
#p <- predict(models_qb[[11]], newdata = test_data)
#sqrt(mean((p - test_data$value)**2))

#### wr
train_data <-train_adp_value %>% 
  select(3,5,6,8,9,10,11,12,13,14,15,16,17,18) %>% 
  filter(pos == "WR" ) %>%
  select(-pos,-player)
#train_data$player <- as.factor(train_data$player)
trees <- c(1000,2000,5000,10000)
depths <- c(1,2,4,8,16)
shrinks <- c(0.0001,.001,0.01,0.1)
models_wr <- list()
iterator <- 1
set.seed(2016)
for(i in 1:4){
  for(j in 1:5) {
    for(k in 1:4){
      models_wr[[iterator]] <- 
        gbm(value ~ .,
            data = train_data,
            distribution = "gaussian",
            n.trees = trees[i],
            interaction.depth = depths[j],
            shrinkage = shrinks[k],
            bag.fraction = 0.5,
            train.fraction = 1.0,
            cv.folds = 10,
            keep.data = TRUE,
            verbose = "CV")
      iterator <- iterator + 1
    }
  }
}
####

rmse_wr <- rep(0,80)
for(h in 1:80){
  rmse_wr[h] <- sqrt(models_wr[[h]]$cv.error[which(models_wr[[h]]$cv.error == min(models_wr[[h]]$cv.error))])
}
rmse_wr


which(rmse_wr == min(rmse_wr))


#### TE
train_data <-train_adp_value %>% 
  select(3,5,6,8,9,10,11,12,13,14,15,16,17,18) %>% 
  filter(pos == "TE" ) %>%
  select(-pos,-player)
#train_data$player <- as.factor(train_data$player)
trees <- c(1000,2000,5000,10000)
depths <- c(1,2,4,8,16)
shrinks <- c(0.0001,.001,0.01,0.1)
models_te <- list()
iterator <- 1
set.seed(2016)
for(i in 1:4){
  for(j in 1:5) {
    for(k in 1:4){
      models_te[[iterator]] <- 
        gbm(value ~ .,
            data = train_data,
            distribution = "gaussian",
            n.trees = trees[i],
            interaction.depth = depths[j],
            shrinkage = shrinks[k],
            bag.fraction = 0.5,
            train.fraction = 1.0,
            cv.folds = 10,
            keep.data = TRUE,
            verbose = "CV")
      iterator <- iterator + 1
    }
  }
}
####

rmse_te <- rep(0,80)
for(h in 1:80){
  rmse_te[h] <- sqrt(models_te[[h]]$cv.error[which(models_te[[h]]$cv.error == min(models_te[[h]]$cv.error))])
}
rmse_te


which(rmse_te == min(rmse_te))

