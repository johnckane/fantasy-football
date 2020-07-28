library(gbm)
library(dplyr)
colnames(train_adp_value)

train_data <-train_adp_value %>% select(5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)


str(train_data)
#train_data$player <- as.factor(train_data$player)
train_data$pos <- as.factor(train_data$pos)
train_data <- train_data %>%
  filter(pos %in% c("QB","RB","WR","TE"))

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

rmse <- rep(0,80)
for(h in 1:80){
  rmse[h] <- sqrt(models[[h]]$cv.error[which(models[[h]]$cv.error == min(models[[h]]$cv.error))])
}
rmse

which(rmse == min(rmse))

models[[70]]

models[[70]]$n.trees
models[[70]]$interaction.depth
models[[70]]$shrinkage


models[[70]]$cv.error[which(models[[70]]$cv.error == min(models[[70]]$cv.error))]
sqrt(75)

colnames(test_adp_value)
test_data <- test_adp_value %>% 
  select(3,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24) %>%
  filter(pos %in% c("QB","RB","WR","TE"))

test_data$player <- as.factor(test_data$player)
test_data$pos <- as.factor(test_data$pos)

p <- predict(models[[70]], newdata = test_data)
sqrt(mean((p - test_data$value)**2))

summary(models[[70]])

# break out by position
df <- data.frame(p,v = test_data$value,pos = as.character(test_data$pos), stringsAsFactors = FALSE)
str(df)
library(dplyr)
detach(package:plyr)
df %>%
  group_by(pos) %>%
  summarise(rmse = sqrt(mean((p-v)**2)))


final_model <- models[[70]]

save(final_model,file = "/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/final_model.R")

#This is the winner!





