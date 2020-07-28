library(xgboost)
library(magrittr)
library(tidyverse)
load("/home/john/stats_corner/2017/predict-cost-model/training_data2017.Rda")

training_data %<>%
  left_join(.,
  training_data %>%
    filter(keeper == 1) %>%
    group_by(year,pos) %>%
    summarise(total_kept = n()) %>%
    spread(key = pos,value=total_kept) %>%
    rename(HC_kept = HC,
           QB_kept = QB,
           RB_kept = RB,
           TE_kept = TE,
          WR_kept = WR),
  by = "year")

# Remove Keepers and  Values > 200
training_data %<>% filter(keeper == 0, adj_value < 200)

# Going to k-fold cv with k = 5
# Reserve 20% of data for generating test errors
set.seed(2017)
train <- sample(c(0,1),1218,replace=TRUE,prob=c(0.2,0.8))
train_df <- training_data[train==1,] 
test_df <- training_data[train==0,]

train_df %<>% select(-year,-owner,-pick,-player,-player_code,-team,-keeper)
with(train_df,table(pos))
train_df %<>% mutate(pos = factor(pos,levels = c("D/ST","HC","K","QB","RB","TE","WR")))


test_df %<>% select(-year,-owner,-pick,-player,-player_code,-team,-keeper)

test_df %<>% mutate(pos = factor(pos,levels = c("D/ST","HC","K","QB","RB","TE","WR")))

str(train_df)

dmatrix <- xgb.DMatrix(data=data.matrix(train_df[,-2]),label=train_df$adj_value)

colnames(train_df)
data.matrix(train_df[,-2])
dmatrix

# Grid Search for optimal model
iterations <- c(100,200,500,1000,5000,10000) #6
etas <- c(0.001,0.01,0.05,0.1,0.25,0.5) #6
min_child_weights <- c(1,5,10) #3
max_depths <- c(1,2,3,4,5,6,8,10,15,20) #10
gammas <- c(0,1) #2
colsample_bytrees <- c(0.5,1) #2

results_matrix <- matrix(0,nrow=4320,ncol=8)
iteration <- 1
sink("/home/john/stats_corner/2017/predict-cost-model/output.txt")
for(i in 1:6){
  for(j in 1:6){
    for(k in 1:3){
      for(l in 1:10){
        for(m in 1:2){
          for(n in 1:2){
            print(iteration)
            set.seed(12)
            model <- xgb.cv(data = dmatrix,
                            nrounds = iterations[i],
                            eta = etas[j],
                            min_child_weight = min_child_weights[k],
                            max_depth = max_depths[l],
                            gamma = gammas[m],
                            colsample_bytree = colsample_bytrees[n],
                            nfold = 5,
                            verbose = FALSE)
            results_matrix[iteration,1] <- iteration
            results_matrix[iteration,2] <- iterations[i]
            results_matrix[iteration,3] <- etas[j]
            results_matrix[iteration,4] <- min_child_weights[k]
            results_matrix[iteration,5] <- max_depths[l]
            results_matrix[iteration,6] <- gammas[m]
            results_matrix[iteration,7] <- colsample_bytrees[n]
            results_matrix[iteration,8] <- model$evaluation_log$test_rmse_mean[model$niter]
            save(results_matrix,file = "/home/john/stats_corner/2017/predict-cost-model/results_matrix.Rda")
            print(model$evaluation_log$test_rmse_mean[model$niter])
            print(min(results_matrix[which(results_matrix[,8]>0),8]))
            iteration <- iteration +1
            }
        }
      }
    }
  }
}

results_df <- data.frame(results_matrix)
colnames(results_df) <- c("iteration","iterations","eta","min_child_weight","max_depth","gamma","colsample_bytree","rmse")
results_df %<>% filter(rmse != 0)



model2017 <- xgb.train(data = dmatrix,
                nrounds = 200,
                eta = .05,
                min_child_weight = 5,
                max_depth = 4,
                gamma = 1,
                colsample_bytree = 0.5)


cbind(train_df$adj_value,predict(final,newdata =dmatrix))


xgb.importance(model = final,feature_names = colnames(dmatrix))


save(model2017,file="/home/john/stats_corner/2017/predict-cost-model/model2017.Rda")

# evaluate on test data
str(test_df)
test_dmatrix <- data.matrix(test_df[,-2])

predictions <- predict(object = model2017,
                       newdata=test_dmatrix)

test_df$pred <- predictions

test_df %>%
  mutate(error = (pred-adj_value),
        sq_error = (pred-adj_value)**2) %>%
  group_by(pos) %>%
  summarise(rmse = sqrt(mean(sq_error)),
            min_e = min(error),
            max_e = max(error),
            p05 <- quantile(error,.05),
            p95 <- quantile(error,.95),
            p25 <- quantile(error,0.25),
            p75 <- quantile(error,0.75)) %>%
  View()

plot1 <- ggplot(data = test_df %>% mutate(error = pred-adj_value),
                aes(x=overall,y=error,color=pos))
plot1 +
  geom_point() +
  geom_smooth()
