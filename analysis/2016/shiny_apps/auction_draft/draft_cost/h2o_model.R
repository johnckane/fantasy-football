library(h2o)
h2o.init(nthreads = -1)
response <- "value"
predictors <- setdiff(colnames(train_adp_value),y)
predictors <- predictors[c(3,5,7,8,9,10,11,12,13,14,15,16,17,18)]
predictors
# Construct a large Cartesian hyper-parameter space
ntrees_opts = c(100,200,500,1000)       # early stopping will stop earlier
max_depth_opts = c(1:10)
learn_rate_opts = seq(0.001,0.01,0.1)

hyper_params = list( ntrees = ntrees_opts, 
                     max_depth = max_depth_opts, 
                     learn_rate = learn_rate_opts
)


# Search a random subset of these hyper-parmameters. Max runtime 
# and max models are enforced, and the search will stop after we 
# don't improve much over the best 5 random models.
search_criteria = list(strategy = "RandomDiscrete",
                       seed = 123456)

gbm_grid <- h2o.grid("gbm", 
                     grid_id = "mygrid",
                     x = predictors, 
                     y = response, 
                     nfolds = 10,
                     distribution="gaussian",
                     seed = 123456,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

gbm_sorted_grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "mse")
print(gbm_sorted_grid)

best_model <- h2o.getModel(gbm_sorted_grid@model_ids[[1]])
summary(best_model)