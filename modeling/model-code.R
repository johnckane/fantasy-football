library(h2o)
library(tidyverse)
h2o.init(nthreads=-1)
# load("/home/john/fantasy-football/modeling/model_data.Rda")
# write_csv(model_data,"/home/john/fantasy-football/modeling/model_data.csv")


df <- h2o.importFile(path = "/home/john/fantasy-football/modeling/model_data.csv")
# dim(df)
# head(df)
# tail(df)
# summary(df,exact_quantiles=TRUE)

response <- "adj_value"

# colnames(df)
predictors <- colnames(df)[c(3,5:41)]


## Split into train, test
## Going to hold out about ~ 200 datapoints for testing, a little more than one draft's worth

splits <- h2o.splitFrame(
  data = df, 
  ratios = c(1176/1376),
  destination_frames = c("train.hex", "test.hex"), 
  seed = 1234
)

train <- splits[[1]]
test  <- splits[[2]]

# 
# xgb <- h2o.xgboost(x = predictors, 
#                y = response, 
#                training_frame = train,
#                model_id = "baseline",
#                nfolds = 5,
#                keep_cross_validation_predictions = TRUE,
#                keep_cross_validation_fold_assignment = TRUE,
#                seed = 2018,
#                distribution = "gaussian")
# 
# 
# 
# ## Show a detailed model summary
# xgb
# 
# ## 
# h2o.varimp_plot(xgb)


# baseline rmse = 4.42


hyper_params <- list(ntrees = seq(10, 1000, 1),
                     learn_rate = seq(0.0001, 0.2, 0.0001),
                     max_depth = seq(1, 20, 1),
                     sample_rate = seq(0.5, 1.0, 0.0001),
                     col_sample_rate = seq(0.2, 1.0, 0.0001))
search_criteria <- list(strategy = "RandomDiscrete",
                        max_runtime_secs = 3600, 
                        seed = 1)

# Train the grid
sink("/home/john/fantasy-football/modeling/xgb-grid-output.txt")
xgb_grid <- h2o.grid(algorithm = "xgboost",
                     x = predictors, 
                     y = response,
                     training_frame = train,
                     nfolds = 5,
                     seed = 1,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)


# Sort the grid by CV AUC
grid <- h2o.getGrid(grid_id = xgb_grid@grid_id, sort_by = "mse", decreasing = FALSE)
grid_top_model <- grid@summary_table[1, "model_ids"]

print(grid)
print(grid_top_model)

model_path <- h2o.saveModel(object=grid_top_model, path='/home/john/fantasy-football/modeling/', force=TRUE)

sink()

## Based on grid results we're going to narrow our focus to 
## Best RMSE from grid is: 7.98
## Best MAE from grid is: 5.28

hyper_params2 <- list(ntrees = c(300,400,500,600),
                     learn_rate = c(0.01,0.02,0.03,0.04,0.05),
                     max_depth = c(4,8,16),
                     sample_rate = c(0.5,0.6,0.7),
                     col_sample_rate = c(0.30,0.40,0.50,0.60))

search_criteria2 <- list(strategy = "RandomDiscrete",
                        max_runtime_secs = 1800, 
                        seed = 1)


xgb_grid2 <- h2o.grid(algorithm = "xgboost",
                      x = predictors,
                      y = response,
                      training_frame = train,
                      nfolds = 5,
                      seed = 1,
                      hyper_params = hyper_params2,
                      search_criteria = search_criteria2)


grid <- h2o.getGrid(grid_id = xgb_grid2@grid_id, sort_by = "mse", decreasing = FALSE)

grid_top_model <- grid@summary_table[1, "model_ids"]

model_path <- h2o.saveModel(object=h2o.getModel(grid_top_model), 
                            path='/home/john/fantasy-football/modeling/', 
                            force=TRUE)
h2o.shutdown()
