library(h2o)
library(tidyverse)
library(magrittr)
h2o.init(nthreads=-1)



df <- h2o.importFile(path = "/home/john/fantasy-football/modeling/model_data.csv")
# dim(df)
# head(df)
# tail(df)
# summary(df,exact_quantiles=TRUE)

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
## The $209 Andre Johnson is in test, so it won't screw up the model training. 



model <- h2o.loadModel('/home/john/fantasy-football/modeling/Grid_XGBoost_train.hex_model_R_1534618752362_1_model_27')

summary(model)

## Validation Summary:
#MSE:  63.26612
#RMSE:  7.954001
#MAE:  5.291917

##
?h2o.performance
h2o.performance(model = model, newdata = test)

test_df <- as.data.frame(test)
pred <- h2o.predict(object = model, newdata = test)
str(pred)
pred$predict
test_df$pred_cost <- as.vector(pred$predict)
View(test_df)

test_df$resid <- test_df$pred_cost - test_df$adj_value

ggplot(data = test_df %>% filter(pos != 'HC'),
       aes(x = pred_cost,y = adj_value, colour = pos)) +
  geom_point() +
  geom_smooth()

ggplot(data = test_df %>% filter(pos != 'HC'),
       aes(x = pred_cost, y = resid, colour = pos)) +
  geom_point() +
  geom_smooth()

test_df %>%
  group_by(pos) %>%
  summarise(min = min(resid),
            max = max(resid),
            avg = mean(resid),
            p50 = median(resid),
            p25 = quantile(resid,.25),
            p75 = quantile(resid,.75))

test_df %>%
  filter(pred_cost > 25) %>%
  group_by(pos) %>%
  summarise(min = min(resid),
            max = max(resid),
            avg = mean(resid),
            p10 = quantile(resid,.10),
            p25 = quantile(resid,.25),
            p50 = median(resid),
            p75 = quantile(resid,.75),
            p90 = quantile(resid,.90))


# pos      min   max   avg   p10      p25   p50   p75   p90
# <fct>  <dbl> <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>
#   1 QB     -26.1  22.5 -5.39 -22.9 -15.7    -7.52  2.28 15.5 
# 2 RB     -31.0  10.2 -5.65 -27.5 -14.1    -2.96  6.96  8.30
# 3 WR    -128.   24.1 -3.61 -11.1  -0.0805  3.91  8.59 12.4 

test_df %>%
  filter(pred_cost <= 25) %>%
  group_by(pos) %>%
  summarise(min = min(resid),
            max = max(resid),
            avg = mean(resid),
            p10 = quantile(resid,.10),
            p25 = quantile(resid,.25),
            p50 = median(resid),
            p75 = quantile(resid,.75),
            p90 = quantile(resid,.90))


# A tibble: 7 x 9
# pos       min   max     avg    p10     p25    p50     p75   p90
# <fct>   <dbl> <dbl>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl> <dbl>
#   1 D/ST   -5.29   2.04  0.308  -1.30   0.0260  0.652  1.58    1.98
# 2 HC     -0.640  4.25  1.48   -0.608  0.444   1.97   1.99    2.22
# 3 K      -5.67   1.83  0.0990 -0.653 -0.222   0.441  1.06    1.45
# 4 QB     -8.25  11.7  -2.45   -7.03  -6.52   -3.09  -0.0555  3.09
# 5 RB     -5.65  18.0   3.07   -2.95  -0.742   2.11   6.10   11.9 
# 6 TE    -14.4    7.57 -1.36   -9.37  -5.92    0.109  3.22    5.13
# 7 WR    -29.5    8.58 -1.58   -9.84  -2.94    0.420  2.81    5.68