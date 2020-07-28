library(h2o)
library(tidyverse)
library(magrittr)
h2o.init(nthreads=-1)
# load("/home/john/fantasy-football/modeling/model_data.Rda")
# write_csv(model_data,"/home/john/fantasy-football/modeling/model_data.csv")

predict_data_pre <- read_csv("/home/john/fantasy-football/modeling/prediction_data_pre.csv")
predict_data_pre %<>% filter(pos != "HC")
predict_h2o_frame <- as.h2o(predict_data_pre)

model <- h2o.loadModel('/home/john/fantasy-football/modeling/Grid_XGBoost_train.hex_model_R_1534618752362_1_model_27')

pred <- h2o.predict(model, newdata = predict_h2o_frame)
str(pred)
pred$predict
predict_data_pre$pred_cost <- round(as.vector(pred$predict))
predict_data_pre %<>% mutate(pred_cost = ifelse(pred_cost < 1, 1,pred_cost))

head(predict_data_pre)

predict_data_post <-
predict_data_pre %>%
  select(player,pos,pred_cost,proj_ppg,age,bye,team)


write_csv(predict_data_post,"/home/john/fantasy-football/modeling/prediction_data_post.csv")
