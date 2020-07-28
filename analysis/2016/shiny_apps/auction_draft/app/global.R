library(shiny)
library(dplyr)
library(lpSolve)

 full_data2 <- read.csv("/home/john/stats_corner/2016/shiny_apps/auction_draft/app/data2017test.csv",
                        stringsAsFactors = F,
                        header = T)

full_data2 <- full_data2 %>%
  filter(is.na(ppg) == F)


library(ggplot2)

p1 <- ggplot(data = full_data2,
             aes(x = pred_cost,
                 y = ppg))
p1 + 
  geom_point() +
  geom_smooth(method = "lm", color = 'blue') +
  facet_wrap(~pos) +
  geom_text(aes(label = player)) +
  scale_x_reverse()

