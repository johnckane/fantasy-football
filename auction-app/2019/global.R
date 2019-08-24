library(shiny)
library(tidyverse)
library(lpSolve)


data <- read_csv("/home/john/fantasy-football/modeling/prediction_data_post.csv")




# data <- 
#   data %>%
#   mutate(pred_cost = case_when(player == 'Jared Goff' ~ 12,
#                                player == 'LeSean McCoy' ~ 82,
#                                player == 'LaMar Miller' ~ 60,
#                                player == 'DeMaryius Thomas' ~ 41,
#                                player == 'Devin Funchess' ~ 7,
#                                player == 'Cooper Kupp' ~ 7,
#                                player == 'Greg Olsen' ~ 41,
#                                player == 'Derek Carr' ~ 40,
#                                player == 'Ravens D/ST' ~ 7,
#                                player == 'Matt Bryant' ~ 11,
#                                player == 'T.Y. Hilton' ~ 55,
#                                player == 'Trevor Siemian' ~ 12,
#                                player == 'Dalvin Cook' ~ 44,
#                                TRUE ~ as.numeric(pred_cost)))





data <- data %>% mutate(ppg = round(proj_ppg,1)) %>% filter(is.na(ppg) == F)

