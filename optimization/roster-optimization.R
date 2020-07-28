library(tidyverse)
load("/home/john/stats_corner/fantasy_football/points_week_pos_cost.Rda")

# remove keepers
no_keepers <- points_week_pos_cost %>% filter(keeper == 0)

#' Maximize weekly points
#' Constraints:
#' 1) No more than $300 dollars spent
#' 2) Draft 2 D/ST, 1 K, 3 QB, 4 WR, 3 RB, 2 TE

#' To play a player in any given week


season <- 2016
data <- filter(no_keepers, year == season) 

week1  <- filter(no_keepers, week == 1)
week2  <- filter(no_keepers, week == 2) 
week3  <- filter(no_keepers, week == 3)
week4  <- filter(no_keepers, week == 4)
week5  <- filter(no_keepers, week == 5)
week6  <- filter(no_keepers, week == 6)
week7  <- filter(no_keepers, week == 7)
week8  <- filter(no_keepers, week == 8)
week9  <- filter(no_keepers, week == 9)
week10 <- filter(no_keepers, week == 10)
week11 <- filter(no_keepers, week == 11)
week12 <- filter(no_keepers, week == 12)
week13 <- filter(no_keepers, week == 13)
week14 <- filter(no_keepers, week == 14)
week15 <- filter(no_keepers, week == 15)
week16 <- filter(no_keepers, week == 16)




c1 <- data$adj_value
c2 <- ifelse(full_data2$pos=='QB',1,0)
c3 <- ifelse(full_data2$pos=='RB',1,0)
c4 <- ifelse(full_data2$pos=='TE',1,0)
c5 <- ifelse(full_data2$pos=='WR',1,0)
c7 <- ifelse(full_data2$pos == 'D/ST',1,0)
c8 <- ifelse(full_data2$pos == 'K',1,0)


## Bye Week Constraints
## No more than 1 at each position on BYE
table(full_data2$bye)

bye_w5  <- ifelse(full_data2$bye == 5,1,0)
bye_w6  <- ifelse(full_data2$bye == 5,1,0)
bye_w7  <- ifelse(full_data2$bye == 5,1,0)
bye_w8  <- ifelse(full_data2$bye == 5,1,0)
bye_w9  <- ifelse(full_data2$bye == 5,1,0)
bye_w10 <- ifelse(full_data2$bye == 5,1,0)
bye_w11 <- ifelse(full_data2$bye == 5,1,0)


c9 <- bye_w5*c2 
c10 <- bye_w6*c2
c11 <- bye_w7*c2 
c12 <- bye_w8*c2
c13 <- bye_w9*c2 
c14 <- bye_w10*c2
c15 <- bye_w11*c2

c16 <- bye_w5*c3 
c17 <- bye_w6*c3
c18 <- bye_w7*c3 
c19 <- bye_w8*c3
c20 <- bye_w9*c3 
c21 <- bye_w10*c3
c22 <- bye_w11*c3

c23 <- bye_w5*c4 
c24 <- bye_w6*c4
c25 <- bye_w7*c4 
c26 <- bye_w8*c4
c27 <- bye_w9*c4 
c28 <- bye_w10*c4
c29 <- bye_w11*c4

c30 <- bye_w5*c5 
c31 <- bye_w6*c5
c32 <- bye_w7*c5 
c33 <- bye_w8*c5
c34 <- bye_w9*c5 
c35 <- bye_w10*c5
c36 <- bye_w11*c5

c37 <- bye_w5*c7 
c38 <- bye_w6*c7
c39 <- bye_w7*c7 
c40 <- bye_w8*c7
c41 <- bye_w9*c7 
c42 <- bye_w10*c7
c43 <- bye_w11*c7

c44 <- bye_w5*c8 
c45 <- bye_w6*c8
c46 <- bye_w7*c8 
c47 <- bye_w8*c8
c48 <- bye_w9*c8 
c49 <- bye_w10*c8
c50 <- bye_w11*c8



## Age Constraints
age30 <- ifelse(full_data2$age > 30,1,0)

c51 <- age30*c3
c52 <- age30*c5

gt10 <- ifelse(full_data2$pred_cost > 5,1,0)

qb_rb_wr <- ifelse(c2|c3|c5,1,0)

c54 <- gt10*qb_rb_wr


## Total BYE constraints
c55 <- bye_w5*(c2+c3+c4+c5+c6+c7+c8) 
c56 <- bye_w6*(c2+c3+c4+c5+c6+c7+c8) 
c57 <- bye_w7*(c2+c3+c4+c5+c6+c7+c8)  
c58 <- bye_w8*(c2+c3+c4+c5+c6+c7+c8) 
c59 <- bye_w9*(c2+c3+c4+c5+c6+c7+c8)  
c60 <- bye_w10*(c2+c3+c4+c5+c6+c7+c8) 
c61 <- bye_w11*(c2+c3+c4+c5+c6+c7+c8) 