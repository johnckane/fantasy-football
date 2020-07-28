adp_value <- read.csv("/home/john/stats_corner/2016/shiny_apps/auction_draft/draft_cost/adp_draft_data.csv",
                      stringsAsFactors = FALSE,
                      header = TRUE)

set.seed(2016)
train <- sample(c(1,0),dim(adp_value)[1],c(0.9,0.1),replace = TRUE)

adp_value$train <- train

prop.table(table(adp_value$pos,adp_value$train),1)

#' As a benchmark let's do loess regression again 

train_adp_value <- filter(adp_value, train == 1)
test_adp_value <- filter(adp_value, train == 0)

#' Do this by position...

spans = c(0.25,0.50,0.75,1.0,2.0,3.0)

f <- function(p,sp){
  df <- adp_value %>% filter(pos == p)
  return(loess(data=df, value~pos_rank, span = sp))
}
#####################################
## QB ##
qb_25  <- f("QB", 0.25)
qb_50  <- f("QB", 0.50)
qb_75  <- f("QB", 0.75)
qb_100 <- f("QB", 1.00)
qb_200 <- f("QB", 2.00)
qb_300 <- f("QB", 3.00)

## Predict on test
qb_preds    <- list()
qb_rmse     <- vector()

qb_preds[[1]] <- (predict(qb_25, newdata = test_adp_value[test_adp_value$pos == "QB",]) - test_adp_value$value[test_adp_value$pos == "QB"])
qb_rmse[1] <- sqrt(mean(qb_preds[[1]]**2, na.rm = TRUE))

qb_preds[[2]] <- (predict(qb_50, newdata = test_adp_value[test_adp_value$pos == "QB",]) - test_adp_value$value[test_adp_value$pos == "QB"])
qb_rmse[2] <- sqrt(mean(qb_preds[[2]]**2, na.rm = TRUE))

qb_preds[[3]] <- (predict(qb_75, newdata = test_adp_value[test_adp_value$pos == "QB",]) - test_adp_value$value[test_adp_value$pos == "QB"])
qb_rmse[3] <- sqrt(mean(qb_preds[[3]]**2, na.rm = TRUE))

qb_preds[[4]] <- (predict(qb_100, newdata = test_adp_value[test_adp_value$pos == "QB",]) - test_adp_value$value[test_adp_value$pos == "QB"])
qb_rmse[4] <- sqrt(mean(qb_preds[[4]]**2, na.rm = TRUE))

qb_preds[[5]] <- (predict(qb_200, newdata = test_adp_value[test_adp_value$pos == "QB",]) - test_adp_value$value[test_adp_value$pos == "QB"])
qb_rmse[5] <- sqrt(mean(qb_preds[[5]]**2, na.rm = TRUE))

qb_preds[[6]] <- (predict(qb_300, newdata = test_adp_value[test_adp_value$pos == "QB",]) - test_adp_value$value[test_adp_value$pos == "QB"])
qb_rmse[6] <- sqrt(mean(qb_preds[[6]]**2, na.rm = TRUE))
qb_rmse

# span of 0.25

#####################################
## RB ##
rb_25  <- f("RB", 0.25)
rb_50  <- f("RB", 0.50)
rb_75  <- f("RB", 0.75)
rb_100 <- f("RB", 1.00)
rb_200 <- f("RB", 2.00)
rb_300 <- f("RB", 3.00)

## Predict on test
rb_preds    <- list()
rb_rmse     <- vector()

rb_preds[[1]] <- (predict(rb_25, newdata = test_adp_value[test_adp_value$pos == "RB",]) - test_adp_value$value[test_adp_value$pos == "RB"])
rb_rmse[1] <- sqrt(mean(rb_preds[[1]]**2, na.rm = TRUE))

rb_preds[[2]] <- (predict(rb_50, newdata = test_adp_value[test_adp_value$pos == "RB",]) - test_adp_value$value[test_adp_value$pos == "RB"])
rb_rmse[2] <- sqrt(mean(rb_preds[[2]]**2, na.rm = TRUE))

rb_preds[[3]] <- (predict(rb_75, newdata = test_adp_value[test_adp_value$pos == "RB",]) - test_adp_value$value[test_adp_value$pos == "RB"])
rb_rmse[3] <- sqrt(mean(rb_preds[[3]]**2, na.rm = TRUE))

rb_preds[[4]] <- (predict(rb_100, newdata = test_adp_value[test_adp_value$pos == "RB",]) - test_adp_value$value[test_adp_value$pos == "RB"])
rb_rmse[4] <- sqrt(mean(rb_preds[[4]]**2, na.rm = TRUE))

rb_preds[[5]] <- (predict(rb_200, newdata = test_adp_value[test_adp_value$pos == "RB",]) - test_adp_value$value[test_adp_value$pos == "RB"])
rb_rmse[5] <- sqrt(mean(rb_preds[[5]]**2, na.rm = TRUE))

rb_preds[[6]] <- (predict(rb_300, newdata = test_adp_value[test_adp_value$pos == "RB",]) - test_adp_value$value[test_adp_value$pos == "RB"])
rb_rmse[6] <- sqrt(mean(rb_preds[[6]]**2, na.rm = TRUE))
rb_rmse
#0.25 wins again

#####################################
## WR ##
wr_25  <- f("TE", 0.25)
wr_50  <- f("TE", 0.50)
wr_75  <- f("TE", 0.75)
wr_100 <- f("TE", 1.00)
wr_200 <- f("TE", 2.00)
wr_300 <- f("TE", 3.00)

## Predict on test
wr_preds    <- list()
wr_rmse     <- vector()

wr_preds[[1]] <- (predict(wr_25, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
wr_rmse[1] <- sqrt(mean(wr_preds[[1]]**2, na.rm = TRUE))

wr_preds[[2]] <- (predict(wr_50, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
wr_rmse[2] <- sqrt(mean(wr_preds[[2]]**2, na.rm = TRUE))

wr_preds[[3]] <- (predict(wr_75, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
wr_rmse[3] <- sqrt(mean(wr_preds[[3]]**2, na.rm = TRUE))

wr_preds[[4]] <- (predict(wr_100, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
wr_rmse[4] <- sqrt(mean(wr_preds[[4]]**2, na.rm = TRUE))

wr_preds[[5]] <- (predict(wr_200, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
wr_rmse[5] <- sqrt(mean(wr_preds[[5]]**2, na.rm = TRUE))

wr_preds[[6]] <- (predict(wr_300, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
wr_rmse[6] <- sqrt(mean(wr_preds[[6]]**2, na.rm = TRUE))
wr_rmse
#0.75 wins 

#####################################
## TE ##
te_25  <- f("TE", 0.25)
te_50  <- f("TE", 0.50)
te_75  <- f("TE", 0.75)
te_100 <- f("TE", 1.00)
te_200 <- f("TE", 2.00)
te_300 <- f("TE", 3.00)

## Predict on test
te_preds    <- list()
te_rmse     <- vector()

te_preds[[1]] <- (predict(te_25, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
te_rmse[1] <- sqrt(mean(te_preds[[1]]**2, na.rm = TRUE))

te_preds[[2]] <- (predict(te_50, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
te_rmse[2] <- sqrt(mean(te_preds[[2]]**2, na.rm = TRUE))

te_preds[[3]] <- (predict(te_75, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
te_rmse[3] <- sqrt(mean(te_preds[[3]]**2, na.rm = TRUE))

te_preds[[4]] <- (predict(te_100, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
te_rmse[4] <- sqrt(mean(te_preds[[4]]**2, na.rm = TRUE))

te_preds[[5]] <- (predict(te_200, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
te_rmse[5] <- sqrt(mean(te_preds[[5]]**2, na.rm = TRUE))

te_preds[[6]] <- (predict(te_300, newdata = test_adp_value[test_adp_value$pos == "TE",]) - test_adp_value$value[test_adp_value$pos == "TE"])
te_rmse[6] <- sqrt(mean(te_preds[[6]]**2, na.rm = TRUE))
te_rmse
#0.25 wins 

## DST ##
dst_25  <- f("K", 0.25)
dst_50  <- f("K", 0.50)
dst_75  <- f("K", 0.75)
dst_100 <- f("K", 1.00)
dst_200 <- f("K", 2.00)
dst_300 <- f("K", 3.00)

## Predict on dstst
dst_preds    <- list()
dst_rmse     <- vector()

dst_preds[[1]] <- (predict(dst_25, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
dst_rmse[1] <- sqrt(mean(dst_preds[[1]]**2, na.rm = TRUE))

dst_preds[[2]] <- (predict(dst_50, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
dst_rmse[2] <- sqrt(mean(dst_preds[[2]]**2, na.rm = TRUE))

dst_preds[[3]] <- (predict(dst_75, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
dst_rmse[3] <- sqrt(mean(dst_preds[[3]]**2, na.rm = TRUE))

dst_preds[[4]] <- (predict(dst_100, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
dst_rmse[4] <- sqrt(mean(dst_preds[[4]]**2, na.rm = TRUE))

dst_preds[[5]] <- (predict(dst_200, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
dst_rmse[5] <- sqrt(mean(dst_preds[[5]]**2, na.rm = TRUE))

dst_preds[[6]] <- (predict(dst_300, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
dst_rmse[6] <- sqrt(mean(dst_preds[[6]]**2, na.rm = TRUE))
dst_rmse
#3.00 wins 

## K ##
k_25  <- f("K", 0.25)
k_50  <- f("K", 0.50)
k_75  <- f("K", 0.75)
k_100 <- f("K", 1.00)
k_200 <- f("K", 2.00)
k_300 <- f("K", 3.00)

## Predict on kst
k_preds    <- list()
k_rmse     <- vector()

k_preds[[1]] <- (predict(k_25, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
k_rmse[1] <- sqrt(mean(k_preds[[1]]**2, na.rm = TRUE))

k_preds[[2]] <- (predict(k_50, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])



k_rmse[2] <- sqrt(mean(k_preds[[2]]**2, na.rm = TRUE))

k_preds[[3]] <- (predict(k_75, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
k_rmse[3] <- sqrt(mean(k_preds[[3]]**2, na.rm = TRUE))

k_preds[[4]] <- (predict(k_100, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
k_rmse[4] <- sqrt(mean(k_preds[[4]]**2, na.rm = TRUE))

k_preds[[5]] <- (predict(k_200, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
k_rmse[5] <- sqrt(mean(k_preds[[5]]**2, na.rm = TRUE))

k_preds[[6]] <- (predict(k_300, newdata = test_adp_value[test_adp_value$pos == "K",]) - test_adp_value$value[test_adp_value$pos == "K"])
k_rmse[6] <- sqrt(mean(k_preds[[6]]**2, na.rm = TRUE))
k_rmse
#3.00 wins 
test_adp_value %>% arrange(pos,value) %>% select(player,year,pos,value)

length(qb_preds)

k_preds[[6]]
qb_preds[[1]]
test_adp_value %>%
  
qb_preds[[2]]
