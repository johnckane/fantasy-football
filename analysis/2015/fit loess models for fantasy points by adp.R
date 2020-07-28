setwd("/home/john/Fantasy Football/2015 Prep")

adp_draft <- read.csv("adp_draft.csv", stringsAsFactors = FALSE, header = TRUE)
player_data_adp <- read.csv("player_data_adp.csv", stringsAsFactors = FALSE, header = TRUE)

library(sqldf)
library(dplyr)

## merge adp with draft cost and fantasy production

summary <- sqldf('
                 select
                    a.year,
                    a.pos,
                    a.pick,
                    a.name,
                    a.points,
                    b.value,
                    b.keeper
                 from
                    player_data_adp as a
                 left join
                    adp_draft as b
                 on
                    a.year = b.year
                 and a.pick = b.pick
                 and a.pos = b.pos
                 and a.name = b.name')
filter(summary,is.na(value))
adp_draft$name <- ifelse(adp_draft$name == "Beanie Wells",
                         "Chris Wells",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "Mike Sims-Walker",
                         "Mike Walker",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "Robert Griffin III",
                         "Robert Griffin",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "EJ Manuel",
                         "E.J. Manuel",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "T.Y. Hilton",
                         "Ty Hilton",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "LeVeon Bell",
                         "Le'Veon Bell",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "Joshua Cribbs",
                         "Josh Cribbs",
                         adp_draft$name)
adp_draft$name <- ifelse(adp_draft$name == "Carnell Williams",
                         "Cadillac Williams",
                         adp_draft$name)

## Try it again
summary2 <- sqldf('
                 select
                    a.year,
                 a.pos,
                 a.pick,
                 a.name,
                 a.points,
                 b.value,
                 b.keeper
                 from
                 player_data_adp as a
                 left join
                 adp_draft as b
                 on
                 a.year = b.year
                 and a.pick = b.pick
                 and a.pos = b.pos
                 and a.name = b.name')
filter(summary2,is.na(value))
## remove keepers, the draft value isn't reflective of an open-market
summary_points <- summary2

## Now fit multiple loess and spline models to determine fitted values for picks
## Need to do this separately for each position
f <- function(p,sp){
    df <- summary_points %>% filter(pos == p)
    return(loess(data=df, points~pick, span = sp))
}

## QB ##
qb_25  <- f("QB", 0.25)
qb_50  <- f("QB", 0.50)
qb_75  <- f("QB", 0.75)
qb_100 <- f("QB", 1.00)
qb_200 <- f("QB", 2.00)
qb_300 <- f("QB", 3.00)

qb_df <- data.frame(qb_25$x,
                    qb_25$fitted,
                    qb_50$fitted,
                    qb_75$fitted,
                    qb_100$fitted,
                    qb_200$fitted,
                    qb_300$fitted)
qb_resid <- data.frame(qb_25$residuals,
                       qb_50$residuals,
                       qb_75$residuals,
                       qb_100$residuals,
                       qb_200$residuals,
                       qb_300$residuals)
colnames(qb_resid)
colnames(qb_resid) <- c("x1","x2","x3","x4","x5","x6")
qb_resid %>%
    summarise(rmse25  = sqrt(mean(x1**2)),
              rmse50  = sqrt(mean(x2**2)),
              rmse75  = sqrt(mean(x3**2)),
              rmse100 = sqrt(mean(x4**2)),
              rmse200 = sqrt(mean(x5**2)),
              rmse300 = sqrt(mean(x6**2)))
## RB ##
rb_25  <- f("RB", 0.25)
rb_50  <- f("RB", 0.50)
rb_75  <- f("RB", 0.75)
rb_100 <- f("RB", 1.00)
rb_200 <- f("RB", 2.00)
rb_300 <- f("RB", 3.00)

rb_df <- data.frame(rb_25$x,
                    rb_25$fitted,
                    rb_50$fitted,
                    rb_75$fitted,
                    rb_100$fitted,
                    rb_200$fitted,
                    rb_300$fitted)
rb_resid <- data.frame(rb_25$residuals,
                       rb_50$residuals,
                       rb_75$residuals,
                       rb_100$residuals,
                       rb_200$residuals,
                       rb_300$residuals)
colnames(rb_resid)
colnames(rb_resid) <- c("x1","x2","x3","x4","x5","x6")
rb_resid %>%
    summarise(rmse25  = sqrt(mean(x1**2)),
              rmse50  = sqrt(mean(x2**2)),
              rmse75  = sqrt(mean(x3**2)),
              rmse100 = sqrt(mean(x4**2)),
              rmse200 = sqrt(mean(x5**2)),
              rmse300 = sqrt(mean(x6**2)))

## WR ##
wr_25  <- f("WR", 0.25)
wr_50  <- f("WR", 0.50)
wr_75  <- f("WR", 0.75)
wr_100 <- f("WR", 1.00)
wr_200 <- f("WR", 2.00)
wr_300 <- f("WR", 3.00)

wr_df <- data.frame(wr_25$x,
                    wr_25$fitted,
                    wr_50$fitted,
                    wr_75$fitted,
                    wr_100$fitted,
                    wr_200$fitted,
                    wr_300$fitted)
wr_resid <- data.frame(wr_25$residuals,
                       wr_50$residuals,
                       wr_75$residuals,
                       wr_100$residuals,
                       wr_200$residuals,
                       wr_300$residuals)
colnames(wr_resid)
colnames(wr_resid) <- c("x1","x2","x3","x4","x5","x6")
wr_resid %>%
    summarise(rmse25  = sqrt(mean(x1**2)),
              rmse50  = sqrt(mean(x2**2)),
              rmse75  = sqrt(mean(x3**2)),
              rmse100 = sqrt(mean(x4**2)),
              rmse200 = sqrt(mean(x5**2)),
              rmse300 = sqrt(mean(x6**2)))
## TE ##
te_25  <- f("TE", 0.25)
te_50  <- f("TE", 0.50)
te_75  <- f("TE", 0.75)
te_100 <- f("TE", 1.00)
te_200 <- f("TE", 2.00)
te_300 <- f("TE", 3.00)

te_df <- data.frame(te_25$x,
                    te_25$fitted,
                    te_50$fitted,
                    te_75$fitted,
                    te_100$fitted,
                    te_200$fitted,
                    te_300$fitted)
te_resid <- data.frame(te_25$residuals,
                       te_50$residuals,
                       te_75$residuals,
                       te_100$residuals,
                       te_200$residuals,
                       te_300$residuals)
colnames(te_resid)
colnames(te_resid) <- c("x1","x2","x3","x4","x5","x6")
te_resid %>%
    summarise(rmse25  = sqrt(mean(x1**2)),
              rmse50  = sqrt(mean(x2**2)),
              rmse75  = sqrt(mean(x3**2)),
              rmse100 = sqrt(mean(x4**2)),
              rmse200 = sqrt(mean(x5**2)),
              rmse300 = sqrt(mean(x6**2)))

## Looks like a span of 0.25 works the best for all positions.
qb <- data.frame(qb_25$x,qb_25$fitted)
rb <- data.frame(rb_25$x,rb_25$fitted)
wr <- data.frame(wr_25$x,wr_25$fitted)
te <- data.frame(te_25$x,te_25$fitted)

colnames(qb) <- c("pick","fitted_points")
colnames(rb) <- c("pick","fitted_points")
colnames(wr) <- c("pick","fitted_points")
colnames(te) <- c("pick","fitted_points")

qb$pos <- "QB"
rb$pos <- "RB"
wr$pos <- "WR"
te$pos <- "TE"

fitted_ppg <- rbind(qb,rb,wr,te)
fitted_ppg <- sqldf('
                    select distinct
                        pos,
                        pick,
                        fitted_points
                    from
                        fitted_ppg
                    ')
write.csv(fitted_ppg,"loess_ppg.csv")
