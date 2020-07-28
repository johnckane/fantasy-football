
draft_adp <- read.csv("draft_adp.csv",
                      stringsAsFactors = FALSE,
                      header = TRUE)

qb <- filter(draft_adp, pos == 'QB')
rb <- filter(draft_adp, pos == 'RB')
wr <- filter(draft_adp, pos == 'WR')
te <- filter(draft_adp, pos == 'TE')

qb_model <- loess(data = qb,value~pick, span = 0.25)
qb_df <- data.frame(cbind(qb_model$x[,"pick"], as.numeric(qb_model$fitted)))
qb_df$pos <- "QB"

rb_model <- loess(data = rb, value ~ pick, span = 0.25)
rb_df <- data.frame(cbind(rb_model$x[,"pick"], as.numeric(rb_model$fitted)))
rb_df$pos <- "RB"

wr_model <- loess(data = wr, value ~ pick, span = 0.25)
wr_df <- data.frame(cbind(wr_model$x[,"pick"], as.numeric(wr_model$fitted)))
wr_df$pos <- "WR"

te_model <- loess(data = te, value ~ pick, span = 0.25)
te_df <- data.frame(cbind(te_model$x[,"pick"], as.numeric(te_model$fitted)))
te_df$pos <- "TE"

all_df <- bind_rows(qb_df,rb_df,wr_df,te_df)

colnames(all_df) <- c("pick","est_cost","pos")
library(sqldf)
all_df2 <- sqldf('select distinct * from all_df')

write.csv(all_df2,"fitted_values.csv",row.names = FALSE)
