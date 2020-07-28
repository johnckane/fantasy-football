library(sqldf)
library(reshape2)

# This version includes all record options, including ties. While nice, it muddies up
# the presentation. I will re-do this citing only wins rather than record


g <- function(x){

    data_all <- games_played %>% 
        filter(year != 2009,year!=2015,week<=x) %>%
        group_by(year,team,playoffs) %>%
        summarise(wins = sum(W)) %>%
        group_by(wins) %>%
        summarise(yeas = sum(playoffs),
                  ttl  = n()) %>%
        select(wins,ttl,yeas)
    
  data_summary <- 
    data_all %>%
    group_by(wins) %>%
    summarise(total_yeas = sum(yeas),
              total      = sum(ttl),
              playoff_rate = total_yeas/total) %>%
    mutate(week = x)
  
  return(data_summary)
}


full_data <- rbind(g(1),g(2),g(3),g(4),g(5),g(6),g(7),g(8),g(9),g(10),g(11),g(12),g(13))
write.csv(full_data,"playoff_rates.csv",row.names=FALSE)

rate_matrix <- dcast(full_data,week~wins,value.var='playoff_rate')

full_data$round_prob = paste(format(round(full_data$playoff_prob*100)),"%",sep="")