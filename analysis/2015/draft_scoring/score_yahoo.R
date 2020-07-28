setwd('/home/john/stats_corner/2015/draft_scoring')
final_projections <- read.csv("final_projections_data.csv",stringsAsFactors = FALSE)

qbs_yahoo <- filter(final_projections,
                  pos == 'QB') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    top_n(2,yahoo_points)
rbs_yahoo <- filter(final_projections,
                  pos == 'RB') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    top_n(2,yahoo_points)
wrs_yahoo <- filter(final_projections,
                  pos == 'WR') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    top_n(3,yahoo_points)
tes_yahoo <- filter(final_projections,
                  pos == 'TE') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    top_n(1,yahoo_points)
ks_yahoo <- filter(final_projections,
                 pos == 'K') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    top_n(1,yahoo_points)
matovina_k <- data.frame(owner = 'Matovina',
                         player = 'Replacement Level',
                         pos = 'K',
                         yahoo_points = 112.5)
ks_yahoo <- bind_rows(ks_yahoo,matovina_k)
dst_yahoo <- filter(final_projections,
                  pos == 'D/ST') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    top_n(1,yahoo_points)
yahoo_starters <- bind_rows(qbs_yahoo,
                          rbs_yahoo,
                          wrs_yahoo,
                          tes_yahoo,
                          ks_yahoo,
                          dst_yahoo)
summarise_yahoo <- yahoo_starters %>%
    group_by(owner) %>%
    summarise(total_points = sum(yahoo_points)) %>%
    arrange(desc(total_points))
summarise_yahoo
summarise_yahoo$z <- (summarise_yahoo$total_points - mean(summarise_yahoo$total_points))/sd(summarise_yahoo$total_points)
summarise_yahoo