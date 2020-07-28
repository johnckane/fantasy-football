setwd('/home/john/stats_corner/2015/draft_scoring')
final_projections <- read.csv("final_projections_data.csv",stringsAsFactors = FALSE)

qbs_yahoo_bench <- filter(final_projections,
                         pos == 'QB') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 3)
rbs_yahoo_bench <- filter(final_projections,
                         pos == 'RB') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 3)
wrs_yahoo_bench <- filter(final_projections,
                         pos == 'WR') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 4)
tes_yahoo_bench <- filter(final_projections,
                         pos == 'TE') %>%
    group_by(owner) %>%
    arrange(desc(yahoo_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 2)

yahoo_bench <- bind_rows(qbs_yahoo_bench,
                        rbs_yahoo_bench,
                        wrs_yahoo_bench,
                        tes_yahoo_bench)
summarise_yahoo_bench <- yahoo_bench %>%
    group_by(owner) %>%
    summarise(total_points = sum(yahoo_points)) %>%
    arrange(desc(total_points))
summarise_yahoo_bench
summarise_yahoo_bench$z <- (summarise_yahoo_bench$total_points - mean(summarise_yahoo_bench$total_points))/sd(summarise_yahoo_bench$total_points)
summarise_yahoo_bench