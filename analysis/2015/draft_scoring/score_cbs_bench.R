setwd('/home/john/stats_corner/2015/draft_scoring')
final_projections <- read.csv("final_projections_data.csv",stringsAsFactors = FALSE)

qbs_cbs_bench <- filter(final_projections,
                         pos == 'QB') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 3)
rbs_cbs_bench <- filter(final_projections,
                         pos == 'RB') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 3)
wrs_cbs_bench <- filter(final_projections,
                         pos == 'WR') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 4)
tes_cbs_bench <- filter(final_projections,
                         pos == 'TE') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 2)

cbs_bench <- bind_rows(qbs_cbs_bench,
                        rbs_cbs_bench,
                        wrs_cbs_bench,
                        tes_cbs_bench)
summarise_cbs_bench <- cbs_bench %>%
    group_by(owner) %>%
    summarise(total_points = sum(cbs_points)) %>%
    arrange(desc(total_points))
summarise_cbs_bench
summarise_cbs_bench$z <- (summarise_cbs_bench$total_points - mean(summarise_cbs_bench$total_points))/sd(summarise_cbs_bench$total_points)
summarise_cbs_bench