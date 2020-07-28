setwd('/home/john/stats_corner/2015/draft_scoring')
final_projections <- read.csv("final_projections_data.csv",stringsAsFactors = FALSE)

qbs_espn_bench <- filter(final_projections,
                   pos == 'QB') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 3)
rbs_espn_bench <- filter(final_projections,
                   pos == 'RB') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 3)
wrs_espn_bench <- filter(final_projections,
                   pos == 'WR') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 4)
tes_espn_bench <- filter(final_projections,
                   pos == 'TE') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    mutate(n = row_number()) %>%
    filter(n == 2)

espn_bench <- bind_rows(qbs_espn_bench,
                           rbs_espn_bench,
                           wrs_espn_bench,
                           tes_espn_bench)
summarise_espn_bench <- espn_bench %>%
    group_by(owner) %>%
    summarise(total_points = sum(espn_points)) %>%
    arrange(desc(total_points))
summarise_espn_bench
summarise_espn_bench$z <- (summarise_espn_bench$total_points - mean(summarise_espn_bench$total_points))/sd(summarise_espn_bench$total_points)
summarise_espn_bench
