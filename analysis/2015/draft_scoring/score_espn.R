setwd('/home/john/stats_corner/2015/draft_scoring')
final_projections <- read.csv("final_projections_data.csv",stringsAsFactors = FALSE)

qbs_espn <- filter(final_projections,
                   pos == 'QB') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    top_n(2,espn_points)
rbs_espn <- filter(final_projections,
                   pos == 'RB') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    top_n(2,espn_points)
wrs_espn <- filter(final_projections,
                   pos == 'WR') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    top_n(3,espn_points)
tes_espn <- filter(final_projections,
                   pos == 'TE') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    top_n(1,espn_points)
ks_espn <- filter(final_projections,
                   pos == 'K') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    top_n(1,espn_points)
matovina_k <- data.frame(owner = 'Matovina',
                         player = 'Replacement Level',
                         pos = 'K',
                         espn_points = 129)
ks_espn <- bind_rows(ks_espn,matovina_k)
dst_espn <- filter(final_projections,
                   pos == 'D/ST') %>%
    group_by(owner) %>%
    arrange(desc(espn_points)) %>%
    top_n(1,espn_points)
espn_starters <- bind_rows(qbs_espn,
                           rbs_espn,
                           wrs_espn,
                           tes_espn,
                           ks_espn,
                           dst_espn)
summarise_espn <- espn_starters %>%
    group_by(owner) %>%
    summarise(total_points = sum(espn_points)) %>%
    arrange(desc(total_points))
summarise_espn$z <- (summarise_espn$total_points - mean(summarise_espn$total_points))/sd(summarise_espn$total_points)
print(summarise_espn)
