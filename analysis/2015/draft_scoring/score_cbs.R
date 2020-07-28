setwd('/home/john/stats_corner/2015/draft_scoring')
final_projections <- read.csv("final_projections_data.csv",stringsAsFactors = FALSE)

qbs_cbs <- filter(final_projections,
                   pos == 'QB') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    top_n(2,cbs_points)
rbs_cbs <- filter(final_projections,
                   pos == 'RB') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    top_n(2,cbs_points)
wrs_cbs <- filter(final_projections,
                   pos == 'WR') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    top_n(3,cbs_points)
tes_cbs <- filter(final_projections,
                   pos == 'TE') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    top_n(1,cbs_points)
ks_cbs <- filter(final_projections,
                  pos == 'K') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    top_n(1,cbs_points)
matovina_k <- data.frame(owner = 'Matovina',
                         player = 'Replacement Level',
                         pos = 'K',
                         cbs_points = 129.2)
ks_cbs <- bind_rows(ks_cbs,matovina_k)
dst_cbs <- filter(final_projections,
                   pos == 'D/ST') %>%
    group_by(owner) %>%
    arrange(desc(cbs_points)) %>%
    top_n(1,cbs_points)
cbs_starters <- bind_rows(qbs_cbs,
                           rbs_cbs,
                           wrs_cbs,
                           tes_cbs,
                           ks_cbs,
                           dst_cbs)
summarise_cbs <- cbs_starters %>%
    group_by(owner) %>%
    summarise(total_points = sum(cbs_points)) %>%
    arrange(desc(total_points))
summarise_cbs
summarise_cbs$z <- (summarise_cbs$total_points - mean(summarise_cbs$total_points))/sd(summarise_cbs$total_points)
summarise_cbs

