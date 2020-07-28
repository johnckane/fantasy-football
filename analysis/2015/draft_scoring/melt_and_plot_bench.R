
all_scores_bench <- sqldf('
                    select
                    a.owner
                    ,   a.total_points as espn_points
                    ,   a.z as espn_z
                    ,   b.total_points as cbs_points
                    ,   b.z as cbs_z
                    ,   c.total_points as yahoo_points
                    ,   c.z as yahoo_z
                    from
                    summarise_espn_bench as a
                    ,   summarise_cbs_bench as b
                    ,   summarise_yahoo_bench as c
                    where
                    a.owner = b.owner
                    and a.owner = c.owner')
library(reshape2)
melt_all_scores_bench <- melt(all_scores_bench,
                        id.vars = "owner")
melt_all_scores_bench$src <- lapply(str_split(melt_all_scores_bench$variable,"_"),'[',1)
melt_all_scores_bench$var <- lapply(str_split(melt_all_scores_bench$variable,"_"),'[',2)
melt_all_scores_bench <- melt_all_scores_bench[,c(1,4,5,3)]

library(ggplot2)

plot <- ggplot(data=filter(melt_all_scores_bench, var == 'z'),
               aes(x=owner,
                   y=value))
plot + 
    geom_point() +
    scale_x_discrete
    theme_bw()
