
all_scores <- sqldf('
                    select
                        a.owner
                    ,   a.total_points as espn_points
                    ,   a.z as espn_z
                    ,   b.total_points as cbs_points
                    ,   b.z as cbs_z
                    ,   c.total_points as yahoo_points
                    ,   c.z as yahoo_z
                    from
                        summarise_espn as a
                    ,   summarise_cbs as b
                    ,   summarise_yahoo as c
                    where
                        a.owner = b.owner
                    and a.owner = c.owner')
library(reshape2)
melt_all_scores <- melt(all_scores,
                        id.vars = "owner")
melt_all_scores$src <- lapply(str_split(melt_all_scores$variable,"_"),'[',1)
melt_all_scores$var <- lapply(str_split(melt_all_scores$variable,"_"),'[',2)
melt_all_scores <- melt_all_scores[,c(1,4,5,3)]

library(ggplot2)

plot <- ggplot(data=filter(melt_all_scores, var == 'z'),
               aes(x=owner,
                   y=value))
plot + 
    geom_point() +
    scale_x_discrete
    theme_bw()
