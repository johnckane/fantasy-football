library(sqldf)
setwd("/home/john/Fantasy Football/2015 Prep")

cost_model <- read.csv("loess_cost.csv", header = TRUE, stringsAsFactors = FALSE)
points_model <- read.csv("loess_ppg.csv", header = TRUE, stringsAsFactors = FALSE)
raw_data <- read.csv("raw_data_for_models.csv", header = TRUE, stringsAsFactors = FALSE)
cluster_data <-read.csv("cluster_data.csv", header = TRUE, stringsAsFactors = FALSE)


data <-sqldf('
             select
                a.pos
            ,   a.pick
            ,   a.fitted_cost
            ,   c.n_cost
            ,   c.mean_cost
            ,   c.last_cost
            ,   c.var_cost
            ,   c.range_cost
            ,   b.fitted_points
            ,   c.last_points
            ,   c.n_points
            ,   c.mean_points
            ,   c.var_points
            ,   c.range_points
            ,   d.cluster
            ,   d.mean_cost as mean_cluster_cost
            ,   d.range_cost as range_cluster_cost
            ,   d.mean_points as mean_cluster_points
            ,   d.range_points as range_cluster_points
            ,   d.range_picks as range_cluster_picks
            from
                cost_model as a
             ,  points_model as b
             ,  raw_data as c
             ,  cluster_data as d
             where
                a.pos = b.pos
             and a.pick = b.pick
             and b.pos = c.pos
             and b.pick = c.pick
             and c.pos = d.pos
             and c.pick = d.pick')

write.csv(data,"final_data.csv",row.names=FALSE)
