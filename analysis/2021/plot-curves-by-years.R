## Objective is to see if curves are different based on season

## QB
load(file="/home/john/projects/fantasy-football/data/clustering-data/cost_adp_prod_cleaned.Rda")

qb_data <- 
  cost_adp_prod_cleaned %>%
  filter(position == 'QB',
         pos_adp < 32) %>%
  mutate(year_group = ifelse(year <=2014,'2014 or before','2015 or later'))

ggplot(data = qb_data,
       aes(x = pos_adp, y = adj_value,colour=year_group)) +
  geom_point() +
  geom_smooth(span=0.75)

ggplot(data = qb_data,
       aes(x = pos_adp, y = ppg,colour=year_group)) +
  geom_point() +
  geom_smooth(span=0.75)


## RB
load(file="/home/john/projects/fantasy-football/data/clustering-data/cost_adp_prod_cleaned.Rda")

rb_data <- 
  cost_adp_prod_cleaned %>%
  filter(position == 'RB',
         pos_adp < 100) %>%
  mutate(year_group = ifelse(year <=2014,'2014 or before','2015 or later'))

ggplot(data = rb_data,
       aes(x = pos_adp, y = adj_value,colour=year_group)) +
  geom_point() +
  geom_smooth(span = 0.75)

ggplot(data = rb_data,
       aes(x = pos_adp, y = ppg,colour=year_group)) +
  geom_point() +
  geom_smooth(span=0.75)


## TE

load(file="/home/john/projects/fantasy-football/data/clustering-data/cost_adp_prod_cleaned.Rda")

te_data <- 
  cost_adp_prod_cleaned %>%
  filter(position == 'TE',
         pos_adp < 100) %>%
  mutate(year_group = ifelse(year <=2014,'2014 or before','2015 or later'))

ggplot(data = te_data,
       aes(x = pos_adp, y = adj_value, colour=year_group)) +
  geom_point() +
  geom_smooth(span=0.50)

ggplot(data = te_data,
       aes(x = pos_adp, y = ppg, colour=year_group)) +
  geom_point() + 
  geom_smooth(span=0.75)


## WR
load(file="/home/john/projects/fantasy-football/data/clustering-data/cost_adp_prod_cleaned.Rda")

wr_data <- 
  cost_adp_prod_cleaned %>%
  filter(position == 'WR',
         pos_adp < 100) %>%
  mutate(year_group = ifelse(year <=2014,'2014 or before','2015 or later'))

ggplot(data = wr_data,
       aes(x = pos_adp, y = adj_value,colour=year_group)) +
  geom_point() +
  geom_smooth(span=0.25)

ggplot(data = wr_data,
       aes(x = pos_adp, y = ppg,colour=year_group)) +
  geom_point() +
  geom_smooth(span=0.75)