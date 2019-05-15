library(tidyverse)
load(file = "/home/john/projects/fantasy-football/data/analysis-data/wr.Rda")

top <- 
wr %>%
  mutate(top40 = ifelse(league_rank <= 40,1,0),
         top40_first8 = ifelse(league_rank_first8 <= 40,1,0),
         top40_last8 = ifelse(league_rank_last8 <= 40,1,0),
         top20 = ifelse(league_rank <= 20,1,0),
         top20_first8 = ifelse(league_rank_first8 <= 20,1,0),
         top20_last8 = ifelse(league_rank_last8 <= 20,1,0),
         top10 = ifelse(league_rank <= 10,1,0),
         top10_first8 = ifelse(league_rank_first8 <= 10,1,0),
         top10_last8 = ifelse(league_rank_last8 <= 10,1,0))

with(top %>% filter(top40 == 1), prop.table(table(top40_first8,top40_last8)))
with(top %>% filter(top20 == 1), prop.table(table(top20_first8,top20_last8)))
with(top %>% filter(top10 == 1), prop.table(table(top10_first8,top10_last8)))

top %>%
  filter(top10_first8 == 1, top10_last8 == 1) %>%
  ggplot(data = .,
         aes(x = league_rank)) +
  geom_histogram(binwidth = 1)

