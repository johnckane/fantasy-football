head(lj2)

lj3 <- lj2%>%
  ungroup() %>%
  filter(keeper == 0, pos %in% c("QB","RB","WR","TE")) %>%
  arrange(year,pos,pos_rank) %>%
  group_by(year,pos) %>%
  mutate(new_rank = row_number()) %>%
  select(year,pos,new_rank,value)
lj3

library(tidyr)

lj4 <- lj3 %>%
  arrange(pos,new_rank,year) %>%
  group_by(pos,new_rank) %>%
  mutate(pred_value = lag(value),
         error = pred_value - value,
         sq_error  = (pred_value - value) ** 2) 
  
p1 <- ggplot(data = lj4, 
             aes(x = new_rank, y = error))
p1 + geom_point() + 
  facet_wrap(~pos) +
  geom_smooth()

  lj4 %>%
  group_by(pos,year) %>%
  summarise(rmse = sqrt(mean(sq_error, na.rm = T))) %>%
  print(n = 24)

