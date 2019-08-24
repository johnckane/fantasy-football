clustered_data %>%
  group_by(new_cluster,position) %>%
  slice(1) %>%
  mutate(cost_per_point = avg_cost/avg_ppg) %>%
  ggplot(data = .,
         aes(x = new_cluster, y = cost_per_point))+
  geom_point()+
  geom_line() +
  facet_wrap(~position)

