library(ggplot2)

draft_data %>%
  ggplot(aes(x = as.factor(year), y = value)) + 
  geom_boxplot() + 
  facet_wrap(~pos)

train_adp_value %>%
  ggplot(aes(x = overall_rank, y = value)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~pos)

train_adp_value %>%
  ggplot(aes(x = pos_rank, y = value)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~pos)

train_adp_value %>%
  ggplot(aes(x = lag1_price, y = value)) + 
  geom_point() + 
  facet_wrap(~pos)

train_adp_value %>%
  ggplot(aes(x = lag5_price, y = value)) + 
  geom_point() +
  facet_wrap(~pos)

train_adp_value %>%
  ggplot(aes(x = lag4_value, y = value)) + 
  geom_point() + 
  facet_wrap(~pos)

# what do I want to check for missingness?
colnames(train_adp_value)
train_adp_value %>%
  group_by(pos) %>%
  summarise(overall = sum(is.na(overall_rank))/n(),
            pos_rank_s = sum(is.na(pos_rank))/n()
)

train_adp_value %>%
  group_by(year,pos) %>%
  filter(pos == "QB") %>%
  summarise(num = n(),
            mean_value = mean(value),
            q75 = quantile(value, p=0.75),
            q25 = quantile(value, p=0.25))
  