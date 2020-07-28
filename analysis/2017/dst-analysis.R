dst_data <-
  adp_data3 %>%
  left_join(pwd_total_wlag, by = c("year","player_code" = "player")) %>%
  left_join(draft_data, by = c("year","player_code" = "player")) %>%
  filter(pos.x == "D/ST")

library(ggplot2)

p <- ggplot(data = dst_data,
            aes(x=position_adp,y=ppg))
p +
  geom_point() +
  geom_smooth()

training_data2017

dst_data %>%
  filter(pos == 'D/ST') %>%
  group_by(year) %>%
  summarise(min_ppg = min(ppg),
            max_ppg = max(ppg),
            mean_ppg = mean(ppg))


# Maybe you want two top 5 defenses?
# Rare chance of going < 7.5 ppg, should be around 8 or 9. 

p2 <- ggplot(data = dst_data,
             aes(x = adj_value,y=ppg,label=position_adp))



p2 +
  geom_point() +
  geom_smooth() +
  geom_label()

lm(data = dst_data,ppg~adj_value)

k_data <-
  adp_data3 %>%
  left_join(pwd_total_wlag, by = c("year","player_code" = "player")) %>%
  left_join(draft_data, by = c("year","player_code" = "player_code")) %>%
  filter(pos.x == "K")

colnames(adp_data3)
head(adp_data3)
k_data %>% select(player_code,position_adp,ppg,adj_value)

p <- ggplot(data = k_data,
            aes(x=position_adp,y=ppg))

p +
  geom_point() +
  geom_smooth()


k_data %>% head()
p2 <- ggplot(data = k_data,
             aes(x = adj_value,y=ppg,label=position_adp))
draft_data %>% filter(pos == "K")

with(adp_data3,table(pos))
adp_data3 %>% filter(pos == "K")



k_data$adj_value
k_data$ppg
colnames(k_data)
colnames(draft_data)
colnames(adp_data3)
p2 +
  geom_point() +
  geom_label()

qb_data <-
  adp_data3 %>%
  left_join(pwd_total_wlag, by = c("year","player_code" = "player")) %>%
  left_join(draft_data, by = c("year","player_code" = "player_code")) %>%
  filter(pos.x == "QB")

p2 <- ggplot(data = qb_data,
             aes(x = adj_value,y=ppg,label=position_adp))
p2 +
  geom_point() +
  geom_label()

