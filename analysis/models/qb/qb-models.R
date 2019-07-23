library(tidyverse)
load("/home/john/projects/fantasy-football/data/analysis-data/qb.Rda")
load("/home/john/projects/fantasy-football/data/analysis-data/qb_offense.Rda")
qb_model_data <- 
  qb %>%
  filter(league_rank <= 32) %>%
  mutate(top20 = ifelse(league_rank <= 20,1,0),
         top10 = ifelse(league_rank <= 10,1,0),
         top5  = ifelse(league_rank <= 5,1,0)) %>%
  arrange(player,year) %>%
  group_by(player) %>%
  mutate(top20_lastyear = ifelse(lag(year) == year - 1, lag(top20),NA),
         top10_lastyear = ifelse(lag(year) == year - 1, lag(top10),NA),
         top5_lastyear  = ifelse(lag(year) == year - 1, lag(top5),NA),
         team_rank_lastyear = ifelse(lag(year) == year - 1,lag(team_rank),NA),
         league_rank_lastyear = ifelse(lag(year) == year - 1,lag(league_rank),NA))

model1 <- glm(data = qb_model_data %>% filter(year == 2018),
              family = "binomial",
              formula = top10 ~ experience + age + top20_lastyear + top10_lastyear + top5_lastyear +
                league_rank_lastyear)

summary(model1)

library(rpart)
tree1 <- rpart(data = qb_model_data %>% filter(year == 2018),
               method = "class",
               formula = top10 ~ experience + age + top20_lastyear + top10_lastyear + top5_lastyear +
               league_rank_lastyear,
               control = rpart.control(cp = 10e-5))
tree1


qb_model_data %>%
  filter(top20 == 1) %>%
  group_by(year) %>%
  summarise(total_players = sum(top20),
            carryover = sum(top20_lastyear, na.rm = T))

qb_model_data %>%
  filter(top10 == 1) %>%
  group_by(year) %>%
  summarise(total_players = sum(top10),
            carryover10 = sum(top10_lastyear,na.rm = T),
            carryover20 = sum(top20_lastyear,na.rm = T))

qb_model_data %>%
  filter(top5 == 1) %>%
  group_by(year) %>%
  summarise(total_players = sum(top5),
            carryover5 = sum(top5_lastyear, na.rm = T),
            carryover10 = sum(top10_lastyear,na.rm = T),
            carryover20 = sum(top20_lastyear,na.rm = T))
head(ab)
head(qb)
qb_model_data %>%
  filter(year == 2018) %>%
  arrange(desc(total_points)) %>%
  print(n = 10)

qb_model_data %>%
  filter(year == 2017) %>%
  arrange(desc(total_points)) %>%
  select(name,team,league_rank,league_rank_lastyear) %>%
  print(n = 10)

qb_model_data %>%
  filter(year == 2018) %>%
  arrange(desc(total_points)) %>%
  select(name,team,league_rank,league_rank_lastyear) %>%
  print(n = 10)
