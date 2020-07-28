# Gam models

colnames(train_adp_value)
library(gam)

train_adp_value2 <- train_adp_value %>%
  filter(is.na(pos_rank) == FALSE,
         is.na(overall_rank) == FALSE,
         is.na(year) == FALSE,
         is.na(pos) == FALSE)
train_adp_value2$pos <- as.factor(train_adp_value2$pos)

gam2 <- gam(value ~ s(year,2) + s(overall_rank,2) + pos + s(pos_rank,2), data = train_adp_value2)
gam3 <- gam(value ~ s(year,3) + s(overall_rank,3) + pos + s(pos_rank,3), data = train_adp_value2)

test_data2 <- test_adp_value
test_data2$pos <- as.factor(test_data2$pos)
test_data2 <- test_data2 %>%
  filter(is.na(pos_rank) == FALSE,
         is.na(overall_rank) == FALSE,
         is.na(year) == FALSE,
         is.na(pos) == FALSE)
p2 <- predict(gam2, newdata = test_data2)
p3 <- predict(gam3, newdata = test_data2)

rmse <- sqrt(mean((p2 - test_data2$value)**2))
rmse3 <- sqrt(mean((p3 - test_data2$value)**2))
rmse3
gam1
plot.gam(gam1, se = TRUE, col = "red")
summary(gam2)
summary(gam3)

df <- data.frame(p3, test_data2$value, test_data2$pos)
colnames(df)
str(df)
df %>%
  group_by(test_data2.pos) %>%
  summarise(rmse = sqrt(mean((p3 - test_data2.value)**2)))
