
#plot different spans

# alphas <- c(0.1,0.25,0.5,0.75,0.9)

alphas <- c(0.25,0.5,0.75,0.9,1)
predict(object = loess(data = qb_data, adj_value~ pos_adp,span=alphas[1]),
        newdata = qb_smoothed_data)

for(i in seq_along(alphas)){
  ggplot(data = data.frame(adp = qb_smoothed_data$pos_adp,
                         fitted_value = predict(
                           object = loess(data = qb_data, adj_value ~ pos_adp,
                                          span = alphas[i]),
                           newdata = qb_smoothed_data)
                         ),
       aes(x = adp,
           y = fitted_value)) +
  geom_line() +
    geom_point(data = qb_data,aes(x = pos_adp, y = adj_value))

}


i = 2
ggplot(data = data.frame(adp = qb_smoothed_data$pos_adp,
                         fitted_value = predict(
                           object = loess(data = qb_data, adj_value ~ pos_adp,
                                          span = alphas[i]),
                           newdata = qb_smoothed_data)
),
aes(x = adp,
    y = fitted_value)) +
  geom_line() +
  geom_point(data = qb_data,aes(x = pos_adp, y = adj_value))


################################
i = 5
ggplot(data = data.frame(adp = wr_smoothed_data$pos_adp,
                         fitted_value = predict(
                           object = loess(data = wr_data, adj_value ~ pos_adp,
                                          span = alphas[i]),
                           newdata = wr_smoothed_data)
),
aes(x = adp,
    y = fitted_value)) +
  geom_line() +
  geom_point(data = wr_data,aes(x = pos_adp, y = adj_value))

?loess
## Which offers the lowest
alphas <- c(0.15,0.2,0.25)
i = 2
ggplot(data = data.frame(adp = wr_smoothed_data$pos_adp,
                         fitted_value = predict(
                           object = loess(data = wr_data, adj_value ~ pos_adp,
                                          span = alphas[i]),
                           newdata = wr_smoothed_data)
),
aes(x = adp,
    y = fitted_value)) +
  geom_line() +
  geom_point(data = wr_data,aes(x = pos_adp, y = adj_value))

## 