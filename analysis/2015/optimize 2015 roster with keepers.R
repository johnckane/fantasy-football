player <- c("Ben Roethlisberger",
             "DeMarco Murray",
             "Arian Foster",
             "Mohamed Sanu",
             "Malcom Floyd",
             "Doug Baldwin",
             "Jordan Reed",
             "Shaun Hill",
             "Bills D/ST",
             "Adam Vinatieri",
             "Seahawks Coach",
             "Nick Foles",
             "Donte Moncrief",
             "Joseph Randle",
             "Lance Dunbar",
             "John Brown")
pos <- c("QB",
               "RB",
               "RB",
               "WR",
               "WR",
               "WR",
               "TE",
               "QB",
               "D/ST",
               "K",
               "HC",
               "QB",
               "WR",
               "RB",
               "RB",
               "WR")
cost_to_keep <- c(43,78,30,7,7,7,8,8,7,7,8,17,7,7,7,7)

df_keeper <- data.frame(player,pos,cost_to_keep)
df_keeper$keeper <- 1

df_keeper <- df_keeper %>% filter(pos %in% c('QB','RB','WR','TE'))

