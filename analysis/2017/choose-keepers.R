Derek Carr, Oak QB	
Jeremy Hill, Cin RB  QBreaking News	
Terrance West, Bal RB	
Golden Tate, Det WR	
T.Y. Hilton, Ind WRBreaking Video	
Taylor Gabriel, Atl WR	
Greg Olsen, Car TEBreaking News	
Matthew Stafford, Det QBRecent News	
Chargers D/ST D/ST	
Stephen Hauschka, Buf K	
Chargers Coach H


Rashad Jennings,
Steve Smith Sr., Bal WR	
Mike Wallace, Bal WR	
Joe Flacco, Bal QB  QBreaking News	
Dwayne Washington, Det RBRecent News



pos <- c("QB",
               "RB",
               "RB",
               "WR",
               "WR",
               "WR",
               "TE",
               "QB",
               "RB",
               "WR",
               "WR",
               "QB",
               "WR",
               "QB")
player <- c("Aaron Rodgers",
            "Karlos Williams",
            "Alfred Morris",
            "Anquan Boldin",
            "Travis Benjamin",
            "Michael Floyd",
            "Travis Kelce",
            "Drew Brees",
            "Justin Forsett",
            "Julian Edelman",
            "Stevie Johnson",
            "Joe Flacco",
            "Keenan Allen",
            "Colin Kaepernick")
pred_cost <- c(111,7,31,13,7,11,24,92,22,31,7,27,21,38)

df_keeper <- data.frame(player,pos,pred_cost)
df_keeper$keeper <- 1

df_keeper <- df_keeper %>% filter(pos %in% c('QB','RB','WR','TE'))
df_keeper <- df_keeper %>% arrange(player)
df_keeper
df_keeper$ppg <- c(24.5,3.7,6.1,5.8,22,18.6,10.8,9.5,4.8,13.0,10.9,0,7.9,9.0)
df_keeper

setdiff(colnames(full_data),colnames(df_keeper))

colnames(full_data)
full_data2 <- full_data %>%
  select(player,pos,pred_cost,ppg,keeper)

###
full_data$keeper <- 0

all_data <- union(full_data2,df_keeper)
all_data$pred_cost <- ifelse(all_data$pos == "QB" & all_data$keeper == 0,
                             all_data$pred_cost + 14.4,
                             ifelse(all_data$pos == "RB" & all_data$keeper == 0,
                                    all_data$pred_cost + 10.2,
                                    ifelse(all_data$pos == "TE" & all_data$keeper == 0,
                                           all_data$pred_cost + 6.0,
                                           ifelse(all_data$pos == "WR" & all_data$keeper == 0,
                                                  all_data$pred_cost + 6.9,
                                                  all_data$pred_cost)
                                    )
                             )
                        )

names = c("Cam Newton","Todd Gurley","Odell Beckham Jr","Marcus Mariota","Tyrod Taylor",
          "Jamaal Charles","Devonta Freeman","Julio Jones","Russell Wilson","Alshon Jeffry","Russell Wilson",
          "Brandon Marshall","Kirk Cousins","Blake Bortles","Andrew Luck")
filter(all_data, player == "Aaron Rodgers") %>% select(pred_cost)


### Re-do constraints
## Set up the objective function ##
objective_model<- ifelse(is.na(all_data$ppg), 0, all_data$ppg)
## Set up the objective function ##
## Lay out the constraints ##
# Total salary row
c1_model <- all_data$pred_cost
# 2 QBs
c2 <- ifelse(all_data$pos=='QB',1,0)
# 2 RBs 
c3 <- ifelse(all_data$pos=='RB',1,0)
# 1 TE 
c4 <- ifelse(all_data$pos=='TE',1,0)
# 3 WRs 
c5 <- ifelse(all_data$pos=='WR',1,0)
# 2 keepers
c6 <- ifelse(all_data$keeper==1,1,0)
# Only 1 of each possible keeper
c7  <- ifelse(all_data$player == "Aaron Rodgers",1,0)
c8  <- ifelse(all_data$player == "Karlos Williams",1,0)
c9  <- ifelse(all_data$player == "Alfred Morris",1,0)
c10 <- ifelse(all_data$player == "Anquan Boldin",1,0)
c11 <- ifelse(all_data$player == "Travis Benjamin",1,0)
c12 <- ifelse(all_data$player == "Michael Floyd",1,0)
c13 <- ifelse(all_data$player == "Travis Kelce",1,0)
c14 <- ifelse(all_data$player == "Drew Brees",1,0)
c15 <- ifelse(all_data$player == "Justin Forsett",1,0)
c16 <- ifelse(all_data$player == "Julian Edelman",1,0)
c17 <- ifelse(all_data$player == "Stevie Johnson",1,0)
c18 <- ifelse(all_data$player == "Joe Flacco",1,0)
c19 <- ifelse(all_data$player == "Keenan Allen",1,0)
# noone already kept
c20 <- ifelse(all_data$player %in% names,1,0)
# only 8 players
c24 <- rep(1,dim(all_data)[1])

constraints_model <- matrix(rbind(c1_model,c2,c2,c3,c3,c4,c4,c5,c5,
                                  c6,c7,c8,c9,c10,c11,c12,
                                  c13,c14,c15,c16,c17,c18,c19,
                                  c20,
                                  c24),nrow=25)

## Set up the direction of the constrants ##
direction <- c('<=','>=','<','>=','<','>=','<','>=','<',
               '<=',
               '<=','<=','<=','<=','<=','<=','<=','<=','<=','<=','<=','<=','<=',
               '==',
               '==')

## Set up the right-hand-side of the equation
rhs <- c(290,2,4,2,4,1,3,3,5,
         2, # num keepers
         1,1,1,1,1,1,1,1,1,1,1,1,1, # 13 eligible keepers
         0, # no one already being kept
         8) #total players
library(lpSolve)
solve_model <- lp("max",
                  objective_model,
                  constraints_model,
                  direction,
                  rhs,
                  all.bin=TRUE,
                  num.bin.solns = 1,
                  compute.sens = 1)

solution_model <- data.frame(as.character(all_data$player),
                             as.character(all_data$pos),
                             all_data$pred_cost,
                             all_data$ppg,
                             all_data$keeper,
                             solve_model$solution) %>% 
  filter(solve_model.solution==1)



colnames(solution_model) <- c("player","pos","pred_cost","ppg","keeper","solution")



### Model Solution ###
solution_model %>% arrange(pos,ppg)
sum(solution_model$pred_cost)
sum(solution_model$ppg)


