library(httr)
library(dplyr)
library(lpSolve)
library(sqldf)
library(XML)

url15 <- "http://fantasyfootballcalculator.com/adp.php?format=standard&year=2015&teams=12&view=graph&pos=all"

data15 <- GET(url15)
data15 <- readHTMLTable(rawToChar(data15$content), stringsAsFactors = F)
data15 <- as.data.frame(data15)
colnames(data15) <- c('obs',
                      'pick',
                      'name',
                      'pos',
                      'team',
                      'bye',
                      'overall',
                      'std_dev',
                      'high',
                      'low',
                      'times',
                      'graph')


data15 <- select(data15,name,pos,team,overall)
data15$overall <- as.numeric(data15$overall)

data <- data15 %>%
    group_by(pos) %>%
    mutate(pick = rank(overall,ties.method="random")) %>%
    filter(pos %in% c('QB','RB','TE','WR'))
setwd("/home/john/Fantasy Football/2015 Prep")
final_data   <- read.csv("final_data.csv", header = TRUE, stringsAsFactors = FALSE)


player_data <- sqldf('
                     select
                     a.name
                     ,  b.*
                     from
                     data as a
                     ,   final_data as b
                     where
                     
                     a.pos = b.pos
                     and a.pick = b.pick')




player_data$fitted_points <- round(all_data$fitted_points,1)
player_data$fitted_cost <- round(all_data$fitted_cost)



####################################
###     Incorporate Keepers      ###
####################################
name <- c("Ben Roethlisberger",
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
            "John Brown",
            "T.Y. Hilton",
            "Ryan Tannehill",
            "Mike Evans",
            "Justin Forsett",
            "Alshon Jeffery",
            "Lamar Miller")
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
         "WR",
         "WR",
         "QB",
         "WR",
         "RB",
         "WR",
         "RB")
fitted_cost <- c(43,78,30,7,7,7,8,8,7,7,8,17,7,7,7,7,22,23,12,22,18,9)
mean_cost <- fitted_cost

df_keeper <- data.frame(as.character(name),
                        as.character(pos),
                        fitted_cost,
                        mean_cost)
df_keeper$keeper <- 1
colnames(df_keeper) <- c('name','pos','fitted_cost','mean_cost','keeper')

df_keeper <- df_keeper %>% filter(pos %in% c('QB','RB','WR','TE'))

## need ADP and average projected points

df_keeper2 <- sqldf(
    'select
        a.*
    ,   b.pick
    ,   b.fitted_points
    ,   b.mean_points
    from
        df_keeper as a
    left join
        all_data as b
    on
        a.name = b.name
        ')
df_keeper2 <- df_keeper2 %>% filter(is.na(pick) == 0)

player_data$keeper <- 0
colnames(player_data)
player_data <- player_data %>% select(1,2,4,6,22,3,10,13)

all_data <- union(player_data,df_keeper2)
names = c("Demaryius Thomas","Odell Beckham Jr")
### Re-do constraints
## Set up the objective function ##
objective_model<- all_data$fitted_points
## Set up the objective function ##
## Lay out the constraints ##
# Total salary row
c1_model <- all_data$fitted_cost
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
c7  <- ifelse(all_data$name == 'John Brown',1,0)
c8  <- ifelse(all_data$name == 'Joseph Randle',1,0)
c9  <- ifelse(all_data$name == 'Donte Moncrief',1,0)
c10 <- ifelse(all_data$name == 'Doug Baldwin',1,0)
c11 <- ifelse(all_data$name == 'Arian Foster',1,0)
c12 <- ifelse(all_data$name == 'DeMarco Murray',1,0)
c13 <- ifelse(all_data$name == 'Ben Roethlisberger',1,0)
c14 <- ifelse(all_data$name == 'Nick Foles',1,0)
c15 <- ifelse(all_data$name == "Donte Moncrief",1,0)
c16 <- ifelse(all_data$name == "Lance Dunbar",1,0)
c17 <- ifelse(all_data$name == "T.Y. Hilton",1,0)
c18 <- ifelse(all_data$name == "Ryan Tannehill",1,0)
c19 <- ifelse(all_data$name == "Mike Evans",1,0)
c20 <- ifelse(all_data$name == "Justin Forsett",1,0)
c21 <- ifelse(all_data$name == "Alshon Jeffery",1,0)
c22 <- ifelse(all_data$name == "Lamar Miller",1,0)
c23 <- ifelse(all_data$name %in% names,1,0)
c24 <- rep(1,dim(all_data)[1])

constraints_model <- matrix(rbind(c1_model,c2,c2,c3,c3,c4,c4,c5,c5,c6,c7,c8,c9,c10,c11,c12,
                                  c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24),nrow=28)

## Set up the direction of the constrants ##
direction <- c('<=','>=','<','>=','<','>=','<','>=','<',
               '<=','<=','<=','<=','<=','<=','<=','<=','<=','<=','<=','<=','<=','<=',
               '<=','<=','<=',               
               '==','==')

## Set up the right-hand-side of the equation
rhs <- c(273,2,4,2,4,1,3,3,5,
         2,
         1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
         0,
         8)
solve_model <- lp("max",
            objective_model,
            constraints_model,
            direction,
            rhs,
            all.bin=TRUE,
            num.bin.solns = 1,
            compute.sens = 1)

solution_model <- data.frame(as.character(all_data$name),
                       as.character(all_data$pos),
                       as.character(all_data$pick),
                       all_data$fitted_cost,
                       all_data$fitted_points,
                       all_data$keeper,
                       solve_model$solution) %>% 
    filter(solve_model.solution==1)



colnames(solution_model) <- c("name","pos","pick","cost","points","keeper","solution")



### Model Solution ###
solution_model %>% arrange(pos,pick)
sum(solution_model$cost)
sum(solution_model$points)

