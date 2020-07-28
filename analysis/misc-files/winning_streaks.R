library(googlesheets)
library(dplyr)
library(stringr)
sheet <- gs_title("Bad Newz Archives")


owner <- sheet %>% gs_read(ws = "Owner-Team Name")
games <- sheet %>% gs_read(ws = "Regular Season Games")

owner_game <- left_join(games,owner,by=c("year","team"))

## Arrange data and create win, loss and tie variables
data <- arrange(owner_game,game_id,points)
data$W <- rep(0,dim(data)[1])
data$L <- rep(0,dim(data)[1])
data$T <- rep(0,dim(data)[1])
data$PA <- rep(0,dim(data)[1])
data$result <- rep(" ",dim(data)[1])
data$last_of_streak <- rep(0,dim(data)[1])

### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(data)[1],by=2)){
    
    data$PA[i]   <- data$points[i+1]
    data$PA[i+1] <- data$points[i]
    
    data$Opponent[i] <- data$owner[i+1]
    data$Opponent[i+1] <- data$owner[i]
    
    if(data$points[i] < data$points[i+1]){
        data$L[i] <- 1
        data$W[i+1] <- 1
        data$result[i] <- "L"
        data$result[i+1] <- "W"
        
    }
    if(data$points[i] == data$points[i+1]){
        data$T[i] <- 1
        data$T[i+1] <- 1
        data$result[i] <- "T"
        data$result[i+1] <- "T"
    }
    
}


data <- data %>%
    arrange(year,week,desc(points)) %>%
    group_by(year,week) %>%
    mutate(rk = rank(points,ties.method="min")) 

# Now to calculate and add up proportional wins
data$pw<- ifelse(data$year==2009,(data$rk-1)/9,(data$rk-1)/11)


data2 <- data %>% 
    ungroup() %>% 
    arrange(owner,year,week) %>%
    group_by(owner) %>%
    mutate(streak = c(1,diff(W)),
           last_of_streak = ifelse(lead(W) != W,1,0),
           row_number = row_number())


## I think the way to solve this is to use last_of_streak == 1, the difference in rows
## between where they are equal to 1 is the length of the streak
last <- filter(data2,last_of_streak==1)

data2$length <- ifelse(data2$last_of_streak == 0,
                       NA,
                       diff(data2$row_number[data2$last_of_streak == 0]))
cbind(data2$row_number,data2$result,data2$last_of_streak,data2$length)

                                                
                                                     
                        
        
        

