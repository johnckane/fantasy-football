setwd('/home/john/stats_corner/record_book')
library(googlesheets)
library(dplyr)
gs_ls()
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

### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(data)[1],by=2)){
    
    data$PA[i]   <- data$points[i+1]
    data$PA[i+1] <- data$points[i]
    
    if(data$points[i] < data$points[i+1]){
        data$L[i] <- 1
        data$W[i+1] <- 1
    }
    if(data$points[i] == data$points[i+1]){
        data$T[i] <- 1
        data$T[i+1] <- 1
    }
}


data <- data %>%
    arrange(year,week,desc(points)) %>%
    group_by(year,week) %>%
    mutate(rk = rank(points,ties.method="min"))

# Now to calculate and add up proportional wins
data$pw<- ifelse(data$year==2009,(data$rk-1)/9,(data$rk-1)/11)





shinyServer(function(input, output, session){
    mydata <- reactive({
        switch(input$dataset, mtcars = mtcars, iris = iris,
               faithful = data.table(faithful)
        )
    })
    output$mytable <- renderChart2({
        dTable(mydata(), sPaginationType = input$pagination)
    })
})
