library(shiny)
library(dplyr)
library(lpSolve)


## Set up the objective function ##
objective_model <- full_data2$ppg

## Lay out the constraints ##
# Total salary row
c1_fit   <- full_data2$pred_cost
c1_error <- full_data2$pred_cost2
# 2 QBs
c2 <- ifelse(full_data2$pos=='QB',1,0)
# 2 RBs 
c3 <- ifelse(full_data2$pos=='RB',1,0)
# 1 TE 
c4 <- ifelse(full_data2$pos=='TE',1,0)
# 3 WRs 
c5 <- ifelse(full_data2$pos=='WR',1,0)

# No players eliminated yet
c6 <- rep(0,dim(full_data2)[1])

# 1 D/ST
c7 <- ifelse(full_data2$pos == 'DST',1,0)

# 1 K  
c8 <- ifelse(full_data2$pos == 'K',1,0)
  
## Set up the direction of the constrants ##
direction <- c('<=','==','==','==','==','==','==','==')

shinyServer(
    function(input, output) {  
        
        rhs <- reactive({
            c(input$cash,input$qb,input$rb,input$te,input$wr,0,input$dst,input$k)
        })
        
        c6 <- reactive({
            ifelse(full_data2$player %in% input$drafted |
                   full_data2$player %in% input$my_team,
                   1,0)
        })
        
        constraints_model <- reactive({
            matrix(rbind(c1_fit,c2,c3,c4,c5,c6(),c7,c8),nrow=8)
        })

        find_value <- function(player,low,high,start){
          found_value <- NULL
          last_val_in = NULL
          last_val_out = NULL
          salary_constraint <- c1_fit
          while(is.null(found_value) == TRUE){
            guess = floor((low+high)/2)
            salary_constraint[which(full_data2$player==input$candidate)] <- guess
            constraints_auction_block <- reactive({
              matrix(rbind(salary_constraint,c2,c3,c4,c5,c6(),c7,c8),nrow=8)
            })
            solve_auction_block_model <- lp("max",
                                            objective_model,
                                            constraints_auction_block(),
                                            direction,
                                            rhs(),
                                            all.bin=TRUE,
                                            num.bin.solns = 1)$solution
            print("last_val_in:")
            print(last_val_in)
            print("las_val_out:")
            print(last_val_out)
            print("high: ")
            print(high)
            print("low: ")
            print(low)
            print("current guess:")
            print(guess)
            print("in the solution: ")
            print(solve_auction_block_model[which(full_data2$player==input$candidate)] == 1)
            print("--------------------------------------------------------------------------")
            if(solve_auction_block_model[which(full_data2$player==input$candidate)] == 1){
              last_val_in <- guess
              if(is.null(last_val_out) == TRUE){
                low = guess
              }
              else{
                if((last_val_out - last_val_in) == 1){
                  found_value <- last_val_in
                }
                else{
                  low = guess
                }
              }
            }
            else if(solve_auction_block_model[which(full_data2$player==input$candidate)] != 1){
              last_val_out = guess
              if(is.null(last_val_in) == TRUE){
                if(guess == 1){
                  found_value <- 0
                }
                else{
                  high = guess
                }
              }
              else if(is.null(last_val_in) == FALSE){
                if((last_val_out - last_val_in) == 1){
                  found_value = last_val_in
                }
                else{
                  high = guess
                  low = last_val_in
                }
              }
              else if(guess == 1){
                found_value <- 0
              }
            }
          }
          return(found_value)
        }
        
        
  
    
        
        solve_model <- reactive({
            lp(
            "max",
            objective_model,
            constraints_model(),
            direction,
            rhs(),
            all.bin=TRUE,
            num.bin.solns = 1)$solution
        })
        
        
        
        auction_block <- reactive({
          solution <-   data.frame(as.character(full_data2$player),
                                   as.character(full_data2$pos),
                                   full_data2$pred_cost,
                                   full_data2$pred_cost2,
                                   full_data2$ppg,
                                   solve_model()) %>% 
            filter(solve_model()==1) %>%
            data.frame() %>%
            `colnames<-`(c("Player","Position","Est. Cost", "Est. Cost w/ Error","PPG","Solution"))
          
          candidate_pred <- full_data2$pred_cost[which(full_data2$player == input$candidate)]

          if(input$candidate %in% solution$Player){
            value <- find_value(input$candidate, 
                                low = candidate_pred, 
                                high = input$cash)
          }
          else if(!(input$candidate %in% solution$Player)){
            value <- find_value(input$candidate, 
                                low = 1, 
                                high =  candidate_pred)
          }
          return(value)
        })
        output$auction_block_value <- renderText(auction_block())
        

           output$table_solution_model <- renderDataTable({
            data.frame(as.character(full_data2$player),
                       as.character(full_data2$pos),
                       full_data2$pred_cost,
                       full_data2$pred_cost2,
                       full_data2$ppg,
                       solve_model()) %>% 
                filter(solve_model()==1) %>%
               data.frame() %>%
                `colnames<-`(c("Player","Position","Est. Cost", "Est. Cost w/ Error","PPG","Solution"))%>%
               arrange(Position, PPG)
               },
            options=list(searching=FALSE, paging = FALSE))


      
       # output$table_my_team <- renderDataTable({
       #      full_data2 %>%
       #           filter(player %in% input$my_team) %>%
       #           data.frame()
       #  },
       #  options=list(searching=FALSE, paging = FALSE))
# 
#        
#        output$model_ppg <- renderText({
#          
#          model_ppg <- data.frame(as.character(full_data2$player),
#                                  as.character(full_data2$pos),
#                                  full_data2$pred_cost,
#                                  full_data2$pred_cost2,
#                                  full_data2$ppg,
#                                  solve_model()) %>%
#            filter(solve_model_error() == 1) %>%
#            data.frame() %>%
#            `colnames<-`(c("Player","Position","Est. Cost", "Est. Cost w/ Error","PPG","Solution")) %>%
#            arrange(Position,PPG) %>%
#            summarise(ttl_ppg = sum(PPG))
#          
#          myteam_ppg <- full_data2 %>%
#            filter(player %in% input$my_team) %>%
#                   data.frame() %>%
#                     summarise(my_team_ppg = sum(ppg))
#          
#          round(as.numeric(model_ppg + myteam_ppg),1)
#          
#          
#        })
#        
        output$table_all_players <- renderDataTable({
            full_data2 %>%
                data.frame()
        })
        
})