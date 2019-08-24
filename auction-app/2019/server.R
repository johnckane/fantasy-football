library(shiny)
library(dplyr)
library(lpSolve)


## Set up the objective function ##
objective_model <- data$ppg

## Lay out the constraints ##
# Total salary row

# 2 QBs
c2 <- ifelse(data$pos=='QB',1,0)
# 2 RBs 
c3 <- ifelse(data$pos=='RB',1,0)
# 1 TE 
c4 <- ifelse(data$pos=='TE',1,0)
# 3 WRs 
c5 <- ifelse(data$pos=='WR',1,0)

# No players eliminated yet
c6 <- rep(0,dim(data)[1])

# 1 D/ST
c7 <- ifelse(data$pos == 'D/ST',1,0)

# 1 K  
c8 <- ifelse(data$pos == 'K',1,0)
  

## Bye Week Constraints
## No more than 1 at each position on BYE
table(data$bye)

bye_w5  <- ifelse(data$bye == 5,1,0)
bye_w6  <- ifelse(data$bye == 5,1,0)
bye_w7  <- ifelse(data$bye == 5,1,0)
bye_w8  <- ifelse(data$bye == 5,1,0)
bye_w9  <- ifelse(data$bye == 5,1,0)
bye_w10 <- ifelse(data$bye == 5,1,0)
bye_w11 <- ifelse(data$bye == 5,1,0)


c9 <- bye_w5*c2 
c10 <- bye_w6*c2
c11 <- bye_w7*c2 
c12 <- bye_w8*c2
c13 <- bye_w9*c2 
c14 <- bye_w10*c2
c15 <- bye_w11*c2

c16 <- bye_w5*c3 
c17 <- bye_w6*c3
c18 <- bye_w7*c3 
c19 <- bye_w8*c3
c20 <- bye_w9*c3 
c21 <- bye_w10*c3
c22 <- bye_w11*c3

c23 <- bye_w5*c4 
c24 <- bye_w6*c4
c25 <- bye_w7*c4 
c26 <- bye_w8*c4
c27 <- bye_w9*c4 
c28 <- bye_w10*c4
c29 <- bye_w11*c4

c30 <- bye_w5*c5 
c31 <- bye_w6*c5
c32 <- bye_w7*c5 
c33 <- bye_w8*c5
c34 <- bye_w9*c5 
c35 <- bye_w10*c5
c36 <- bye_w11*c5

c37 <- bye_w5*c7 
c38 <- bye_w6*c7
c39 <- bye_w7*c7 
c40 <- bye_w8*c7
c41 <- bye_w9*c7 
c42 <- bye_w10*c7
c43 <- bye_w11*c7

c44 <- bye_w5*c8 
c45 <- bye_w6*c8
c46 <- bye_w7*c8 
c47 <- bye_w8*c8
c48 <- bye_w9*c8 
c49 <- bye_w10*c8
c50 <- bye_w11*c8



## Age Constraints
age30 <- ifelse(data$age > 30,1,0)

c51 <- age30*c3
c52 <- age30*c5

gt10 <- ifelse(data$pred_cost > 5,1,0)

qb_rb_wr <- ifelse(c2|c3|c5,1,0)

c54 <- gt10*qb_rb_wr


## Total BYE constraints
c55 <- bye_w5*(c2+c3+c4+c5+c6+c7+c8) 
c56 <- bye_w6*(c2+c3+c4+c5+c6+c7+c8) 
c57 <- bye_w7*(c2+c3+c4+c5+c6+c7+c8)  
c58 <- bye_w8*(c2+c3+c4+c5+c6+c7+c8) 
c59 <- bye_w9*(c2+c3+c4+c5+c6+c7+c8)  
c60 <- bye_w10*(c2+c3+c4+c5+c6+c7+c8) 
c61 <- bye_w11*(c2+c3+c4+c5+c6+c7+c8) 


# For lineup optimization
week <- c(1:13)
week_df <- data.frame(week, dummy = 1)

limits_df <- data.frame(pos = c("QB","RB","WR","TE","D/ST","K"),
                        lim = c(2,2,3,1,1,1),
                        stringsAsFactors = FALSE)


## Set up the direction of the constrants ##
direction <- c('<=','==','==','==','==','==','==','==', #C8
               '<=','<=','<=','<=','<=','<=','<=', #QB byes
               '<=','<=','<=','<=','<=','<=','<=', #RB byes
               '<=','<=','<=','<=','<=','<=','<=', #TE byes
               '<=','<=','<=','<=','<=','<=','<=', #WR byes
               '<=','<=','<=','<=','<=','<=','<=', #DST byes
               '<=','<=','<=','<=','<=','<=','<=',  #K byes
               '==','==',# age constraints on RB and WR
               '==','==', #my team requirement,cost min for qb/rb/wr
               '<=','<=','<=','<=','<=','<=','<=') # max allowable byes
shinyServer(
    function(input, output) {  
        
        rhs <- reactive({
            c(input$cash,input$qb,input$rb,input$te,input$wr,0,input$dst,input$k,
              rep(1,42), # bye week constraints
              0,0, # age constraints
              length(input$my_team),#my team constraint
              input$qb+input$wr+input$rb, # cost requirement
              rep(input$max_byes,7)) #max allowable bye per week
        })  

        c1_fit   <- reactive({
          ifelse(data$player %in% input$my_team,0,
                 data$pred_cost)
        })

        
        
        c6 <- reactive({
            ifelse(data$player %in% input$drafted,1,0)
        })
        
        # Any body on my team must be selected
        
        c53 <- reactive({
          ifelse(data$player %in% input$my_team,1,0)
        })
        
        constraints_model <- reactive({
            matrix(rbind(c1_fit(),c2,c3,c4,c5,c6(),c7,c8,
                         c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,
                         c29,c30,c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,c41,c42,c43,c44,c45,c46,c47,c48,
                         c49,c50,c51,c52,c53(),c54,c55,c56,c57,c58,c59,c60,c61),nrow=61)
        })

        find_value <- function(player,low,high,start){
          found_value <- NULL
          last_val_in = NULL
          last_val_out = NULL
          salary_constraint <- c1_fit()
          while(is.null(found_value) == TRUE){
            guess = floor((low+high)/2)
            salary_constraint[which(data$player==input$candidate)] <- guess
            constraints_auction_block <- reactive({
              matrix(rbind(salary_constraint,c2,c3,c4,c5,c6(),c7,c8,
                           c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,
                           c29,c30,c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,c41,c42,c43,c44,c45,c46,c47,c48,
                           c49,c50,c51,c52,c53(),c54,c55,c56,c57,c58,c59,c60,c61),nrow=61)
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
            print(solve_auction_block_model[which(data$player==input$candidate)] == 1)
            print("--------------------------------------------------------------------------")
            if(solve_auction_block_model[which(data$player==input$candidate)] == 1){
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
            else if(solve_auction_block_model[which(data$player==input$candidate)] != 1){
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
          solution <-   data.frame(as.character(data$player),
                                   as.character(data$pos),
                                   data$pred_cost,
                                   data$ppg,
                                   solve_model()) %>%
            filter(solve_model()==1) %>%
            data.frame() %>%
            `colnames<-`(c("Player","Position","Est. Cost", "PPG","Solution"))

          candidate_pred <- data$pred_cost[which(data$player == input$candidate)]

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
            data.frame(as.character(data$player),
                       as.character(data$pos),
                       data$pred_cost,
                       data$ppg,
                       solve_model()) %>%
                filter(solve_model()==1) %>%
               data.frame() %>%
                `colnames<-`(c("Player","Position","Est. Cost","PPG","Solution"))%>%
               arrange(Position, PPG)
               },
            options=list(searching=FALSE, paging = FALSE))


       my_team_w_weeks <- reactive({
         data %>%
           mutate(dummy = 1) %>%
           filter(player %in% input$my_team) %>%
           full_join(.,week_df, by = "dummy") %>%
           mutate(ppg = replace(ppg,bye==week,0)) %>%
           arrange(week,pos,desc(ppg)) %>%
           group_by(week,pos) %>%
           mutate(obs = row_number()) %>%
           inner_join(limits_df, by = c("pos")) %>%
           filter(obs <= lim) %>%
           group_by(week) %>%
           summarise(total = round(sum(ppg)))
       })
       

      optimal_w_weeks <- reactive({
        data.frame(as.character(data$player),
                   as.character(data$pos),
                   data$pred_cost,
                   data$ppg,
                   data$bye,
                   solve_model()) %>%
          filter(solve_model()==1) %>%
          data.frame() %>%
          `colnames<-`(c("player","pos","est_cost","ppg","bye","solution"))%>%
          arrange(pos,ppg) %>%
          mutate(dummy = 1) %>%
          full_join(.,week_df, by = "dummy") %>%
          mutate(ppg = replace(ppg,bye==week,0)) %>%
          arrange(week,pos,desc(ppg)) %>%
          group_by(week,pos) %>%
          mutate(obs = row_number()) %>%
          inner_join(limits_df, by = c("pos")) %>%
          filter(obs <= lim) %>%
          group_by(week) %>%
          summarise(total_opt = round(sum(ppg)))
      })
      
      output$my_lineup_optimized <- renderDataTable({
          optimal_w_weeks() %>%
          left_join(my_team_w_weeks(),by="week") %>%
          `colnames<-`(c("Week","Optimal Model","Current Team"))
        },
      options=list(searching=FALSE, paging = FALSE))

       



        output$table_all_players <- renderDataTable({
            data %>%
                data.frame()
        })
        
})