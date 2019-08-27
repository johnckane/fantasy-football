library(shiny)
library(dplyr)
library(lpSolve)



load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/qb_single_obs_per_cluster.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/rb_single_obs_per_cluster.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/wr_single_obs_per_cluster.Rda")
load("/home/john/projects/fantasy-football/data/clustering-data/smoothed-and-clustered/te_single_obs_per_cluster.Rda")

df <- bind_rows(qb_single_obs_per_cluster %>% mutate(position = 'QB',
                                                     avg_cost = round(avg_cost),
                                                     avg_ppg = round(avg_ppg,1)),
                rb_single_obs_per_cluster %>% mutate(position = 'RB',
                                                     avg_cost = round(avg_cost),
                                                     avg_ppg = round(avg_ppg,1)),
                wr_single_obs_per_cluster %>% mutate(position = 'WR',
                                                     avg_cost = round(avg_cost),
                                                     avg_ppg = round(avg_ppg,1)),
                te_single_obs_per_cluster %>% mutate(position = 'TE',
                                                     avg_cost = round(avg_cost),
                                                     avg_ppg = round(avg_ppg,1)))




shinyServer(
    function(input, output) {  
        
      
      df_expanded <- reactive({
        df[rep(row.names(df), c(input$qb_cluster1,
                                input$qb_cluster2,
                                input$qb_cluster3,
                                input$qb_cluster4,
                                input$qb_cluster5,
                                input$rb_cluster1,
                                input$rb_cluster2,
                                input$rb_cluster3,
                                input$rb_cluster4,
                                input$rb_cluster5,
                                input$rb_cluster6,
                                input$rb_cluster7,
                                input$rb_cluster8,
                                input$rb_cluster9,
                                input$rb_cluster10, 
                                input$wr_cluster1,
                                input$wr_cluster2,
                                input$wr_cluster3,
                                input$wr_cluster4,
                                input$wr_cluster5,
                                input$wr_cluster6,
                                input$wr_cluster7,
                                input$wr_cluster8,
                                input$wr_cluster9,
                                input$wr_cluster10, 
                                input$te_cluster1,
                                input$te_cluster2,
                                input$te_cluster3,
                                input$te_cluster4,
                                input$te_cluster5)),]     
      })
      rhs <- reactive({
            c(input$budget,input$qbs_needed,input$rbs_needed,input$wrs_needed,input$tes_needed)
      })  
      
      
      c_salary <- reactive({df_expanded()$avg_cost})
      # 2 QBs
      c_qb <- reactive({ifelse(df_expanded()$position=='QB',1,0)})
      # 2 RBs 
      c_rb <- reactive({ifelse(df_expanded()$position=='RB',1,0)})
      # 1 TE 
      c_te <- reactive({ifelse(df_expanded()$position=='TE',1,0)})
      # 3 WRs 
      c_wr <- reactive({ifelse(df_expanded()$position=='WR',1,0)})

      
      constraints <- reactive({
        matrix(rbind(c_salary(),
                     c_qb(),
                     c_rb(),
                     c_wr(),
                     c_te()),
               nrow = 5)})
      
      
      
      ## Set up the objective function ##
      objective_model <- reactive({df_expanded()$avg_ppg})
      
      direction <- c('<','==','==','==','==')
      
       solve_model <- reactive({
            lp(
            "max",
            objective_model(),
            constraints(),
            direction,
            rhs(),
            all.bin=TRUE,
            num.bin.solns = 1)$solution 
        })

       output$table_solution_model <- renderDataTable({
            data.frame(as.character(df_expanded()$position),
                       as.character(df_expanded()$cluster),
                       df_expanded()$avg_ppg,
                       df_expanded()$avg_cost,
                       solve_model()) %>%
                filter(solve_model()==1) %>%
               data.frame() %>%
               select(-5) %>%
                `colnames<-`(c("Position","Cluster","PPG","Cost"))%>%
               arrange(Position, Cluster)
               },
            options=list(searching=FALSE, paging = FALSE))
})