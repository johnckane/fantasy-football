library(shiny)
library(rCharts)

shinyUI(pageWithSidebar(
    
    
    
numericInput("qb", label = h3("QBs to draft"), value = 0)
numericInput("rb", label = h3("RBs to draft"), value = 0)
numericInput("wr", label = h3("WRs to draft"), value = 0)
numericInput("te", label = h3("TEs to draft"), value = 0)

    
    
    headerPanel('DataTables in Shiny with rCharts'),
    sidebarPanel(
        selectInput('dataset', 'Choose DataSet',
                    c('mtcars', 'iris', 'faithful')            
        ),
        selectInput('pagination', 'Choose Pagination',
                    c('two_button', 'full_numbers')            
        )
    ),
    mainPanel(
        chartOutput('mytable', 'datatables') 
    )
))

