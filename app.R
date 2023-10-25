

library(shiny)
library(tidyverse)

# ui: https://shiny.posit.co/r/gallery/reactive-programming/reactivity/

# Data 
source("~/budgetForecasting/2023-10-23 importing_example_data.R")

Sys.setlocale("LC_CTYPE")
df1 <- nav_data$df1 |> 
    mutate( age = str_remove(age, "år") |> str_trim( side = "both"),
            cat = str_c(sex, " ", age)
            ) |> 
    filter( ! str_detect(str_to_lower(age), "uopp") 
            ) |> 
    arrange( sex, age)

# 
cat <- setNames(unique(df1$cat), unique(df1$cat) ) #



ui <- fluidPage(

    titlePanel("Forecast analysis"),
    
    navlistPanel(
        widths = c(2, 10),
        id = "Info",
        "Instillinger",
    tabPanel( "Tab 1 Spesify the data",
              fluidRow(
                  # Choose which data to look at 
                  column(4, selectInput( "cat", "Choose category", choices = cat, width = "80%") ),
                  column(4, sliderInput("periods", "lenght of data", min =  min(df1$date),  max = max(df1$date), value =  c(min(df1$date),max(df1$date)), ticks = F),
                        )
                ),
              # Choose which periods we are looking at
              fluidRow(
                  column(8, tableOutput("regnskapTabell") )
                  ),
              # Look at the data with a graph
              fluidRow(
                  column(10, plotOutput("tsplot") )
              )
              ),
    #
    tabPanel( "Tab 2 Modeling"
              
              )
        )
    )
    
server <- function(input, output) {
    
    # 
    data <- reactive({
        df1 |> 
            filter( cat == input$cat,
                    between(date, min(input$periods), max(input$periods) )
                    )
    })

    # 
    output$regnskapTabell <- renderTable(
        { data() |> fn_monthly_table() },
        res = 96
        )
    
    output$tsplot <- renderPlot(
        {ggplot( data = data(),
                 aes(x = date, y = value)
                 ) +
                geom_line(color = "steelblue")
                }, res = 96
    )
}


shinyApp(ui, server)



# "Descriptive data",
# tabPanel("Descriptive data",
#          # fluidRow( column(12, tableOutput("Table") )
#          # ),
#          # fluidRow(
#          #     column(10, plotOutput("Plot"))
#          # )
#     ),
# tabPanel("Modeling"),
#     fluidRow( column(12, tableOutput("Statinarity") )),
#     fluidRow( column(12, tableOutput("Forecasting") ))       
