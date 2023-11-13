
# Packages and test data
library(shiny)
library(tidyverse)
library(forecast)
library(xts)
library(gt)

# Source
Sys.setlocale("LC_CTYPE")
source("~/budgetForecasting/2023-10-23 importing_example_data.R")  # Data
source("~/budgetForecasting/appendix.R") # Appendix with functions
source("~/budgetForecasting/dataStore.R")
source(here::here("tsModeling.R"))
source("~/budgetForecasting/modelselection.R")

# UTF
Sys.setlocale("LC_CTYPE")

# Test data
df1 <- nav_data$df1 |> 
    mutate( age = str_remove(age, "År") |> str_trim( side = "both"),
            cat = str_c(sex, " ", age)
            ) |> 
    filter( ! str_detect(str_to_lower(age), "uopp") 
            ) |> 
    arrange( sex, age)


# Defining range for input selection in the app 
cat <- setNames(unique(df1$cat), unique(df1$cat) ) ## category for the data
h <- c(1:24) # Length for data



# user interface
ui <- fluidPage(
    titlePanel("Forecast Machine"),
        #
    navlistPanel(
        widths = c(3,9),
        id = "Info",
        "Tabs",
    # Tab 1.
    tabPanel( "Tab 1 import data",
              helpText("If 'imported data' is selected, the 'choose'-example function on tab 1 wont work"),
                  radioButtons("data_source", "Choose Data Source:",
                               choices = c("Imported Data", "Integrated Data"),
                               selected = "Imported Data"),
                  fileInput("file", "Upload your data (if applicable):"),
              helpText("Imported data needs to be structured as the example bellow:"),
              mainPanel(
                    tableOutput("display_table")
                    )
              ),
    tabPanel( "Tab 2 Spesify the data",
              fluidRow(
                  # Choose which data to look at
                  column(5, selectInput( "cat", "Choose data", choices = cat, width = "60%" )  ),
                  column(4, sliderInput("periods", "lenght of data (filter start 2015)", min =  ymd("2016-01-01"),
                                        # This needs to be updated 
                                        max = max(df1$date),
                                        value =  c( min(df1$date),max(df1$date)), ticks = T) 
                         ),
                  column(3, selectInput( "calc", "Statistics", choices = c("mean", "sum"), width = "60%") )
                ),
              # Choose which periods we are looking at
              fluidRow(
                  column(8, h4("Table 1: Yearly development", class = "custom-title"), 
                         tags$head(
                             tags$style(HTML(
                                 "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                                    )
                                 ) 
                            )
                         ),
                  column(8, gt_output("regnskapTabell") )
                  #column(8, tableOutput("regnskapTabell") )
                  ),
              fluidRow(
                  column(8, h4("Table 2: Monthly development", class = "custom-title") ),
                  column(12, tableOutput("mndTabell") )
                  ),
              # Look at the data with a graph
              fluidRow(
                  column(10, plotOutput("tsplot") )
                )
              ),
    tabPanel( "Tab 3 Forecasting",
              fluidRow(
                  # Choose which data to look at
                  column( 8, selectInput( "len", "Choose forecast length", choices = h)  ),
                  #column(4, selectInput("season", "Include seasonal effect in model", choices = c("additive", "multiplicative"), width = "80%" )),
                  column( 4, selectInput( "model_type", "Arima or Holt-Winter", choices = c("arima", "hw") ) )
                  ),
              # tabPanel( "",
              #           fluidRow(
              #               # Tabell av AIC mm.
              #           )),
              fluidRow(
                  # Choose which data to look at
                  column(8, plotOutput("forecastPlot") )
              ),
                  # Choose which periods we are looking at
              fluidRow(
                  column(7, h4("Table 3: Forecast values", class = "custom-title") ),
                  column(5, h4("Table 4: Yearly development", class = "custom-title"))
              ),
              fluidRow(
                  column(7, tableOutput("forecastTable") ),
                  column(5, tableOutput("yearlyChange"))
                )
              ),
        tabPanel("Tab 4 Modelevaluation",
                 # Choose which periods we are looking at
                 fluidPage(
                     fluidRow(
                         column(4, selectInput( "model_type_eval", "Model selection", choices = c("arima", "hw"), width = "80%") ),
                         # Ikke lagt inn enda
                         column(4, sliderInput( "ts_length", "ts data length", min =  0,  max = 1, value =  c(0,1), step = 0.1,  ticks = T) )
                         ),
                     #     column(4, sliderInput("acf_lags",
                     #                           "Nr lags in ACF:",
                     #                           min = 4,
                     #                           max = 50,
                     #                           value = 10) )
                     # ),
                     fluidRow(
                         column(6, plotOutput("residualHistogram")),
                         column(4, sliderInput("hist_bins",
                                               "Number of bins in histogram:",
                                               min = 1,
                                               max = 50,
                                               value = 30) ),
                         column(6, 
                                br(),
                                p("The residuals' mean should be near zero.
                                  A normal distribution should be centered at zero.
                                  The Shapiro-Wilk test assesses the normality assumption.\n",
                                  "The null hypothesis assumes normality in errors.\n",
                                  "A low p-value raises doubts about normality."),
                                br(),
                                verbatimTextOutput("histogramText")
                                )
                     ),
                     fluidRow(
                         column(6, plotOutput("acf") ),
                         column(4, sliderInput("acf_lags",
                                               "Nr lags in ACF:",
                                               min = 4,
                                               max = 50,
                                               value = 10) ),
                         column(6, 
                                br(),
                                p("The residuals (e) should not be series correlated.
                                In the ACF-plot, this corresponds to no columns crossing the blue horizontal line,
                                indicates that the residuals are white noise (iid).
                                The Breusch-Godfrey (Lagrange Multiplier) test () cbe used as a formal test. 
                                A small p-value (for instance, p-value < .05) indicates there is significant autocorrelation remaining in the residuals. 
                                  "),
                                br() ),
                         column(6, verbatimTextOutput("acfText") )
                     ),
                     fluidRow(
                         wellPanel( h3("Regression results"),
                                    p("Her er tekst som skal forklare noen paramter til modellen")),
                         column(10, verbatimTextOutput("modelsummary"))
                     )
                    )
                 ),

# UI:Tab 5 (model selection) ----------------------------------------------

        tabPanel("Tab 5 Modelselections",
                 fluidRow(
                     column(5, selectInput( "cat", "Choose data", choices = cat, width = "60%" )  ),
                     column(6, sliderInput("train_range", 
                                           label = "Selecting part to training data",
                                           min = 0, max = 0.95, value = c(0, 0.8) )
                     ) 
                 ),
                 fluidRow(
                     column(6,h4("Training and test data printed", class = "custom-title"), 
                            tags$head(
                                tags$style(HTML(
                                    "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                                )
                                )
                            )),
                     column(10, verbatimTextOutput("testText") )
                 ),
                 fluidRow( 
                     column(10, plotOutput("testPlot") )
                 ),
                 fluidRow( 
                     column(10, tableOutput("testTable") )
                 )
             ),
    # Exporting data
    tabPanel("Tab 6 Export data",
             fluidRow(
                 column(6,h4("Table 1: Yearly development", class = "custom-title"), 
                        tags$head(
                            tags$style(HTML(
                                "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                                    )
                                )
                            )),
                 column(6, downloadButton(outputId = "downloadData1",label =  "Download") )
                 ),
             fluidRow(
                 column(6,h4("Table 2: Monthly change", class = "custom-title"), 
                        tags$head(
                            tags$style(HTML(
                                "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                            )
                            )
                        )),
                 column(6, downloadButton(outputId = "downloadData2",label =  "Download") )
             ),
             fluidRow(
                 column(6,h4("Table 3: Forecast h-periods", class = "custom-title"), 
                        tags$head(
                            tags$style(HTML(
                                "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                            )
                            )
                        )),
                 column(6, downloadButton(outputId = "downloadData3",label =  "Download") )
             )
            )
        )
)

# Server
server <- function(input, output) {
    

# Tab 1: Import data ------------------------------------------------------
    
    # Data
    data <- reactive({
        # If importing data is selected
        if (input$data_source == "Imported Data") {
            req(input$file)
            dat <- readxl::read_excel(input$file$datapath, sheet = 1) |> 
                filter( 
                    between(date, min(input$periods), max(input$periods) )
                )
            # Else use test data integreded in App (df) 
            } else {
                dat <- df1 |>
                    filter( cat == input$cat,
                            between(date, min(input$periods), max(input$periods) )
                    )
            }
        
        # Data is an object DataStore. 
        # DataStore contain df and ts data. 
        # Data can be return by calling dfReturn() and tsReturn()
        dataStore$new( df = dat,
                       value = dat$value,
                       date = dat$date
        )
    })
    
    # Data for model selection
    data_to_model_selection <- reactive({ 
            # If importing data is selected
            if (input$data_source == "Imported Data") {
                req(input$file)
                dat <- readxl::read_excel(input$file$datapath, sheet = 1) |>
                    filter(
                        between(date, min(input$periods), max(input$periods) )
                    )
                # Else use test data integreded in App (df)
            } else {
                dat <- df1 |>
                    filter( cat == input$cat)
            }
        
            dat_mod_select <- dataStore$new( df = dat,
                           value = dat$value,
                           date = dat$date
            )
    
            dat_mod_select$tsSplit(
                start_train = input$partUsedToTrain[1],
                  end_train = input$partUsedToTrain[2]
            )   
        })
        
    # Models
       models <-  reactive({ tsModeling$new( ts = data()$tsReturn(), h = input$len )$doModeling(type = input$model_type) })       # For Tab-1: Forecast
    modeleval <-  reactive({ tsModeling$new( ts = data()$tsReturn(), h = input$len )$doModeling(type = input$model_type_eval) })  # For Tab-3: Evaluation of model
    

# Model Selection ---------------------------------------------------------
    
    # Data for model selection
    data_to_model_selection <- reactive({ 
        
        dat <- df1 |>
            filter( cat == input$cat)
        # }
        
        dataStore$new(
            df = dat,
            value = dat$value,
            date = dat$date
        )
        
    })
    
    #
    listTrainTest <- reactive({ 
        
        data_to_model_selection()$tsSplit( start_train = input$train_range[1],
                                           end_train = input$train_range[2] )
        
    })
    
    
    
# Tab 2: Data description ---------------------------------------------------------------

    # Example data displayed first page:
    # For illustration of how the data should be structured
    output$display_table <- renderTable({
        tibble::tibble( date = seq.Date(from = ymd("2019-01-01"), length.out = 5, by = "month") |> as.character(),
                        value = sample( x = c(100:1000), size = 5, replace = T )
                        ) 
    })
    
    
    
    
    # Table 1: Yearly numbers
    output$regnskapTabell <- renderTable(
        { data()$dfReturn() |>
                fn_desc_binded( f = eval( parse( text = input$calc) ) ) |>
                rename( !! input$calc := 2)
                },
        res = 96
    )

    
    # Download (Tab6) 1
    output$downloadData1 <- downloadHandler(
        filename = function() {
            paste0( lubridate::today(),"_tabell1.xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(x = data()$dfReturn() |>
                                   fn_desc_binded( f = eval( parse( text = input$calc) ) ) |>
                                   rename( !! input$calc := 2),
                               path = file)
        }
    )
    
    
    
    # Table 2: Monthly development data 
    output$mndTabell <- renderTable( { data()$dfReturn() |> fn_monthly_table() }, res = 96 )
    
    # Download (Tab6) 2
    output$downloadData2 <- downloadHandler(
        filename = function() {
            paste0( lubridate::today(),"_tabell2.xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(x = data()$dfReturn() |> fn_monthly_table(),
                                path = file)
        }
    )
    
    
    
    
    output$tsplot <- renderPlot(
        {ggplot( data = data()$dfReturn(),
                 aes(x = date, y = value)
                 ) +
                geom_line(color = "steelblue") +
                theme_minimal(base_size = 14)
                }, res = 96
    )
    
    
    
    

# Tab 3: Forecast ---------------------------------------------------------
    
    # 1)
    # Result from forecast Holt Winter model in table
    output$forecastTable <- renderTable(
        { 
            models()$fc |> mutate( date = str_sub(date, 1, 7) )
            },
        res = 96
    )
    
    # Download (Tab6) 3
    output$downloadData3 <- downloadHandler(
        filename = function() {
            paste0( lubridate::today(),"_tabell3.xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(x = models()$fc |> mutate( date = str_sub(date, 1, 7) ),
                                path = file)
        }
    )
    
    
    # 2) 
    # Yearly change
    output$yearlyChange <- renderTable(
        { 
            data()$dfReturn() |> select(date, value) |> 
                bind_rows( models()$fc |> 
                               mutate( date = ymd(date), value = point)
                               ) |>
                mutate( year = year(date) ) |> 
                summarise( mean = mean(value), .by = year) |> 
                mutate( `percent change` = mean/lag(mean)-1,
                        year = as.integer(year)
                        )    
                
            #odels()$doModeling( type = input$model_type )$fc
        },
        res = 96
    )
    
    # 3)  The Plot
    output$forecastPlot <- renderPlot( models()$obj.fc |>
                                           forecast::autoplot() + 
                                           theme_minimal(base_size = 10) +
                                           theme( plot.title = element_text( size = 10)),
                                       res = 96
                                       )
    
   

# Tab 4: Model evaluation --------------------------------------------------

    # model summary
    output$modelsummary <- renderPrint({
        summary(modeleval()$model |> summary() )
    })
    
    # Histogram/Normality test 
    output$histogramText <- renderPrint({ cat("P-value (Shapiro-Wilk): ", round( stats::shapiro.test( modeleval()$model$residuals)$p.value, 2)*100, "%" ) })
    
    output$acfText <- renderPrint({
        paste0("P-value (LM-test): ",lmtest::bgtest( lm( modeleval()$model$residuals ~1))$p.value |> round(2)*100, " %" )
    })    
    
    # Model evaluation
    output$residualHistogram <- renderPlot({
        
        x <- modeleval()$model$residual    
        bins <- seq(min(x), max(x), length.out = input$hist_bins + 1 )
        
        hist( x, 
              main = paste0("Model type: ",str_to_upper( modeleval()$name.model ) ),
              xlab = "residuals",
              breaks = bins
              )    },
        res = 96
    )
    
    # Model evaluation
    output$acf <- renderPlot( 
        # First convert data to integers
        coredata( modeleval()$model$residual ) |>
            # Plot the acf
            acf( 
                # Main title
                main = paste0("Model type: ",str_to_upper( modeleval()$name.model ) ),
                # Input for numbers of lags 
                lag.max = input$acf_lags
                 ),
        res = 96
    )
    

# Tab 5: Model selections  ---------------------------------------------------------

    output$testTable <- renderTable({
        fn_rmse_table(
            models = fn_train_model(
                train = listTrainTest()$train,
                test  = listTrainTest()$test ) |> map("mean"),
            ts_test = listTrainTest()$test
        )
    })
    
    output$testText <- renderPrint({
        
        listTrainTest()
    }
    )
    
    
    output$testPlot <- renderPlot(
        {
            autoplot(
                data_to_model_selection()$tsReturn()
            ) +
                autolayer(
                    map(
                        fn_train_model( train = listTrainTest()$train,
                                        test = listTrainTest()$test ),
                        \(x) x$mean )$arima,
                    alpha = 0.7,
                    linetype = 2,
                    color = "darkblue"
                ) +
                autolayer(
                    map(
                        fn_train_model( train = listTrainTest()$train,
                                        test = listTrainTest()$test ),
                        \(x) x$mean )$`Holt winter`,
                    alpha = 0.7,
                    linetype = 2,
                    color = "darkred"
                ) +
                labs( title = "How well ARIMA (dark blue) and Holt Winter predict test data", y = "value") +
                theme_light( base_size = 14)
            
        }
    )
    # 
}


# 
shinyApp(ui, server)






