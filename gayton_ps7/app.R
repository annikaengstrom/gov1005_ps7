library(shiny)
library(tidyverse)
library(stargazer)

app_data <- readRDS("result_context.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("GOV1005 PSET 7"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
        selectInput("x",
                    "X-axis:",
                    c(`Polled Republican Advantage` = "rep_adv",
                      `Actual Republican Advantage` = "actual",
                      `Trump Voters` = "trump_pct",
                      `Clinton Voters` = "clinton_pct",
                      `Other Voters, 2016 Presidential Election` = "otherpres16_pct",
                      `Obama Voters` = "obama_pct",
                      `Romney Voters` = "romney_pct",
                      `Other Voters, 2012 Presidential Election` = "otherpres12_pct",
                      `White Voters` = "white_pct",
                      `Black Voters` = "black_pct",
                      `Hispanic Voters` = "hispanic_pct",
                      `Nonwhite Voters` = "nonwhite_pct",
                      `Foreign Born Voters` = "foreignborn_pct",
                      `Female Voters` = "female_pct",
                      `Young Voters (29 and under)` = "age29andunder_pct",
                      `Older Voters (65 and older)` = "age65andolder_pct")),
        checkboxInput("line", label = "Add linear model"),
        htmlOutput("see_table"),
        htmlOutput("regression_table")
      ),
      
      # Show a plot
      mainPanel(
        h3("Summary of Findings"),
        h5("In a simple linear regression relating the error margin (the difference between polled Republican advantage 
           and actual Republican advantage)in Upshot polls to 16 other political and demographic variables, only 
           Republican advantage is significantly correlated with the error margin at the 95% confidence level. However, 
           this may be influenced by a handful of districts with very high predicted Republican advantage but low
           actual Republican advantage."),
        plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     if(input$line == TRUE) {
       ggplot(app_data, aes_string(x = input$x, y = "error")) + geom_jitter() + geom_smooth(method = "lm")
     } 
     else {
       ggplot(app_data, aes_string(x = input$x, y = "error")) + geom_jitter()
     }
   })
   
   output$see_table <- renderUI ({
     if(input$line == TRUE) {
       h5("Please consult the regression table below to see if the relationship is statistically significant:")
     }
   })
   
   output$regression_table <- renderUI({
     if(input$line == TRUE) {
       model <- reactive({ form <- as.formula( paste( "error ~", paste(names(app_data)[names(app_data) %in% input$x], collapse="+")))
       lm(form, data=app_data)
       })
       HTML(stargazer(model(), type = "html"))
     }
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

