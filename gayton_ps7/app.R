library(shiny)
library(tidyverse)

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
        checkboxInput("line", label = "Add linear model")
      ),
      
      # Show a plot
      mainPanel(
         plotOutput("distPlot"),
         verbatimTextOutput("modelSummary")
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
   
   
   output$modelSummary <- renderPrint({
     if(input$line == TRUE) {
       model <- reactive({ form <- as.formula( paste( "error ~", paste(names(app_data)[names(app_data) %in% input$x], collapse="+")))
       print(form)
       lm(form, data=app_data)
       })
       summary(model())
     }
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

