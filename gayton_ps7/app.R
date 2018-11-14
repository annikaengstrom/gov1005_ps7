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
      ),
      
      # Show a plot
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     ggplot(app_data, aes(x = error, y = rep_adv)) + geom_point()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

