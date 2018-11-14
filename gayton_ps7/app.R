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
        selectInput("y",
                    "Y-axis:",
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
        checkboxInput("check", label = "Add fit line")
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
     if(input$check == TRUE) {
       ggplot(app_data, aes_string(x = "error", y = input$y)) + geom_point() + geom_smooth(method = "lm")
     } 
     else {
       ggplot(app_data, aes_string(x = "error", y = input$y)) + geom_point()
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

