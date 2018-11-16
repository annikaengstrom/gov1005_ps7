library(shiny)
library(tidyverse)
library(stargazer)

app_data <- readRDS("result_context.rds")
labels <- readRDS("labels.rds")
variables <- readRDS("variables.rds")
match_label <- readRDS("match_label.rds")

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
        htmlOutput("regression_table"),
        h2("Choose states to display"),
        checkboxGroupInput("state", "States to show:",
                           c("Arizona" = "AZ",
                             "California" = "CA",
                             "Colorado" = "CO",
                             "Florida" = "FL",
                             "Georgia" = "GA",
                             "Iowa" = "IA",
                             "Illinois" = "IL",
                             "Kansas" = "KS",
                             "Kentucky" = "KY",
                             "Maine" = "ME",
                             "Michigan" = "MI",
                             "Minnesota" = "MN", 
                             "North Carolina" = "NC",
                             "Nebraska" = "NE",
                             "New Jersey" = "NJ",
                             "New Mexico" = "NM",
                             "New York" = "NY",
                             "Ohio" = "OH",
                             "Pennsylvania" = "PA",
                             "Texas" = "TX",
                             "Utah" = "UT",
                             "Virginia" = "VA",
                             "Washington" = "WA",
                             "Wisconsin" = "WI",
                             "West Virginia" = "WV"), selected = app_data$state)
      ),
      
      # Show a plot
      mainPanel(
        h3("Summary of Findings"),
        h5("In a simple linear regression relating the error margin (the difference between polled Republican advantage 
           and actual Republican advantage)in Upshot polls to 16 other political and demographic variables, only 
           Republican advantage is significantly correlated with the error margin at the 95% confidence level when all
           states are included. However, this may be influenced by a handful of districts with very high predicted 
           Republican advantage but low actual Republican advantage. What states are those districts in? 
           See what happens when you remove them from the data!"),
        plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     filteredData <- reactive ({
       df <- app_data[app_data$state %in% input$state,]
     })
     
     for(i in 1:length(variables)) {
       if(input$x == variables[i]) {
         x_axis <- labels[i]
         break
       }
       else {
         i <- i + 1
       }
     }

     if(input$line == TRUE) {
       ggplot(filteredData(), aes_string(x = input$x, y = "error", col = "state")) + geom_jitter() + 
              geom_smooth(inherit.aes = FALSE, aes_string(x = input$x, y = "error"), method = "lm") + 
              labs(x = x_axis, y = "Percent Error", color = "State")
     } 
     else {
       ggplot(filteredData(), aes_string(x = input$x, y = "error", color = "state")) + geom_jitter() + 
              labs(x = x_axis, y = "Percent Error", color = "State")
     }
   })
   
   output$see_table <- renderUI ({
     if(input$line == TRUE) {
       h5("Please consult the regression table below to see if the relationship is statistically significant:")
     }
   })
   
   output$regression_table <- renderUI({
     filteredData <- reactive ({
       df <- app_data[app_data$state %in% input$state,]
     })
     
     if(input$line == TRUE) {
       model <- reactive({ form <- as.formula( paste( "error ~", paste(names(filteredData())[names(filteredData()) %in% input$x], collapse="+")))
       lm(form, data=filteredData())
       })
       HTML(stargazer(model(), type = "html"))
     }
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

