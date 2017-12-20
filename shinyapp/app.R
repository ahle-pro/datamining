library(shiny)

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Cognitive Analysis"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Selector for variable to plot against mpg ----
    selectInput("variable", "Program:", 
                c("All" = "cyl",
                  "PRESCO" = "4",
                  "Brain Injury" = "13")),
    
    # Input: Checkbox for whether outliers should be included ----
    checkboxInput("outliers", "Show outliers", TRUE)
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Formatted text for caption ----
    h3(textOutput("caption")),
    
    # Output: Plot of the requested variable against mpg ----
    plotOutput("mpgPlot")
    
  )
)

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    #boxplot(as.formula(formulaText()), data = mpgData, outline = input$outliers, col = "#75AADB", pch = 19)
    
    boxplot(mtcars)
  })
}


shinyApp(ui, server)