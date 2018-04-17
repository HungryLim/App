#LIM
###########################################
# Define UI for dataset viewer app ----
library(shiny)
library(EBMAforecast)
data("presidentialForecast")
attach(presidentialForecast)
presidentialForecast$years<- seq(1952, 2008, 4)
ui <- fluidPage(
  headerPanel("Presidential Forecasts"),
  # App title ----
  titlePanel("Results of presidential forecasts from 1952-2008"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput("variable", "Projection type:",
                  c("None" = NULL,
                    "Campbell"="Campbell",
                    "Hibbs"="Hibbs",
                    "Fair"="Fair", 
                    "EWT2C2"="EWT2C2",
                    "Abramowitz"="Abramowitz")),
      
      sliderInput("range", label="Observed Years",
                  min=min(1952), 
                  max=max(2008), 
                  value=c(1952, 2008), step=4)
      
      
      # Input: Numeric entry for number of obs to view ----
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for caption ----
      h3(textOutput("caption")),
      
      # Output: ----
      plotOutput("forecastPlot", click="plot_click"),
      verbatimTextOutput("info")
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$presidentialForecast,
           "Campbell" = Campbell,
           "Hibbs" = Hibbs,
           "Fair" = Fair)
  })
  
  formulaText <- reactive({
    paste("Yearly forecast according to", input$presidentialForecast)
  })
  
  # Generate a summary of the dataset ----
  output$forecastPlot <- renderPlot({
    plot(presidentialForecast$years, ylim=c(40,66), presidentialForecast[,input$variable], xlab="Year", ylab="Presidential Vote Share", "l", col="red", lty=2)
    lines(presidentialForecast$years, presidentialForecast$Actual)
    legend("topright", legend=c("Predicted Result", "Actual Result"), col=c("red", "black"), lty=c(2,1), cex=0.8)
    
    output$info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)})
  
  })}


# Create Shiny app ----
shinyApp(ui = ui, server = server)

as.matrix(presidentialForecast)