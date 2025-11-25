library(shiny)

ui <- fluidPage(
  titlePanel("Ann Arbor Housing Search"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "beds", 
        label = "Bedrooms: ",
        min = 0,
        max = 24,
        value = 0
      ),
      sliderInput(
        inputId = "fullBaths", 
        label = "Full Baths: ",
        min = 0,
        max = 24,
        value = 0
      ),
      sliderInput(
        inputId = "halfBaths", 
        label = "Half Baths: ",
        min = 0,
        max = 6,
        value = 0
      ),
      sliderInput(
        inputId = "sqft", 
        label = "Square feet: ",
        min = 383,
        max = 9006,
        value = c(383, 9006)
      ),
      sliderInput(
        inputId = "acres", 
        label = "Acres: ",
        min = 0,
        max = 10,
        value = 0
      )     
    ),
    mainPanel(
      textOutput("Random_test")
    )
  )
  
)

server <- function(input, output) {
  output$Random_test <- renderText({
    paste("This is a test")
  })
}

shinyApp(ui, server)
