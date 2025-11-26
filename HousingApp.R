library(shiny)
library(ggplot2)
library(sf)
library(prettymapr)
library(ggspatial)
theme_set(theme_classic())

a2housing_no_missing <- a2housing |> filter(!is.na(lat), !is.na(long))
a2housing_sf <- st_as_sf(a2housing_no_missing, coords = c("long", "lat"), crs = 4326)

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
        inputId = "full_baths", 
        label = "Full Baths: ",
        min = 0,
        max = 24,
        value = 0
      ),
      sliderInput(
        inputId = "half_baths", 
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
      ),
      selectInput(
        inputId = "region",
        label = "Region: ",
        c("1", "2", "3", "4")
      )
    ),
    mainPanel(
      htmlOutput("house_price"),
      plotOutput("map")
    )
  )
)

server <- function(input, output) {
  output$house_price <- renderUI({
    mdl <- lm(sale_price ~ beds + full_baths + half_baths + sqft + acres, data = a2housing)
    coefs <- coef(mdl)
    
    predicted_price <- function(num) {
      signif(coefs[2] * input$beds + coefs[3] * input$full_baths + 
      coefs[4] * input$half_baths + coefs[5] * input$sqft[num] + coefs[6] * input$acres, 4)
    }
    
    HTML(paste("Lowest predicted price: $", predicted_price(1), 
               "<br>", "Highest predicted price: $", predicted_price(2)))
  })
  
  output$map <- renderPlot({
    ggplot(data = a2housing_sf) + annotation_map_tile(zoom = 14) + geom_sf() 
  })
}

shinyApp(ui, server)
