library(shiny)
library(ggplot2)
library(sf)
library(prettymapr)
library(ggspatial)
library(tidyverse)
theme_set(theme_classic())
load("a2housing.RData")


a2housing_no_missing <- a2housing |> filter(!is.na(lat), !is.na(long), !is.na(acres), !is.na(sqft)) |>
  mutate(region = case_when(
    long <= -83.74 & lat >= 42.28 ~ "1",
    long > -83.74 & lat >= 42.28 ~ "2",
    long <= -83.74 & lat < 42.28 ~ "3",
    long > -83.74 & lat < 42.28 ~ "4",
    TRUE ~ "1"
  ))
      
a2housing_sf <- st_as_sf(a2housing_no_missing, coords = c("long", "lat"), crs = 4326)

ui <- fluidPage(
  titlePanel("Ann Arbor Housing Search"),
  tabsetPanel(
    tabPanel("Housing Search",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "beds", 
                   label = "Bedrooms: ",
                   min = 0,
                   max = 24,
                   value = c(0, 24)
                 ),
                 sliderInput(
                   inputId = "full_baths", 
                   label = "Full Baths: ",
                   min = 0,
                   max = 24,
                   value = c(0, 24)
                 ),
                 sliderInput(
                   inputId = "half_baths", 
                   label = "Half Baths: ",
                   min = 0,
                   max = 6,
                   value = c(0, 6)
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
                   value = c(0, 10)
                 ),
                 selectInput(
                   inputId = "region",
                   label = "Region: ",
                   c("All", "1", "2", "3", "4")
                 )
               ),
               mainPanel(
                 htmlOutput("house_price"),
                 plotOutput("map")
               )
             )
          ),
    tabPanel("Inflation",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "regionInflation",
                   label = "Region: ",
                   c("All", "1", "2", "3", "4")
                 )
               ),
               mainPanel(
                 textOutput("inflation_test_text"),
                 htmlOutput("inflation_test")
               )
             )
    )
  )
)

server <- function(input, output) {
  output$house_price <- renderUI({
    if (input$region == "All") {
      a2housing_filtered <- a2housing_no_missing
    }
    else {
      a2housing_filtered <- a2housing_no_missing |> 
        filter(region == as.numeric(input$region))
    }
    mdl <- lm(sale_price ~ beds + full_baths + half_baths + sqft + acres, data = a2housing_filtered)
    # Solution to this might be not including acres or regressing houses away from 
    # the city center separately from those closer
    coefs <- coef(mdl)
    
    predicted_price <- function(num) {
      signif(coefs[1] + coefs[2] * input$beds[num] + coefs[3] * input$full_baths[num] + 
      coefs[4] * input$half_baths[num] + coefs[5] * input$sqft[num] + coefs[6] * input$acres[num], 4)
    }
    
    HTML(paste("Lowest predicted price: $", predicted_price(1), 
               "<br>", "Highest predicted price: $", predicted_price(2)))
  })
  
  output$map <- renderPlot({
    x_half <- -83.74
    y_half <- 42.28
    ggplot(data = a2housing_sf) + annotation_map_tile(zoom = 14) + geom_sf() +
      geom_hline(yintercept = y_half, linetype="dashed") + 
      geom_vline(xintercept = x_half, linetype="dashed") +
      annotate("text", x = -83.78, y = 42.33, label = "Region 1", color = "blue") +
      annotate("text", x = -83.7, y = 42.33, label = "Region 2", color = "blue") +
      annotate("text", x = -83.78, y = 42.23, label = "Region 3", color = "blue") +
      annotate("text", x = -83.7, y = 42.23, label = "Region 4", color = "blue") +
      labs(
        x = "Longitude", 
        y = "Latitude", 
        title = "Map of Ann Arbor with Homes Sold From 2021-2025"
      )
  })
  
  output$inflation_test_text <- renderText({
    paste("Test here if one or more of the regions of Ann Arbor have experienced greater
    inflation than the national average.")
  })
  
  output$inflation_test <- renderUI({
    inflation21 <- 0.0329
    inflation22 <- 0.0715
    inflation23 <- 0.0644
    inflation24 <- 0.0437
    inflation25 <- 0.0334
    
    HTML(paste("<br>", "According to https://www.in2013dollars.com/Housing/price-inflation 
          and the U.S. Bureau of Labor Statistics, the inflation rates for housing
          in 2021-2025 are as follows: ", "<br>", "<br>", 
          "2021:", inflation21, "<br>", "2022:", inflation22, "<br>",
          "2023:", inflation23, "<br>", "2024:", inflation24, "<br>", 
          "2025:", inflation25, "<br>"))
          
    # figure out how to select houses that are similar to each other, and
    # test how the prices for similar houses have changed from 2021-2025
    
    
  })
  
}

shinyApp(ui, server)
