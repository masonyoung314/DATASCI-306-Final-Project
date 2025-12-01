library(shiny)
library(ggplot2)
library(sf)
library(prettymapr)
library(ggspatial)
library(tidyverse)
library(ggmap)
library(usethis)
library(leaflet)
theme_set(theme_classic())
load("a2housing.RData")



a2housing_no_missing <- a2housing |> filter(!is.na(lat), !is.na(long), !is.na(acres), 
                                            !is.na(sqft), !is.na(sale_price)) |>
  mutate(region = case_when(
    long <= -83.74 & lat >= 42.28 ~ "1",
    long > -83.74 & lat >= 42.28 ~ "2",
    long <= -83.74 & lat < 42.28 ~ "3",
    long > -83.74 & lat < 42.28 ~ "4",
    TRUE ~ "1"
  ))

stadia_key <- Sys.getenv("STADIA_KEY")

bbox <- c(left = -83.8, bottom = 42.22, right = -83.68, top = 43.34)

ggmap::register_stadiamaps(stadia_key)



ui <- fluidPage(
  titlePanel("Ann Arbor Housing Search"),
  tabsetPanel(
    tabPanel("Housing Search",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "budget",
                   label = "Budget: ",
                   min = 0,
                   max = 9500000,
                   value = 100000
                 ),
                 sliderInput(
                   inputId = "beds", 
                   label = "Bedrooms: ",
                   min = 0,
                   max = 24,
                   value = c(0, 1)
                 ),
                 sliderInput(
                   inputId = "full_baths", 
                   label = "Full Baths: ",
                   min = 0,
                   max = 24,
                   value = c(0, 1)
                 ),
                 sliderInput(
                   inputId = "half_baths", 
                   label = "Half Baths: ",
                   min = 0,
                   max = 6,
                   value = c(0, 1)
                 ),
                 sliderInput(
                   inputId = "sqft", 
                   label = "Square feet: ",
                   min = 383,
                   max = 9006,
                   value = c(383, 1000)
                 ),
                 sliderInput(
                   inputId = "acres", 
                   label = "Acres: ",
                   min = 0,
                   max = 10,
                   value = c(0, 1)
                 ),
                 selectInput(
                   inputId = "region",
                   label = "Region: ",
                   c("All", "1", "2", "3", "4")
                 )
               ),
               mainPanel(
                 htmlOutput("house_price"),
                 leafletOutput("map")
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
    for (i in 1:5) {
      if (coefs[i] < 0) {
        coefs[i] <- 0
      }
    }
    
    # y-intercept for region 4 was about -157,000, so I just made it zero for
    # now, along with any other variables that had very large negative numbers
    # because of the affect of distance to city center on acre price, etc. so
    # our estimate of the home price should never be less than 0.
    
    predicted_price <- function(num) {
      signif(coefs[1] + coefs[2] * input$beds[num] + coefs[3] * input$full_baths[num] + 
      coefs[4] * input$half_baths[num] + coefs[5] * input$sqft[num], 4)
    }
    
    HTML(paste("Lowest predicted price: <b>$", predicted_price(1), 
               "</b><br>", "Highest predicted price: <b>$", predicted_price(2),
               "</b>"))
  })
  
  output$map <- renderLeaflet({
  
    
      
    if (input$region == "All") {
      a2housing_no_missing_plot <- a2housing_no_missing
    }
    else {
      a2housing_no_missing_plot <- a2housing_no_missing |> 
        filter(region == as.numeric(input$region))
    }
    
    a2_no_missing_filtered <- a2housing_no_missing_plot |> 
      filter(sale_price <= input$budget,
             beds >= input$beds[1] & beds <= input$beds[2],
             full_baths >= input$full_baths[1] & full_baths <= input$full_baths[2],
             half_baths >= input$half_baths[1] & half_baths <= input$half_baths[2],
             sqft >= input$sqft[1] & sqft <= input$sqft[2], 
             acres >= input$acres[1] & acres <= input$acres[2]) 
    
    x_half <- -83.74
    y_half <- 42.28
    
    long <- c(-83.78, -83.7, -83.78, -83.7)
    lat <- c(42.32, 42.32, 42.23, 42.23)
    label <- c("Region 1", "Region 2", "Region 3", "Region 4")
    
    region_markers <- data.frame(
      label = label,
      long = long,
      lat = lat
    )
    
    awesome_icons <- awesomeIcons(
      icon = "home",
      iconColor = "white",
      markerColor = "red"
    )
    
      leaflet() |> addProviderTiles(providers$Stadia.Outdoors) |>
        setView(lng = x_half, lat = y_half, zoom = 12) |>
        addCircleMarkers(data = a2housing_no_missing, lng = ~long, lat = ~lat, radius = 2, color = "black", opacity = 0.5, fillOpacity = 0) |>
        addAwesomeMarkers(
          data = a2_no_missing_filtered, 
          lng = ~long, 
          lat = ~lat, 
          icon = awesome_icons, 
          popup = ~paste0("<b>$", sale_price, "</b><br> Beds: <b>",
                          beds, "</b><br> Bathrooms: <b>", 
                          full_baths + half_baths, "</b><br>Square feet: <b>",
                          sqft, "</b><br>Acres: <b>", 
                          acres, "</b>")) |>
        addPolylines(
          lng = c(x_half, x_half),
          lat = c(y_half + 0.06, y_half - 0.06),
          color = "red",
          weight = 3,
          opacity = 0.8
        ) |>
        addPolylines(
          lng = c(x_half - 0.08, x_half + 0.08),
          lat = c(y_half, y_half),
          color = "red",
          weight = 3,
          opacity = 0.8
        ) |>
        addMarkers(
          data = region_markers,
          lng = ~long,
          lat = ~lat,
          popup = ~label
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
