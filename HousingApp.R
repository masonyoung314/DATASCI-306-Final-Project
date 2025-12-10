library(shiny)
library(ggplot2)
library(sf)
library(prettymapr)
library(ggspatial)
library(tidyverse)
library(ggmap)
library(usethis)
library(leaflet)
library(geosphere)
library(purrr)
theme_set(theme_classic())
load("a2housing.RData")

ui <- fluidPage(
  titlePanel("Ann Arbor Housing Search"),
  tabsetPanel(
    tabPanel("Square Feet vs. Acres",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "distance",
                   label = "Distance from City Center (mi): ",
                   min = 0.1,
                   max = 4,
                   value = 0,
                   step = 0.05
                 )
               ),
               mainPanel(
                 leafletOutput("housingVis"),
                 htmlOutput("coefficients"),
                 plotOutput("TrendofCoefficients"), # Graph of the two coefficients, one of each at each distance
                 plotOutput("trendOfTrend") # A graph of the change of each coefficient at each distance
               )
             )),
    tabPanel("Housing Search",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "budget",
                   label = "Budget: ",
                   min = 0,
                   max = 9500000,
                   value = 300000
                 ),
                 sliderInput(
                   inputId = "beds", 
                   label = "Bedrooms: ",
                   min = 0,
                   max = 24,
                   value = c(0, 3)
                 ),
                 sliderInput(
                   inputId = "full_baths", 
                   label = "Full Baths: ",
                   min = 0,
                   max = 24,
                   value = c(0, 3)
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
  a2housing_no_missing <- a2housing |> filter(!is.na(lat), !is.na(long), !is.na(acres), 
                                              !is.na(sqft), !is.na(sale_price), 
                                              sale_price > 1000) |>
    mutate(region = case_when(
      long <= -83.74 & lat >= 42.28 ~ "1",
      long > -83.74 & lat >= 42.28 ~ "2",
      long <= -83.74 & lat < 42.28 ~ "3",
      long > -83.74 & lat < 42.28 ~ "4",
      TRUE ~ "1"
    ))
  
  # City center used for distance calculations
  a2_center <- tibble(long = -83.74, lat = 42.28)
  
  # Compute distance once
  a2housing_with_distance <- a2housing_no_missing |>
    mutate(
      distance = distHaversine(matrix(c(long, lat), ncol = 2),
                               c(a2_center$long, a2_center$lat)) * 0.000621371
    )
  
  # Reactive dataset for distance slider
  houses_by_distance <- reactive({
    a2housing_with_distance |>
      filter(distance <= input$distance & distance >= input$distance - 0.5)
  })
  
  
  stadia_key <- Sys.getenv("STADIA_KEY")
  
  ggmap::register_stadiamaps(stadia_key)
  
  # Display coefficients of sqft vs acres at chosen distance
  output$coefficients <- renderUI({
    data <- houses_by_distance()
    
    # If too few houses, skip regression
    if (nrow(data) < 10) {
      return(HTML("<b>Not enough houses in this band to run a regression.</b>"))
    }
    
    # Regression using only sqft + acres
    mdl <- lm(sale_price ~ sqft + acres, data = data)
    co <- coef(mdl)
    
    effect_sqft <- round(co["sqft"], 2)
    effect_acres <- round(co["acres"], 2)
    
    # Which effect is larger?
    stronger <- if (effect_acres > effect_sqft) {
      "Acres has a stronger influence on price at this distance."
    } else {
      "Square feet has a stronger influence on price at this distance."
    }
    
    HTML(paste0(
      "<b>Effect of 1 square foot:</b> $", effect_sqft, "<br>",
      "<b>Effect of 1 acre:</b> $", effect_acres, "<br><br>",
      "<b>", stronger, "</b>"
    ))
  })
  
  
  output$housingVis <- renderLeaflet({
    x_half <- -83.74
    y_half <- 42.28
    
    a2_center <- tibble::tibble(
      long = x_half,
      lat = y_half
    )
    
    a2_distance_visualization <- a2housing_no_missing |> 
      mutate(
        distance = distHaversine(matrix(c(long, lat), ncol = 2), 
                                 c(a2_center$long, a2_center$lat)) * 
          0.000621371
      ) |> 
      filter(distance <= input$distance & distance >= input$distance - 0.5)
    
  
    leaflet() |> addProviderTiles(providers$Stadia.Outdoors) |> 
      setView(lng = x_half, lat = y_half, zoom = 12)  |> 
      addCircleMarkers(
        data = a2_center,
        lng = ~long,
        lat = ~lat,
        radius = 2,
        opacity = 1,
        color = "green"
      ) |> 
      addCircleMarkers(
        data = a2_distance_visualization,
        lng = ~long,
        lat = ~lat,
        radius = 1,
        opacity = 0.5,
        color = "red"
      )
  })
  

  
  
  
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
    
    # Also discovered that a lot of houses are listed at the incorrect price
    # Many say $1 or $100, etc. 
    
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
        addCircleMarkers(
          data = a2housing_no_missing, 
          lng = ~long, 
          lat = ~lat, 
          radius = 2, 
          color = "black", 
          opacity = 0.5, 
          fillOpacity = 0) |>
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
