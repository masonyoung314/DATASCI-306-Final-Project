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
                   min = 1,
                   max = 4,
                   value = 0
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
  x_half <- -83.74
  y_half <- 42.28
  
  a2_center <- tibble::tibble(
    long = x_half,
    lat = y_half
  )
  
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
  
  a2_distance_visualization <- a2housing_no_missing |> 
    mutate(
      distance = distHaversine(matrix(c(long, lat), ncol = 2), 
                               c(a2_center$long, a2_center$lat)) * 
        0.000621371
    ) 
  
  
  
  stadia_key <- Sys.getenv("STADIA_KEY")
  
  ggmap::register_stadiamaps(stadia_key)
  
  output$housingVis <- renderLeaflet({
    
    a2_distance_vis_filtered <- a2_distance_visualization |> 
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
        data = a2_distance_vis_filtered,
        lng = ~long,
        lat = ~lat,
        radius = 1,
        opacity = 0.5,
        color = "red"
      )
  })
  
  output$coefficients <- renderUI({
    
      a2_distance_vis_filtered <- a2_distance_visualization |> 
        filter(distance <= input$distance & distance >= input$distance - 0.5)
    
    print(a2_distance_visualization)
    
    sqft <- lm(sale_price ~ sqft, data = a2_distance_vis_filtered) |> coef()
    acres <- lm(sale_price ~ acres, data = a2_distance_vis_filtered) |> coef()

    HTML(paste("Square feet coefficient:<b>", round(sqft[2], digits = 2), "</b><br> Acres coefficient:<b>",
               round(acres[2], digits = 2), "</b>"))
    
  })
  
  output$TrendofCoefficients <- renderPlot({
    
    a2_one <- a2_distance_visualization |> 
      filter(distance <= 1 & distance >= 0.5)
    
    a2_two <- a2_distance_visualization |> 
      filter(distance <= 2 & distance >= 1.5)
    
    a2_three <- a2_distance_visualization |> 
      filter(distance <= 3 & distance >= 2.5)
    
    a2_four <- a2_distance_visualization |> 
      filter(distance <= 4 & distance >= 3.5)
    
    a2_sqft_one <- lm(sale_price ~ sqft, data = a2_one) |> coef()
    a2_sqft_two <- lm(sale_price ~ sqft, data = a2_two) |> coef()
    a2_sqft_three <- lm(sale_price ~ sqft, data = a2_three) |> coef()
    a2_sqft_four <- lm(sale_price ~ sqft, data = a2_four) |> coef()
    
    a2_acres_one <- lm(sale_price ~ acres, data = a2_one) |> coef()
    a2_acres_two <- lm(sale_price ~ acres, data = a2_two) |> coef()
    a2_acres_three <- lm(sale_price ~ acres, data = a2_three) |> coef()
    a2_acres_four <- lm(sale_price ~ acres, data = a2_four) |> coef()
    
    
    a2_coefs_one <- tibble::tibble(
      distance = c(1, 1),
      coefs = c(a2_sqft_one[2], a2_acres_one[2]),
      name = c("sqft", "acres")
    )
    
    a2_coefs_two <- tibble::tibble(
      distance = c(1, 2, 1, 2),
      coefs = c(a2_sqft_one[2], a2_sqft_two[2], a2_acres_one[2], a2_acres_two[2]),
      name = c("sqft", "sqft", "acres", "acres")
    )
    
    a2_coefs_three <- tibble::tibble(
      distance = c(1, 2, 3, 1, 2, 3),
      coefs = c(a2_sqft_one[2], a2_sqft_two[2], a2_sqft_three[2], 
                a2_acres_one[2], a2_acres_two[2], a2_acres_three[2]),
      name = c("sqft", "sqft", "sqft", "acres", "acres", "acres")
    )
    
    a2_coefs_four <- tibble::tibble(
      distance = c(1, 2, 3, 4, 1, 2, 3, 4),
      coefs = c(a2_sqft_one[2], a2_sqft_two[2], a2_sqft_three[2], a2_sqft_four[2], 
                a2_acres_one[2], a2_acres_two[2], a2_acres_three[2], a2_acres_four[2]),
      name = c("sqft", "sqft", "sqft", "sqft", "acres", "acres", "acres", "acres")
    )
    
    if (input$distance == 1) {
      tib_needed <- a2_coefs_one
    }
    else if (input$distance == 2) {
      tib_needed <- a2_coefs_two
    }
    else if (input$distance == 3) {
      tib_needed <- a2_coefs_three
    }
    else {
      tib_needed <- a2_coefs_four
    }
    
    map_base <- tib_needed |> ggplot() + theme_minimal() + labs(
      title = "Effects of Home Square Footage and Acreage on Sale Price vs. Distance from Center of Ann Arbor",
      x = "Distance",
      y = "Price Change Per One Unit Increase"
    )
    
    if (input$distance == 1) {
      map_base + geom_point(aes(x = distance, y = coefs, color = name, shape = name))
    }
    else {
      map_base + geom_line(aes(x = distance, y = coefs, color = name)) +
        geom_point(aes(x = distance, y = coefs, color = name, shape = name))
    }
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
