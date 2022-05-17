library(plotly)
library(tidyverse)
library(shiny)
library(ggplot2)
library(zoo)
library(lubridate)

library(leaflet)
library(sp)

# load call log data
acems <- read.csv("../app/data/acems.csv")

# load campus location data and extract coordinates
locations <- read.csv("../app/data/locations.csv", header = FALSE) %>%
  rename("name" = V1) %>%
  pivot_longer(cols = -1, names_to = "coord_num",
               names_pattern = "V([0-9]*)",
               values_to = "coord") %>%
  # adjust since V[coord_num] starts at second column
  # and make numeric
  mutate(coord_num = as.numeric(coord_num) - 1) %>%
  filter(coord != "") %>%
  separate(coord, into = c("lat", "long"), sep = ", ") %>%
  mutate(across(c(lat, long), as.numeric)) %>%
  select(-coord_num) %>%
  nest(coords = c(long, lat))

# max number of calls in any one location
max_per_location <- max(acems %>%
                          group_by(location) %>%
                          summarize(N = n()) %>%
                          select(N))

#### server
server <- function(input, output, session) {
  
  ## translate input "All" or "Both" into all values of the
  ## respective filter variable
  
  selected_academic_year <- reactive({
    if (input$select_year == "All")
      unique(acems$academic_year)
    else input$select_year
  })
  
  selected_semester <- reactive({
    if (input$select_semester == "Both")
      unique(acems$semester)
    else input$select_semester
  })
  
  selected_category <- reactive({
    if (input$select_category == "All")
      unique(acems$category)
    else input$select_category
  })
  
  selected_chief_complaint <- reactive({
    if (input$select_cc == "All")
      unique(acems$chief_complaint)
    else input$select_cc
  })
  
  ## update selectors once certain filters have been chosen
  
  observe({
    semesters <- acems %>%
      group_by(academic_year, semester) %>%
      summarize() %>%
      filter(academic_year %in% selected_academic_year())
    updateRadioButtons(session, "select_semester", label = "Semester:",
                       choices = c("Both", unique(semesters$semester)))
  })
  
  observe({
    cc <- acems %>%
      group_by(chief_complaint, category) %>%
      summarize() %>%
      filter(category %in% selected_category())
    updateSelectInput(session, "select_cc", label = "Chief complaint:",
                      choices = c("All", sort(cc$chief_complaint)))
  })
  
  # base leaflet map (no polygons)
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 15, maxZoom = 20)) %>%
      # set bounds and center to campus
      setMaxBounds(-72.58, 42.42, -72.47, 42.34) %>%
      setView(-72.521846, 42.371191, zoom = 15) %>%
      # different terrain views
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       group = "Street Map View",
                       options = providerTileOptions(opacity = 0.7)) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "Terrain View",
                       options = providerTileOptions(opacity = 0.7)) %>%
      # terrain controls
      addLayersControl(
        baseGroups = c("Street Map View", "Terrain View"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # update map when chief complaint or variable type are changed
  observe({
    
    # input options
    absolute <- input$select_var == "absolute"
    whole_range <- input$select_scale == "Show whole range"
    
    # wrangle call data for use in map
    location_data <- acems %>%
      group_by(location) %>%
      summarize(total_calls = sum(academic_year %in% selected_academic_year() &
                                  semester %in% selected_semester()),
                num_calls = sum(academic_year %in% selected_academic_year() &
                                semester %in% selected_semester() &
                                category %in% selected_category() &
                                chief_complaint %in% selected_chief_complaint())) %>%
      mutate(percent_calls = num_calls / total_calls * 100.0)
    
    
    # create Spatial Polygon DF from coordinates and add data
    locations_spolydf <- SpatialPolygonsDataFrame(
      # create spatial polygons from coordinates; ID is gathered from row #
      SpatialPolygons(map2(locations$coords,
                           c(1:nrow(locations)),
                           ~Polygons(list(Polygon(.x)), ID = .y))),
      data = location_data)
    
    # labels for polygons
    # Location
    # (#) calls
    # (%)%
    poly_labels <- sprintf(
      "<strong>%s</strong><br/>%i calls<br/>%.1f%%",
      locations_spolydf$location,
      locations_spolydf$num_calls,
      locations_spolydf$percent_calls
    ) %>% lapply(htmltools::HTML)
    
    # color palette for displaying call data
    if (whole_range) {
      # domain should cover entire range of possible data (so that colors can be
      # to each other)
      domain <- if (absolute) c(0,max_per_location)
                else c(0,100)
    }
    else {
      # domain should cover only the relevant range of data (so it is easier to
      # discern differences)
      domain <- if (absolute) location_data$num_calls
                else location_data$percent_calls
    }
    pal <- colorNumeric("inferno", domain = domain)
    
    # add polygons and legend based on the user's selection
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = locations_spolydf,
                  fillColor = if (absolute) ~pal(num_calls)
                              else ~pal(percent_calls),
                  weight = 1.5,
                  opacity = 0.8,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 2.5,
                    color = "black",
                    dashArray = "1",
                    fillOpacity = 0.3,
                    bringToFront = TRUE),
                  label = poly_labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "12px",
                    direction = "right")) %>%
        addLegend(pal = pal, values = domain,
                  opacity = 0.7,
                  title = if (absolute) "Number</br>of calls"
                          else "% of calls</br>at location",
                  position = "bottomright",
                  labFormat = labelFormat(suffix = if (absolute) "" else "%"))
  })
}

#### ui

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 60, width = 250,
    wellPanel(
      selectInput("select_year", label = "Academic year:",
                  choices = c("All", unique(acems$academic_year))),
      radioButtons("select_semester", label = "Semester:",
                  choices = c("Both", unique(acems$semester))),
      hr(style = "border-top: 1px solid #4f4f4f;"),
      selectInput("select_category", label = "Category:",
                  choices = c("All", sort(unique(acems$category)))),
      selectInput("select_cc", label = "Chief complaint(s):",
                  choices = c("All", sort(unique(acems$chief_complaint)))),
      radioButtons("select_var", label = "Show:",
                   choices = c("Call count" = "absolute",
                               "Percent of calls in that area
                               (in selected year & semester)" = "percent")),
      radioButtons("select_scale", label = "Color scale:",
                   choices = c("Limit to relevant range", "Show whole range"))
    ),
    img(src = "acems-logo-transparent.png", width = 250, height = 166)
  )
)

shinyApp(ui = ui, server = server)