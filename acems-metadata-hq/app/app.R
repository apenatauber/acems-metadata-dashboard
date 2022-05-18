library(plotly)
library(tidyverse)
library(shiny)
library(ggplot2)
library(zoo)
library(lubridate)
library(viridis)
library(stringr)
library(leaflet)
library(sp)
library(tidyr)
library(dplyr)
library(shinythemes)


#wrangling data for map tab

# load call log data
acems <- read.csv("data/acems.csv")

# load campus location data and extract coordinates
locations <- read.csv("data/locations.csv", header = FALSE) %>%
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



#wrangling data for timeline tab

# load data for chief complaint grouping
acems_groups <- read.csv("data/chief_complaints_grouping.csv")

# join data sets
acems_combined <- acems %>%
  inner_join(acems_groups, by = "chief_complaint")

# rename and remove two columns
acems_combined <- acems_combined %>%
  select(-nature.x, -category.x) %>%
  rename(nature = nature.y,
         category = category.y)

# create a list of months
acems_combined$month_of_call <-
  factor(x = acems_combined$month_of_call,
         levels = month.name)

# sort the months chronologically
month_choices <-
  list(arrange(acems_combined, month_of_call)$month_of_call)

# list for updating legend title dynamically
names <- c("Nature of Call",
           "Category",
           "Shift Type",
           "Weekend/Weekday",
           "Result")


ui <- navbarPage(title = "ACEMS", theme = shinytheme("cosmo"),
  tabPanel("Info",
           # info tab is populated with HTML file
           includeHTML("data/info_tab.html")
  ),
  tabPanel("Map",
           sidebarLayout(
             sidebarPanel(width = 3,
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
                                                   "Percent of calls in selected category & chief complaint" = "percent")),
                          radioButtons("select_scale", label = "Color scale:",
                                       choices = c("Limit to relevant range", "Show whole range"))
            ),
            mainPanel(width = 9,
                      leafletOutput("map", height = "700px"))
          ),
          sidebarLayout(
            sidebarPanel(width = 3,
                         selectInput("select_sort", label = "Sort bar chart:",
                                     choices = c("Regular order", "Descending"))),
            mainPanel(width = 9,
                      wellPanel(plotOutput("map_barchart")))
          )
  )
)


server <- function(session, input, output) {
  
  
  ### Tab 1 map
  
  
  ## map options
  
  absolute <- reactive({input$select_var == "absolute"})
  whole_range <- reactive({input$select_scale == "Show whole range"})

  # translate input "All" or "Both" into all values of the
  # respective filter variable
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
  
  # update semester selector
  observe({
    semesters <- acems %>%
      group_by(academic_year, semester) %>%
      summarize() %>%
      filter(academic_year %in% selected_academic_year())
    updateRadioButtons(session, "select_semester", label = "Semester:",
                       choices = c("Both", unique(semesters$semester)))
  })
  
  # update year selector
  observe({
    cc <- acems %>%
      group_by(chief_complaint, category) %>%
      summarize() %>%
      filter(category %in% selected_category())
    updateSelectInput(session, "select_cc", label = "Chief complaint:",
                      choices = c("All", sort(cc$chief_complaint)))
  })
  
  
  ## data wrangling and prep
  
  # wrangle call data for use in map
  location_data <- reactive({
    acems %>%
      group_by(location) %>%
      summarize(total_calls = sum(academic_year %in% selected_academic_year() &
                                    semester %in% selected_semester()),
                num_calls = sum(academic_year %in% selected_academic_year() &
                                  semester %in% selected_semester() &
                                  category %in% selected_category() &
                                  chief_complaint %in% selected_chief_complaint())) %>%
      mutate(percent_calls = num_calls / total_calls * 100.0)
  })
  
  # establish domain of data
  domain <- reactive({
    if (whole_range()) {
      # domain should cover entire range of possible data (so that colors can be
      # to each other)
      if (absolute()) c(0,max_per_location)
      else c(0,100)
    }
    else {
      # domain should cover only the relevant range of data (so it is easier to
      # discern differences)
      if (absolute()) range(location_data()$num_calls)
      else range(location_data()$percent_calls)
    }
  })
  
  # palette for map and bar chart
  pal <- reactive({colorNumeric("inferno", domain = domain())})
  
  # legend needs reversed palette so numbers go from low / bottom to
  # high / top
  pal_legend <- reactive({colorNumeric("inferno", domain = domain(), reverse = TRUE)})
  
  
  ## UI outputs
  
  # base leaflet map (no polygons)
  output$map <- renderLeaflet({
    leaf <- leaflet(options = leafletOptions(minZoom = 15, maxZoom = 20)) %>%
      # set bounds and center to campus
      setMaxBounds(-72.5169773138638, 42.3723787395415, -72.5169773138638, 42.3723787395415) %>%
      setView(-72.5169773138638, 42.3723787395415, zoom = 15) %>%
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
    # add polys but do not take dependencies so map doesn't redraw every time
    # an input is changed
    isolate(addLocationPolys(leaf))
  })
  
  # add polygons based on selectors
  addLocationPolys <- function(map) {
    # create Spatial Polygon DF from coordinates and add data
    locations_spolydf <- SpatialPolygonsDataFrame(
      # create spatial polygons from coordinates; ID is gathered from row #
      SpatialPolygons(map2(locations$coords,
                           c(1:nrow(locations)),
                           ~Polygons(list(Polygon(.x)), ID = .y))),
      data = location_data())
    
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
    
    # add polygons and legend based on the user's selection
    map %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = locations_spolydf,
                  fillColor = if (absolute()) ~pal()(num_calls)
                  else ~pal()(percent_calls),
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
      addLegend(pal = pal_legend(), values = domain(),
                opacity = 0.7,
                title = if (absolute()) "Number</br>of calls" else "% of calls",
                position = "bottomright",
                labFormat = labelFormat(suffix = if (absolute()) "" else "%",
                                        # high -> low
                                        transform = function(x) sort(x, decreasing = TRUE)))
  }
  
  # update polygons without redrawing map whenever selectors are changed
  # (this does take dependencies on selectors)
  observe({
    addLocationPolys(leafletProxy("map"))
  })
  
  # bar chart showing location data
  output$map_barchart <- renderPlot({
    location_data() %>%
      ggplot(aes(x = location,
                 y = if (absolute()) num_calls else percent_calls,
                 fill = if (absolute()) num_calls else percent_calls)) +
      geom_col() +
      scale_fill_viridis(option = "inferno") +
      labs(
        x = NULL,
        y = if (absolute()) "Number of calls" else "% of calls"
      ) +
      scale_x_discrete(labels = function(x) str_replace(x, ' [(].*[)]', '')) +
      theme_linedraw() +
      theme(legend.position = "none",
            text = element_text(size = 20)) +
      coord_flip()
  })
  
}



shinyApp(ui, server)