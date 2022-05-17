library(plotly)
library(tidyverse)
library(shiny)
library(ggplot2)
library(zoo)
library(lubridate)

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


ui <- navbarPage(title = "ACEMS", theme = shinytheme("flatly"),
  tabPanel("Info",
           # info tab is populated with HTML file
           includeHTML("data/info_tab.html")
  ),
  tabPanel("Map",
           leafletOutput("map", height= "90vh"),
           absolutePanel(top = 95, left = 70, width = 250,
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
                         )
           )
  ),
  tabPanel("Bar Chart",
           #add theme
           # theme = shinytheme("flatly"),
           #add options for filtering
           radioButtons(
             "radio",
             label = h3("Select filtering"),
             choices = list(
               "Nature of Call" = 1,
               "Category" = 2,
               "Shift Type" = 3,
               "Weekend/Weekday" = 4,
               "Result" = 5
             ),
             selected = 1
           ),
           #add options for count/percent for barcharts
           radioButtons(
             "count_percent",
             label = "Show:",
             choices = c("Call Count" = "count",
                         "Percent of Calls" = "percent")
           ),
           # dropdown for academic year
           sidebarLayout(sidebarPanel(
             width = 2,
             # Select academic year
             selectInput(
               inputId = "academic_year",
               label =  "Choose academic year:",
               selected = "All",
               choices = c("All", acems_combined$academic_year)
             )
           ),
           mainPanel()),
           
           # dropdown for month of call
           sidebarLayout(
             sidebarPanel(
               width = 2,
               # Select month of call
               selectInput(
                 inputId = "month_of_call",
                 label =  "Choose month:",
                 selected = "",
                 choices = c("All", "Months" = month_choices)
               )
             ),
             # plot first bar chart based on month(s)
             mainPanel(width = 10, plotlyOutput("BarChart1"))
           ),
           
           # dropdown for semester of call
           sidebarLayout(
             sidebarPanel(
               width = 2,
               # Select semester of call
               selectInput(
                 inputId = "semester",
                 label =  "Choose semester:",
                 selected = "",
                 choices = c("All", acems_combined$semester)
               )
             ),
             # plot second bar chart based on semester(s)
             mainPanel(width = 10, plotlyOutput("BarChart2"))
           )
  )
)


server <- function(session, input, output) {
  
  # Tab 1 map
  
  
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
    # legend needs reversed palette so numbers go from low / bottom to
    # high / top
    pal_legend <- colorNumeric("inferno", domain = domain, reverse = TRUE)
    
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
      addLegend(pal = pal_legend, values = domain,
                opacity = 0.7,
                title = if (absolute) "Number</br>of calls"
                else "% of calls</br>at location",
                position = "bottomright",
                labFormat = labelFormat(suffix = if (absolute) "" else "%",
                                        # high -> low
                                        transform = function(x) sort(x, decreasing = TRUE)))
  })
  
  # Tab 2 bar chart
  
  # function to dynamically filter based on academic year(s)
  filter_acyear <- reactive({
    acems2 <- acems_combined
    # filter data based on specified academic year(s)
    if (input$academic_year != "All") {
      acems2 <- acems2 %>%
        filter(academic_year == input$academic_year)
    }
    acems2
  })
  
  # filter based on users' option of filtering for month(s)
  acems_filtered_month <- reactive({
    acems2 <- filter_acyear()
    # filter data based on specified month(s)
    if (input$month_of_call != "All") {
      acems2 <- acems2 %>%
        filter(month_of_call == input$month_of_call)
    }
    # filter based on nature of call
    if (input$radio == 1) {
      acems2 <- acems2 %>%
        group_by(month_of_call, nature) %>%
        summarize(calls_per_month = n()) %>%
        rename(variable = nature)
    }
    # filter based on category
    else if (input$radio == 2) {
      acems2 <- acems2 %>%
        group_by(month_of_call, category) %>%
        summarize(calls_per_month = n()) %>%
        rename(variable = category)
    }
    # filter based on shift type
    else if (input$radio == 3) {
      acems2 <- acems2 %>%
        group_by(month_of_call, shift_type) %>%
        summarize(calls_per_month = n()) %>%
        rename(variable = shift_type)
    }
    # filter based on weekend/weekday
    else if (input$radio == 4) {
      acems2 <- acems2 %>%
        group_by(month_of_call, is_weekend) %>%
        summarize(calls_per_month = n()) %>%
        rename(variable = is_weekend)
    }
    # filter based on result
    else if (input$radio == 5) {
      acems2 <- acems2 %>%
        group_by(month_of_call, result) %>%
        summarize(calls_per_month = n()) %>%
        rename(variable = result)
    }
    acems2
  })
  
  # filter based on users' option of filtering for semester(s)
  acems_filtered_semester <- reactive({
    acems2 <- filter_acyear()
    # filter data based on specified semester(s)
    if (input$semester != "All") {
      acems2 <- acems2 %>%
        filter(semester == input$semester)
    }
    # filter based on nature of call
    if (input$radio == 1) {
      acems2 <- acems2 %>%
        group_by(semester, nature) %>%
        summarize(calls_per_semester = n()) %>%
        rename(variable = nature)
    }
    # filter based on category
    else if (input$radio == 2) {
      acems2 <- acems2 %>%
        group_by(semester, category) %>%
        summarize(calls_per_semester = n()) %>%
        rename(variable = category)
    }
    # filter based on shift type
    else if (input$radio == 3) {
      acems2 <- acems2 %>%
        group_by(semester, shift_type) %>%
        summarize(calls_per_semester = n()) %>%
        rename(variable = shift_type)
    }
    # filter based on weekend/weekday
    else if (input$radio == 4) {
      acems2 <- acems2 %>%
        group_by(semester, is_weekend) %>%
        summarize(calls_per_semester = n()) %>%
        rename(variable = is_weekend)
    }
    # filter based on result
    else if (input$radio == 5) {
      acems2 <- acems2 %>%
        group_by(semester, result) %>%
        summarize(calls_per_semester = n()) %>%
        rename(variable = result)
    }
    acems2
  })
  
  # update months that appear
  # in drop down based on academic year
  observeEvent(input$academic_year, {
    updateSelectInput(
      session,
      inputId = "month_of_call",
      label =  "Choose month:",
      selected = "All",
      choices = c("All", "Months" = list(
        arrange(filter_acyear(), month_of_call)$month_of_call
      ))
    )
  })
  
  
  # first bar chart
  # breakdown of acems calls based on
  # academic year, month, and users' filtering
  output$BarChart1 <- renderPlotly({
    ggplot(
      acems_filtered_month(),
      aes(x = month_of_call,
          y = calls_per_month,
          fill = variable)
    ) +
      {
        if (input$count_percent == "percent")
          scale_y_continuous(labels = scales::percent)
      } +
      {
        # show count or percent based on users' input
        if (input$count_percent == "percent")
          geom_col(position = "fill")
        else
          geom_col()
      } +
      coord_flip() +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Month of Call",
           y = "Number of Calls",
           title = "ACEMS Calls per Month") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(fill = guide_legend(title = names[as.numeric(input$radio)]))
  })
  
  # second bar chart
  # breakdown of acems calls based on
  # academic year, semester, and users' filtering
  output$BarChart2 <- renderPlotly({
    ggplot(
      acems_filtered_semester(),
      aes(x = semester,
          y = calls_per_semester,
          fill = variable)
    ) +
      {
        if (input$count_percent == "percent")
          scale_y_continuous(labels = scales::percent)
      } +
      {
        # show count or percent based on users' input
        if (input$count_percent == "percent")
          geom_col(position = "fill")
        else
          geom_col()
      }  +
      coord_flip() +
      scale_fill_brewer(palette = "Paired") +
      labs(x = "Semester",
           y = "Number of Calls",
           title = "ACEMS Calls per Semester") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(fill = guide_legend(title = names[as.numeric(input$radio)]))
  })
}



shinyApp(ui, server)