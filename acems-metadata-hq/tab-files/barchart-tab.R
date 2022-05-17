# necessary packages
library(plotly)
library(tidyr)
library(dplyr)
library(shiny)
library(ggplot2)
library(shinythemes)
library(tidyverse)

# Bar chart tab

# load data for bar chart tab in acems data app
acems <- read.csv("../app/data/acems.csv")
# load data for chief complaint grouping
acems_groups <-
  read.csv("../app/data/chief_complaints_grouping.csv")

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

### UI

ui <- fluidPage(
  #add theme
  theme = shinytheme("flatly"),
  #add title
  title = "Timeline of ACEMS Call History: August 2016 to April 2022",
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
  ,
)

### SERVER
#Bar graphs to show breakdown of calls

server <- function(input, output, session) {
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
      }  +
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
      } +
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

shinyApp(ui = ui,
         server = server,
         options = list(height = 40))
