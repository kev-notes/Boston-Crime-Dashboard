library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(readr)
library(ggplot2)
library(shinyWidgets)

# ---- Load data once ----
crime_data <- read_csv("data/crime.csv", 
                       locale = locale(encoding = "Latin1"),
                       show_col_types = FALSE) %>%
  filter(!is.na(Lat) & !is.na(Long))

pal <- colorFactor(topo.colors(length(unique(crime_data$OFFENSE_CODE_GROUP))), 
                   domain = unique(crime_data$OFFENSE_CODE_GROUP))

dow_levels <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Boston Crime Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", icon = icon("sliders-h")),
      selectInput("year", "Select Year:", 
                  choices = sort(unique(crime_data$YEAR)), 
                  selected = 2018),
      selectInput("month", "Select Month:", 
                  choices = sort(unique(crime_data$MONTH)), 
                  selected = 9),
      pickerInput("offense", "Offense Types:",
                  choices = sort(unique(crime_data$OFFENSE_CODE_GROUP)),
                  selected = sort(unique(crime_data$OFFENSE_CODE_GROUP)),
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    size = 10
                  ),
                  choicesOpt = list(style = rep("color:black;", 
                                                length(unique(crime_data$OFFENSE_CODE_GROUP))))
      ),
      actionButton("select_all", "Select All"),
      actionButton("deselect_all", "Deselect All")
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("kpi_total", width = 3),
      valueBoxOutput("kpi_shooting", width = 3),
      valueBoxOutput("kpi_peak_day", width = 3),
      valueBoxOutput("kpi_peak_hour", width = 3)
    ),
    fluidRow(
      box(width = 8, leafletOutput("crime_map", height = "70vh")),
      box(width = 4, plotOutput("crime_bar", height = "70vh"))
    ),
    fluidRow(
      box(width = 12, plotOutput("crime_heatmap", height = "60vh"))
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Select/Deselect All buttons
  observeEvent(input$select_all, {
    updatePickerInput(session, "offense", selected = sort(unique(crime_data$OFFENSE_CODE_GROUP)))
  })
  observeEvent(input$deselect_all, {
    updatePickerInput(session, "offense", selected = character(0))
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(input$year, input$month)
    if (is.null(input$offense) || length(input$offense) == 0) {
      return(crime_data[0, ])
    }
    crime_data %>%
      filter(YEAR == input$year,
             MONTH == input$month,
             OFFENSE_CODE_GROUP %in% input$offense)
  })
  
  # ---- KPIs ----
  output$kpi_total <- renderValueBox({
    valueBox(
      format(nrow(filtered_data()), big.mark = ","),
      subtitle = "Total Crimes",
      icon = icon("exclamation-triangle"),
      color = "light-blue"
    )
  })
  
  output$kpi_shooting <- renderValueBox({
    data <- filtered_data()
    count_shooting <- if (nrow(data) == 0) 0 else sum(data$SHOOTING == "Y")
    valueBox(
      count_shooting,
      subtitle = "Crimes Involving Shooting",
      icon = icon("crosshairs"),
      color = "red"
    )
  })
  
  output$kpi_peak_day <- renderValueBox({
    data <- filtered_data()
    peak_day <- if (nrow(data) == 0) "—" else data %>% count(DAY_OF_WEEK) %>% arrange(desc(n)) %>% slice(1) %>% pull(DAY_OF_WEEK)
    valueBox(
      peak_day,
      subtitle = "Peak Day",
      icon = icon("calendar-day"),
      color = "yellow"
    )
  })
  
  output$kpi_peak_hour <- renderValueBox({
    data <- filtered_data()
    peak_hour <- if (nrow(data) == 0) "—" else data %>% count(HOUR) %>% arrange(desc(n)) %>% slice(1) %>% pull(HOUR)
    valueBox(
      peak_hour,
      subtitle = "Peak Hour",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  # ---- Map ----
  output$crime_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -71.0589, lat = 42.3601, zoom = 12)
  })
  
  observe({
    data <- filtered_data()
    
    leafletProxy("crime_map", data = data) %>%
      clearMarkers() %>%
      clearHeatmap()
    
    if (nrow(data) > 0) {
      leafletProxy("crime_map", data = data) %>%
        addHeatmap(
          lng = ~Long,
          lat = ~Lat,
          intensity = ~1,
          blur = 20,
          max = 0.05,
          radius = 15,
          gradient = c("0" = "white", "1" = "black")
        ) %>%
        addCircleMarkers(
          lng = ~Long,
          lat = ~Lat,
          radius = 4,
          color = ~pal(OFFENSE_CODE_GROUP),
          stroke = FALSE,
          fillOpacity = 0.6,
          popup = ~paste(INCIDENT_NUMBER, "<br>", OFFENSE_DESCRIPTION, "<br>", STREET, "<br>", DISTRICT, "<br>", OCCURRED_ON_DATE),
          group = ~OFFENSE_CODE_GROUP
        )
    }
  })
  
  # ---- Bar Chart ----
  output$crime_bar <- renderPlot({
    data <- filtered_data()
    if(nrow(data) == 0) return(NULL)
    
    data %>%
      count(OFFENSE_CODE_GROUP) %>%
      mutate(OFFENSE_CODE_GROUP = reorder(OFFENSE_CODE_GROUP, n)) %>%
      ggplot(aes(x = OFFENSE_CODE_GROUP, y = n, fill = OFFENSE_CODE_GROUP)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = setNames(pal(unique(filtered_data()$OFFENSE_CODE_GROUP)), unique(filtered_data()$OFFENSE_CODE_GROUP))) +
      labs(title = paste("Offense Counts -", input$month, "/", input$year),
           x = "Offense Type", y = "Count") +
      theme_minimal(base_size = 14)
  })
  
  # ---- Heatmap (Day vs Hour) ----
  output$crime_heatmap <- renderPlot({
    data <- filtered_data()
    if(nrow(data) == 0) return(NULL)
    
    data %>%
      count(DAY_OF_WEEK, HOUR) %>%
      mutate(DAY_OF_WEEK = factor(DAY_OF_WEEK, levels = dow_levels)) %>%
      ggplot(aes(x = HOUR, y = DAY_OF_WEEK, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "black") +
      labs(title = "Crimes by Day of Week and Hour",
           x = "Hour of Day", y = "Day of Week", fill = "Count") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)