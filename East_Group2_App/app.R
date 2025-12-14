library(shiny)
library(tidyverse)
library(DT)

#bring in prepped data
source("data_prep.R")

ui <- fluidPage(
  titlePanel("City of South Bend Parks"),
  br(),
  fluidRow(
    #park chooser
    column(6,
           selectInput(inputId = "park_select", label = "Select Park(s):",
                       choices = parks_projB$Park_Name, selected = NULL, multiple = TRUE)
    ),
    #radius chooser
    column(6,
           sliderInput(inputId = "radius", label = "Distance from Park (m):", 
                       min = 100, max = 1000, value = 500, step = 50)
    )
  ),
  
  tabsetPanel(
    #Bridget's work ---------------------------------------------------------------------------------------------
    tabPanel(
      "Crime Data",
      h3("How much crime occurs around each park?"),
      fluidRow(
        #inputs for date range
        column(12, dateRangeInput(inputId = "date_range", label = "Select date range:",
                                 start = min(crime_simpleB$date), end = max(crime_simpleB$date),
                                 min = min(crime_proj$date), max = max(crime_proj$date)))
        ),
      fluidRow(
        #commonly used start/end periods
        column(12, actionButton("all_time", "All Time"),
               actionButton("prev_year", "Previous Year"),
               actionButton("ytd", "Year to Date"),
               actionButton("mtd", "Month to Date")
        )
      ),
      br(),
      fluidRow(column(6, h4("Crime Map"))),
      fluidRow(
        column(6, leafletOutput("park_map", height = 600)),
        column(6, plotlyOutput("crime_timeseries", height = 600))
      ),
      #make space between end of dashboard and end of screen
      br(),
      br()
    ),
    
    
    #Danielle's work ---------------------------------------------------------------------------------------------
    tabPanel(
      "Business Licenses",
      h3("Business Licenses"),
      DT::dataTableOutput("table2")
    ),
    #Jake's work ---------------------------------------------------------------------------------------------
    tabPanel(
      "Parks",
      h3("Parks"),
      DT::dataTableOutput("table3")
    ),
    #Erich's work ---------------------------------------------------------------------------------------------
    tabPanel(
      "Facilities Near Parks",
      h3("Facilities Near Parks"),
      h4("Which public facilities are within a chosen distance of parks?"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "facility_park_select",
            label = "Select Park(s):",
            choices = sort(unique(parks_projB$Park_Name)),
            selected = NULL,
            multiple = TRUE
          ),
          sliderInput(
            inputId = "facility_radius",
            label = "Distance from Park (meters):",
            min = 100, max = 2000, value = 500, step = 50
          )
        ),
        mainPanel(
          leafletOutput("facilities_near_map", height = 600),
          br(),
          DT::dataTableOutput("facilities_near_table")
        )
      )
    )
  )
)
    

server <- function(input, output, session) {
  
  ###Bridget - tab1 ---------------------------------------------------------------------------------------------
  #create commonly used start/end dates
  #all time
  observeEvent(input$all_time, {
    updateDateRangeInput(session, "date_range",
                         start = min(crime_proj$date),
                         end = max(crime_proj$date))
  })
  #previous year
  observeEvent(input$prev_year, {
    updateDateRangeInput(session, "date_range",
                         start = max(crime_proj$date) - 365,
                         end = max(crime_proj$date))
  })
  #ytd
  observeEvent(input$ytd, {
    updateDateRangeInput(session, "date_range",
                         #start = as.Date(paste0(format(max(crime_proj$date), "%Y"), "-01-01")),
                         start = as.Date(format(max(crime_proj$date), "%Y-01-01")),
                         end = max(crime_proj$date))
  })
  #mtd
  observeEvent(input$mtd, {
    updateDateRangeInput(session, "date_range",
                         start = as.Date(format(max(crime_proj$date), "%Y-%m-01")),
                         end = max(crime_proj$date))
  })
  #create datasets with filters
  prepped_dataB <- reactive({
    respond_to_inputsB(selected_parks = input$park_select, crime_radius = input$radius,
                      start_date = input$date_range[1], end_date = input$date_range[2])
  })
  ##map
  #create crime map using filtered datasets
  output$park_map <- renderLeaflet({
    parks_mapB <- generate_crime_map(prepped_dataB())
    
    #create colors depending on crime count
    pal <- colorNumeric("Reds", domain = parks_mapB$crime_count)
    
    #create leaflet with crime data
    leaflet(parks_mapB) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addCircleMarkers(radius = 6, fillColor = ~pal(crime_count), fillOpacity = 1, stroke = FALSE,
                       popup = ~paste0("<b>Park: </b>", Park_Name,
                                       "<br><b>Between: ", input$date_range[1], " and ", input$date_range[2], "</b>",
                                       "<br><b># Crimes within ", input$radius, " meters: </b>", crime_count)
      ) %>%
      addLegend(pal = pal, values = ~crime_count, title = "# Crimes in Zone")
  })
  ##timeseries
  #create time series plot using filtered datasets
  time_series_crimes <- reactive({
    generate_crime_timeseries(prepped_dataB())
  })
  
  #plot the crime data using ggplotly
  output$crime_timeseries <- renderPlotly({
    #get crime data
    ts_crimes <- time_series_crimes()
    
    #plot the timeseries - using ggplotly to get text boxes
    p <- ggplot(ts_crimes, aes(x = month)) +
      geom_col(aes(y = crime_count, text = glue("# Crimes: {crime_count}")), 
               fill = "lightblue") +
      geom_line(aes(y = crime_perc_scaled, text = glue("% Crimes: {round(crime_perc, 2)}"), 
                    group = 1), color = "red") +
      labs(x = "Month", y = "# Crimes Near a Park", 
           title = glue("Volume and Percentage of Crimes within {input$radius}m of Selected Parks")) +
      theme_bw()
    ggplotly(p, tooltip = "text")
    
  })
  
  
  
  ###Danielle - tab2 ---------------------------------------------------------------------------------------------
  output$table2 <- DT::renderDataTable(licenses)
  
  ###Jake - tab3 ---------------------------------------------------------------------------------------------
  output$table3 <- DT::renderDataTable(parks)
  
  ###Erich - tab4 (Facilities Near Parks) ---------------------------------------------------------------------------------------------
  
  facilities_near_data <- reactive({
    generate_facilities_near_parks(
      selected_parks = input$facility_park_select,
      radius_m = input$facility_radius
    )
  })
  
  output$facilities_near_map <- renderLeaflet({
    req(input$facility_radius)
    
    dat <- facilities_near_data()
    
    # 1) parks: turn buffer column into the ACTIVE geometry (polygons)
    parks_poly <- dat$parks_buf %>%
      st_set_geometry("buffer") %>%   # <-- THIS is the key fix
      st_transform(4326)
    
    # 2) facilities: convert to lat/lon and create Lon/Lat from geometry (safe)
    fac_map <- st_transform(dat$facilities_near, 4326)
    coords <- st_coordinates(fac_map)
    fac_map$Lon <- coords[, 1]
    fac_map$Lat <- coords[, 2]
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -86.2520, lat = 41.6764, zoom = 11) %>%
      
      # Park buffer polygons
      addPolygons(
        data = parks_poly,
        color = "blue",
        weight = 2,
        fillOpacity = 0.10,
        popup = ~paste0("<b>Park:</b> ", Park_Name,
                        "<br><b>Radius (m):</b> ", input$facility_radius)
      ) %>%
      
      # Facility points inside zones
      addCircleMarkers(
        data = fac_map,
        lng = ~Lon, lat = ~Lat,
        radius = 6,
        popup = ~paste0(
          "<b>", Name, "</b><br/>",
          Type, "<br/>",
          Address, "<br/>",
          City, ", ", state, " ", zip_Code, "<br/>",
          "Phone: ", Phone
        )
      )
  })
  
  output$facilities_near_table <- DT::renderDataTable({
    dat <- facilities_near_data()
    fac <- dat$facilities_near %>% st_drop_geometry()
    
    fac %>%
      select(Name, Type, Address, City, state, zip_Code, Phone, Lat, Lon) %>%
      DT::datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
}

shinyApp(ui = ui, server = server)
