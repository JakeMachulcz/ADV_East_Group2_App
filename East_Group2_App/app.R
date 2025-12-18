library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(glue)

#bring in prepped data
source("data_prep.R")

ui <- fluidPage(
  titlePanel("City of South Bend Parks"),
  br(),
  fluidRow(
    #park chooser
    column(6,
           selectInput(inputId = "park_select", label = "Select Park(s):",
                       choices = sort(parks_projB$Park_Name), selected = NULL, multiple = TRUE)
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
      "Crime",
      h3("How Much Crime Occurs Around Each Park?"),
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
      h3("What Businesses Are Around Each Park?"),
      
      fluidRow(
        column(
          6,
          selectInput(
            inputId = "biz_type",
            label   = "Business Type:",
            choices = c("All", sort(unique(licenses_proj$license_type))),
            selected = "All"
          )
        ),
        column(
          6,
          checkboxInput(
            inputId = "active_only",
            label   = "Show Only Active Licenses",
            value   = FALSE
          )
        )
      ),
      
      br(),
      
      fluidRow(
        column(6, h4("Business Locations")),
        column(6, h4("Business Type Breakdown"))
      ),
      
      fluidRow(
        column(6, leafletOutput("business_map", height = 600)),
        column(6, plotOutput("business_bar", height = 600))
      ),
      
      br(),
      br()
    ),
    
    #Jake's work ---------------------------------------------------------------------------------------------
    tabPanel(
      "Streetlights",
      h3("How Well-Lit Is Each Park?"),
      
      fluidRow(
        column(
          12,
          sliderInput(
            inputId = "min_lumens",
            label = "Minimum streetlight brightness (lumens):",
            min = min(sl_clean$Lumens, na.rm = TRUE),
            max = max(sl_clean$Lumens, na.rm = TRUE),
            value = min(sl_clean$Lumens, na.rm = TRUE),
            step = 500
          )
        )
      ),
      
      br(),
      
      fluidRow(column(6, h4("Streetlights in South Bend"))),
      
      fluidRow(
        column(6, leafletOutput("light_map", height = 600)),
        column(6, plotOutput("light_hist", height = 600))
      ),
      
      br(),
      br()
    ),
    
    #Erich's work ---------------------------------------------------------------------------------------------
    tabPanel(
      "Public Facilities",
      h3("Which Public Facilities Are Around Each Park?"),
      br(),
      fluidRow(
        column(
          12,
          selectInput(
            inputId = "fac_type",
            label   = "Facility Type:",
            choices = c("All", sort(unique(facilities$Type))),
            selected = "All"
          )
        ),
      ),
      fluidRow(
        column(6, leafletOutput("facilities_near_map", height = 600)),
        column(6, plotOutput("facilities_graphed", height = 600))
      ),
      br(),
      br()
    ),
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
  
  
  
  ### Danielle - Business Licenses Tab -------------------------------------------------------------

  prepped_dataD <- reactive({
    respond_to_inputsD(
      selected_parks = input$park_select,
      biz_radius     = input$radius,
      biz_type       = input$biz_type,
      active_only    = input$active_only
    )
  })
  
  output$business_map <- renderLeaflet({
    
    dat <- prepped_dataD()
    
    # Convert parks to lat/lon for leaflet
    parks_map <- dat$parks %>%
      st_set_geometry("buffer") %>%
      st_transform(4326)
    
    # Convert businesses to lat/lon
    biz_map <- dat$businesses %>%
      st_transform(4326)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Park buffer polygons
      addPolygons(
        data = parks_map,
        color = "blue",
        weight = 2,
        fillOpacity = 0.1,
        popup = ~paste0(
          "<b>Park:</b> ", Park_Name,
          "<br><b>Radius (m):</b> ", input$radius
        )
      ) %>%
      
      # Business points
      addCircleMarkers(
        data = biz_map,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>", business_name, "</b><br/>",
          "Type: ", license_type, "<br/>",
          "Status: ", status
        )
      )
  })
  
  output$business_bar <- renderPlot({
    
    dat <- prepped_dataD()
    
    biz_df <- dat$businesses %>%
      st_drop_geometry()
    
    # Handle empty selections gracefully
    if (nrow(biz_df) == 0) {
      ggplot() +
        labs(title = "No businesses found for selected filters") +
        theme_minimal()
    } else {
      ggplot(biz_df, aes(x = fct_infreq(license_type))) +
        geom_bar(fill = "blue") +
        coord_flip() +
        labs(
          x = "Business Type",
          y = "Number of Businesses",
          title = "Business Types Near Selected Parks"
        ) +
        theme_minimal()
    }
  })

  ###Jake - tab3 ---------------------------------------------------------------------------------------------
  prepped_dataJ <- reactive({
    respond_to_inputsJ(
      selected_parks = input$park_select,
      light_radius   = input$radius,
      min_lumens     = input$min_lumens
    )
  })
  
  ## Map
  output$light_map <- renderLeaflet({
    
    prepped <- prepped_dataJ()
    
    parks_buf <- prepped$parks %>%
      st_set_geometry("buffer") %>%
      # Match CRS of lights, since it was a bug
      st_transform(st_crs(prepped$streetlights))
    
    # Only streetlights within selected park buffers
    lights_within <- st_join(
      prepped$streetlights,
      parks_buf,
      join = st_within
    ) %>%
      filter(!is.na(Park_Name)) %>%
      st_transform(4326)
    
    # Transform parks to lat/lon for leaflet
    parks_poly <- parks_buf %>% st_transform(4326)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addPolygons(
        data = parks_poly,
        color = "blue",
        weight = 2,
        fillOpacity = 0.1,
        popup = ~paste0("<b>Park:</b> ", Park_Name)
      ) %>%
      addCircleMarkers(
        data = lights_within,
        radius = 4,
        stroke = FALSE,
        fillColor = "yellow",
        fillOpacity = 0.8,
        popup = ~paste0("<b>Lumens:</b> ", Lumens)
      )
  })
  
  ## Histogram
  output$light_hist <- renderPlot({
    
    prepped <- prepped_dataJ()
    
    # Create the park buffer
    parks_buf <- prepped$parks %>%
      st_set_geometry("buffer") %>%
      st_transform(st_crs(prepped$streetlights))
    
    # Filter to streetlights within the buffer
    lights_filtered <- if (!is.null(input$park_select) && length(input$park_select) > 0) {
      st_join(prepped$streetlights, parks_buf, join = st_within) %>%
        filter(!is.na(Park_Name))
    } else {
      prepped$streetlights
    }
    
    # Drop geometry for plotting
    lights_df <- st_drop_geometry(lights_filtered)
    
    # Plot histogram
    ggplot(lights_df, aes(x = Lumens)) +
      geom_histogram(binwidth = 5000, fill = "yellow") +
      labs(
        x = "Streetlight Brightness (Lumens)",
        y = "Count",
        title = "Distribution of All Streetlights"
      ) +
      # Match the hist theme to the map theme
      theme(
        panel.background = element_rect(fill = "black"),
        plot.background  = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "grey"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white")
      )
  })
  
  ###Erich - tab4 (Facilities Near Parks) ---------------------------------------------------------------------------------------------
  
  facilities_near_data <- reactive({
    generate_facilities_near_parks(
      selected_parks = input$park_select,
      radius_m = input$radius,
      fac_type = input$fac_type
    )
  })
  
  output$facilities_near_map <- renderLeaflet({
    req(input$radius)
    
    dat <- facilities_near_data()
    
    #create poly df
    parks_poly <- dat$parks_buf %>%
      st_set_geometry("buffer") %>%
      st_transform(4326)
    
    #use lat and lon for facilities
    fac_map <- st_transform(dat$facilities_near, 4326)
    coords <- st_coordinates(fac_map)
    fac_map$Lon <- coords[, 1]
    fac_map$Lat <- coords[, 2]
    
    facility_map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -86.2520, lat = 41.6764, zoom = 11) %>%
      
      #park buffer polygons
      addPolygons(
        data = parks_poly,
        color = "darkgreen",
        weight = 2,
        fillOpacity = 0.10,
        popup = ~paste0("<b>Park:</b> ", Park_Name,
                        "<br><b>Radius (m):</b> ", input$facility_radius)
      )
    
    #facility points inside zones
    if(nrow(fac_map) > 0) {
      facility_map <- facility_map %>%
        addCircleMarkers(
          data = fac_map,
          radius = 5,
          stroke = FALSE,
          fillOpacity = 0.8,
          color = "black",
          popup = ~paste0("<b>", Name, "</b><br/>",
                          Type, "<br/>",
                          Address, "<br/>",
                          City, ", ", state, " ", zip_Code, "<br/>",
                          "Phone: ", Phone)
        )
    }
    facility_map
  })
  
  output$facilities_graphed <- renderPlot({
    
    dat <- facilities_near_data()
    
    fac_df <- dat$facilities_near %>%
      st_drop_geometry()
    
    #fix for errors where no facilities exist
    if (nrow(fac_df) == 0) {
      ggplot() +
        labs(title = "No facilities found for selected filters") +
        theme_minimal()
    } else {
      ggplot(fac_df, aes(x = fct_infreq(Type))) +
        geom_bar(fill = "darkgreen") +
        coord_flip() +
        labs(
          x = "Facility Type",
          y = "Number of Facilities",
          title = "Facility Types Near Selected Parks"
        ) +
        theme_minimal()
    }
  })
  
}

shinyApp(ui = ui, server = server)


