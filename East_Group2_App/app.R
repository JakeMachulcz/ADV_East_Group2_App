library(shiny)
library(tidyverse)
library(DT)


source("data_prep.R", local = TRUE)

#calls <- read.csv("311_Phone_Call_Log_Mod.csv")
#licenses <- read.csv("Business_Licenses.csv")
#parks <- read.csv("Parks_Locations_and_Features.csv")
#facilities <- read.csv("Public_Facilities.csv")



ui <- fluidPage(
  titlePanel("City of South Bend"),
  
  tabsetPanel(
    tabPanel(
      "311 Phone Call Log",
      h3("311 Phone Call Log"),
      DT::dataTableOutput("table1")
    ),
    tabPanel(
      "Business Licenses",
      h3("Business Licenses"),
      DT::dataTableOutput("table2")
    ),
    tabPanel(
      "Parks",
      h3("Parks"),
      DT::dataTableOutput("table3")
    ),
    tabPanel(
      "Public Facilities",
      h3("Public Facilities"),
      DT::dataTableOutput("table4")
    )
  )
)

server <- function(input, output) {
  output$table1 <- DT::renderDataTable(calls)
  output$table2 <- DT::renderDataTable(licenses)
  output$table3 <- DT::renderDataTable(parks)
  output$table4 <- DT::renderDataTable(facilities)
}

shinyApp(ui = ui, server = server)
