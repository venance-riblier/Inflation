rm(list=ls())
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
theme_set(theme_bw())


library(dashboardthemes)

################################################################################

################################################################################
# Update data from API and Import
################################################################################

last_update <- file.info("./data_clean.csv")$mtime
# Update data every 10 days 
if ( (today() - (as.Date(last_update)) ) > 10) {
  source("./get_api_oecd.R")
}


data <- read_csv("./data_clean.csv") 
last_date <- max(data$Date)

################################################################################
# User Interface
################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Inflation in Europe"),
  
  
  dashboardSidebar(collapsed = TRUE,
                  sidebarMenu(
    menuItem("Compare countries", tabName = "var_country", icon = icon("th")),
    menuItem("Compare indexes", tabName = "country_var", icon = icon("th")))),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "var_country",
    
        fluidRow(
          box(width = 2,
              radioButtons("main_var", label = h4("Choice of index"), 
                           choices = list("Core inflation (excluding food and energy)" = "Core",
                                          "Headline inflation (including food and energy)" = "Non Core",
                                          "Energy prices" = "Energy",
                                          "Food prices" = "Food"),
                           selected = "Core"),
              
              checkboxGroupInput("country_choosed", h4("Countries"), 
                                 choices = list("Euro Area"= "EA19", "United-States" = "USA",
                                                "OECD Countries" = "OECD",
                                                "Germany" = "DEU", "United-Kingdom" = "GBR",
                                                "France" = "FRA", "Netherlands" = "NLD",
                                                "Russia" = "RUS", "Turkey" = "TUR",
                                                "Italy" = "ITA", "Spain" = "ESP",
                                                "Belgium" = "BEL", "Poland" = "POL",
                                                "Austria" = "AUT", "Hungary" = "HUN",
                                                "Finland" = "FIN", "Sweden" = "SWE",
                                                "Iceland" = "ISL", "Norway" = "NOR",
                                                "Switzerland" = "CHE"
                                 ), selected = c("EA19", "USA")),
              
              
              
          ),
          
          box(width = 10,
              
              plotOutput("plot"), 
              
              sidebarPanel(
                (sliderInput("week",
                             "Time range",
                             min = ym("2007-01"),
                             max = as.Date(max(data$Date)),
                             value = c(ym("2015-01"), as.Date(max(data$Date))),
                             step = 15)),
                width = 12),
              "Index: Consumer Price Index", br(),
              "Unit of measure: Year-on-year inflation (% change vs. same month of the previous year)", br(),
              "Data source: ", tags$a(href="https://stats.oecd.org/Index.aspx?DataSetCode=PRICES_CPI#", "OECD"),
              br(), br(), "Note: You can select several countries but only one index. You can navigate on the sidebar to 
              compare several indexes for a given country."
          )
        )
    ),
    
    #### Country > Var ####
    tabItem(tabName = "country_var",
            
            fluidRow(
              box(width = 2,
                  checkboxGroupInput("main_var2", label = h4("Choice of index"), 
                               choices = list("Core inflation (excluding food and energy)" = "Core",
                                              "Headline inflation (including food and energy)" = "Non Core",
                                              "Energy prices" = "Energy",
                                              "Food prices" = "Food"),
                               selected = c("Core", "Non Core")),
                  
                  radioButtons("country_choosed2", h4("Countries"), 
                                     choices = list("Euro Area"= "EA19", "OECD Countries" = "OECD",
                                                    "United-States" = "USA", 
                                                    "Germany" = "DEU", "United-Kingdom" = "GBR",
                                                    "France" = "FRA", "Netherlands" = "NLD",
                                                    "Russia" = "RUS", "Turkey" = "TUR",
                                                    "Italy" = "ITA", "Spain" = "ESP",
                                                    "Belgium" = "BEL", "Poland" = "POL",
                                                    "Austria" = "AUT", "Hungary" = "HUN",
                                                    "Finland" = "FIN", "Sweden" = "SWE",
                                                    "Iceland" = "ISL", "Norway" = "NOR",
                                                    "Switzerland" = "CHE"
                                     ), selected = "EA19"),
                  
                  
                  
              ),
              
              box(width = 10,
                  
                  plotOutput("plot2"), 
                  
                  sidebarPanel(
                    (sliderInput("week2",
                                 "Time range",
                                 min = ym("2007-01"),
                                 max = as.Date(max(data$Date)),
                                 value = c(ym("2015-01"), as.Date(max(data$Date))),
                                 step = 31)),
                    width = 12),
                  "Index: Consumer Price Index", br(),
                  "Unit of measure: Year-on-year inflation (% change vs. same month of the previous year)", br(),
                  "Data source: ", tags$a(href="https://stats.oecd.org/Index.aspx?DataSetCode=PRICES_CPI#", "OECD"),
                  br(), br(), "Note: You can select several indexes but only one country. You can navigate on the sidebar to 
              compare several countries for a given index."
              )
            )
            
            )
    
    )
  )
)

################################################################################
# Server
################################################################################

server <- function(input, output) { 
  
  output$plot <-renderPlot({
    
    # Unpack inputs
    country_choosed <- input$country_choosed
    date_min <- input$week[1]
    date_max <- input$week[2]
    var_choosed <- input$main_var
    
    
    toplot <- data %>% 
      filter(
        Country %in% country_choosed,
        Date >= date_min & Date <= date_max,
        Variable %in% var_choosed
      )

    toplot %>%
      ggplot(aes(x = Date, y = Value)) +
      geom_line(aes(color = Country), size = 0.7) +
      geom_point(aes(color = Country)) +
      labs(x = "", y = "Year-on-year inflation") +
      scale_x_date(breaks = "year", labels = scales::date_format("%Y")) 
    
  })
  
  output$plot2 <-renderPlot({
    
    # Unpack inputs
    var_choosed <- input$main_var2
    country_choosed <- input$country_choosed2
    date_min <- input$week2[1]
    date_max <- input$week2[2]
    
    
    toplot <- data %>% 
      filter(Country %in% country_choosed) %>% 
      filter(Date > date_min & Date < date_max) %>% 
      filter(Variable %in% var_choosed)
    
    toplot %>% 
      ggplot(aes(x = Date, y = Value)) +
      geom_line(aes(color = Variable), size = 0.7) +
      geom_point(aes(color = Variable)) +
      labs(x = "", y = "Year-on-year inflation") +
      scale_x_date(breaks = "year", labels = scales::date_format("%Y")) 
    
    
  })
  
}

shinyApp(ui, server)