#
# This is the user-interface definition for a Shiny web application that provides interactive visual exploration of
# the Philadelphia Crime statistics database
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Philadelphia Crime Statistics"),
  
  h5("More information available at ", a("Leonard Analytics", href = "http://leonardanalytics.com")),
  
  # Sidebar with time period inputs and crime categories 
  sidebarLayout(
    sidebarPanel(
        selectInput("CrimeType", "Select Crime Category", c("Homicide - Criminal",
                                                            "Aggravated Assault Firearm",
                                                            "Aggravated Assualt No Firearm",
                                                            "Burlary Non-Residential",
                                                            "Burglary Residential",
                                                            "Disorderly Conduct",
                                                            "DRIVING UNDER THE INFLUENCE",
                                                            "Motor Vehicle Theft",
                                                            "Offenses Against Family and Children",
                                                            "Public Drunkenness",
                                                            "Rape",
                                                            "Recovered Stolen Motor Vehicle",
                                                            "Robbery Firearm",
                                                            "Robbery No Firearm",
                                                            "Vagrancy/Loitering",
                                                            "Vandalism/Criminal Mischief",
                                                            "Weapon Violations"), selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        dateRangeInput('dateRange',
                       label = 'Specify time period (yyyy-mm-dd)',
                       start = Sys.Date() - 365, end = Sys.Date()
                       )
        ),
    
    # Show a leaflet map
    mainPanel(
        "Map", htmlOutput("MapTitle"), leafletOutput("leafletMap", width = "100%", height = 500))
    # mainPanel(
    #     tabsetPanel(
    #         tabPanel("Map", htmlOutput("MapTitle"), leafletOutput("leafletMap", width = "100%", height = 550))
    #         #tabPanel("Table", dataTableOutput("table"))
    #         #tabPanel("Chart", htmlOutput("chart"))
       #  )
       # )
    )
  )
)
