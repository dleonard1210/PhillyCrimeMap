#
# This is the user-interface definition for a Shiny web application that provides interactive visual exploration of
# the Philadelphia Crime statistics database
#

library(shiny)
library(leaflet)

navbarPage("Philadelphia Crime Statistics",
           tabPanel("Crime Cluster Map",
                    div(class="outer",
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                    ),
               # htmlOutput("MapTitle"), 
               leafletOutput("leafletMap", width = "100%", height = 700),
               
               absolutePanel(id = "controls", #class = "panel panel-default", style = "background-color: white",
                             fixed = TRUE, draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 500, height = "auto",
                             h3("Explore Philly's Crime History"),
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
                             sliderInput('dateRange', width = 450,
                                            label = 'Specify time period for the map (yyyy-mm-dd)', min=as.Date("2006-01-01"),
                                         max = Sys.Date(), value = c(Sys.Date()-365, Sys.Date())),
                             plotOutput("countPlot"))
               )
               )
)

