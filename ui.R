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
                             fixed = TRUE, draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                             width = 400, height = "auto",
                             HTML('<p style="text-align:right"><button data-toggle="collapse" data-target="#demo">-/+</button></p>'),
                             h3("Explore Philly's Crime History"),
                             selectInput("CrimeType", "Select Crime Category", c("Homicide - Criminal"), 
                                         multiple = FALSE,
                                         selectize = FALSE),
                             tags$div(id = 'demo',  class="collapse in",
                             plotOutput("countPlot")
                             )
                             # hr(),
                             # dateRangeInput('dateRange',
                             #                label = 'Specify time period for the map (yyyy-mm-dd)',
                             #                start = Sys.Date() - 365, end = Sys.Date()
                             # )
                             # sliderInput('dateRange', width = 450,
                             #                label = 'Specify time period for the map (yyyy-mm-dd)', min=as.Date("2006-01-01"),
                             #             max = Sys.Date(), value = c(Sys.Date()-365, Sys.Date()))
               ),
               absolutePanel(id = "controls", 
                             fixed = TRUE, draggable = TRUE, top = 100, left = 80, right = "auto", bottom = "auto",
                             width = 350, height = "auto",
                             br(),
                             div(
                                 dateRangeInput('dateRange',
                                                label = 'Map Date Range',
                                                start = Sys.Date() - 365, end = Sys.Date(),
                                                startview = "year",
                                                min = as.Date("2006-01-01"), max = Sys.Date()), 
                                 style="font-size:140%"
                             ),
                             uiOutput("NumberOfObservations")
                             # sliderInput('dateRange', width = 450,
                             #                label = 'Specify time period for the map (yyyy-mm-dd)', min=as.Date("2006-01-01"),
                             #             max = Sys.Date(), value = c(Sys.Date()-365, Sys.Date()))
               )
               
               )
               ),
           tabPanel("Historical Trends",
                    plotOutput("facetWrap")
           )
)

