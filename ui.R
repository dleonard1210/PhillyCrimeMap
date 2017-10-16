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
                        leafletOutput("leafletMap", width = "100%", height = 700),
                        
                        absolutePanel(id = "controls", #class = "panel panel-default", style = "background-color: white",
                             fixed = TRUE, draggable = TRUE, top = 75, left = "auto", right = 20, bottom = "auto",
                             width = 350, height = "auto",
                             div(br(),
                                 selectInput("CrimeType", "Crime Category", c("Homicide - Criminal"), 
                                             multiple = FALSE,
                                             selectize = FALSE),
                                 dateRangeInput('dateRange',
                                                label = 'Date Range',
                                                start = Sys.Date() - 365, end = Sys.Date(),
                                                startview = "year",
                                                min = as.Date("2006-01-01"), max = Sys.Date()), 
                                 style="font-size:140%"
                             ),
                             uiOutput("NumberOfObservations"),
                             div(style="float:right",h5(a("Leonard Analytics", href = "http://leonardanalytics.com")))
                             )
                        )
                    ),
           tabPanel("Historical Trends",
                    div(style="float:right",h5(a("Leonard Analytics", href = "http://leonardanalytics.com"))),
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                     tags$div("                       Wait for it...")),
                    plotOutput("facetWrap")
           )
)

