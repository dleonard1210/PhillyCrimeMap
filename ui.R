#
# This is the user-interface definition for a Shiny web application that provides interactive visual exploration of
# the Philadelphia Crime statistics database
#

library(shiny)
library(shinyjs)
library(leaflet)

navbarPage("Philadelphia Crime Statistics",
           tabPanel("Crime Cluster Map",
                    div(class="outer",
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                            ),
                        leafletOutput("leafletMap", width = "100%", height = 700),
                        
                        absolutePanel(id = "controls",
                                      useShinyjs(),
                                      fixed = TRUE, draggable = TRUE, 
                                      top = 200, left = 20, right = "auto", bottom = "auto",
                                      width = 350, height = "auto",
                                      h4("Crime Category"),
                                          selectInput("CrimeType", NA, c("Homicide - Criminal"), 
                                             multiple = FALSE,
                                             selectize = FALSE),
                                    h4("Show Incidents From Most Recent:"),
                                 radioButtons("radio", NA, c("Month" = "month",
                                                                         "6 Months" = "sixmonths",
                                                                         "Year" = "year", 
                                                                         "Custom" = "custom"),
                                              inline = TRUE),
                                 dateRangeInput('dateRange',
                                                label = NA,
                                                start = "2006-01-01", end = Sys.Date(),
                                                startview = "year",
                                                min = as.Date("2006-01-01"), max = Sys.Date()),
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
           ),
           tabPanel("About", 
                    div(style="max-width:800px", 
                    htmlOutput("about"),div(style="float:right",
                                            h5(a("Leonard Analytics", href = "http://leonardanalytics.com"))))
)
)

