#
# This is the server logic of a Shiny web application that provides interactive visual exploration of
# the Philadelphia Crime statistics database
#

library(shiny)
library(ggplot2)
library(leaflet)
library(lubridate)
library(dplyr)


# Define server logic 
shinyServer(function(input, output, session) {
    
    # Get a list of the different types of crime
    urlstring <- paste0("http://phl.carto.com/api/v2/sql?format=csv&q=",
                        "SELECT distinct rtrim(text_general_code) as text_general_code ",
                        "FROM incidents_part1_part2 order by text_general_code")
    urlstring <- URLencode(urlstring)
    crimelist <- read.csv(url(urlstring),
                          stringsAsFactors = FALSE)$text_general_code

    updateSelectInput(session,
                      "CrimeType",
                      choices = crimelist,
                      selected = "Homicide - Criminal")
                      
    parms <- reactive({
        # Create input list for testing - commented out for production
        # input <- list(start = "2017-01-01", end = Sys.Date(), CrimeType = "Homicide - Criminal")

        CrimeType <- input$CrimeType
        urlstring <- paste0("http://phl.carto.com/api/v2/sql?format=csv&q=",
                        "SELECT dispatch_date, point_x, point_y, text_general_code ",
                        "FROM incidents_part1_part2",
                        " where text_general_code like '")
        urlstring <- paste0(URLencode(urlstring),gsub(" ","_",CrimeType),"'")
        crimedata <- read.csv(url(urlstring),
                              stringsAsFactors = FALSE)
        crimedata$dispatch_date <- as.Date(crimedata$dispatch_date)
        crimedata$year <- year(crimedata$dispatch_date)
        crimedata$month <- month(crimedata$dispatch_date)
        crimedata$day <- day(crimedata$dispatch_date)
        crimedata$lat <- crimedata$point_y
        crimedata$lng <- crimedata$point_x

        list(crimedata = crimedata,
             crimecount = length(crimedata$dispatch_date),
             CrimeType = CrimeType,
             FirstDispatch = min(crimedata$dispatch_date),
             LastDispatch = max(crimedata$dispatch_date))
        
    })
    
  output$leafletMap <- renderLeaflet({
      startDate <- input$dateRange[1]
      endDate <- input$dateRange[2]
      
      if(parms()$crimecount > 0)
          parms()$crimedata %>% 
          filter(dispatch_date >= startDate, dispatch_date <= endDate) %>%
          leaflet() %>%
          addTiles() %>%
          addCircleMarkers(radius = 6,
                           color = "black",
                           fillOpacity = 1,
                           popup = ~as.character(dispatch_date),
                           lng = ~lng,
                           lat = ~lat,
                           clusterOptions = markerClusterOptions()
                           )

  })
  output$MapTitle <- renderUI({
      headingString <- paste0("<b>Cluster Map for Crime Type '",parms()$CrimeType,"' for the period from ",
                              as.character(parms()$startDate)," to ",as.character(parms()$LastUpdate),"</b>")
      if(parms()$crimecount > 0) 
          heading <- HTML(paste0('<br><p style="text-align:center">',headingString,'</p>'))
      else 
          heading <- HTML('<br><br><p style="text-align:center"><b>Sorry - No Data Available For Selected Type and Time Period.</b></p>')
      heading
  })
  output$countPlot <- renderPlot({
      crimecounts <- parms()$crimedata %>% count(year) %>% arrange(year)
      
      plot(crimecounts$year, crimecounts$n, type = "b",
           main = paste0("Occurrences of '",parms()$CrimeType,"' by Year"),
           ylim = c(0,max(crimecounts$n)*1.2),
           xlim = c(min(crimecounts$year), max(crimecounts$year + 1)),
           xlab = "Year",
           ylab = "Number of Occurrences",
           col = "navyblue",
           bty = "n",
           lwd = 3)
      abline(lm(crimecounts$n ~ crimecounts$year) ,col = "maroon", lty = 3, lwd = 2)
        })
})
